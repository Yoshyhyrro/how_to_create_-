#!/usr/bin/env csi -script
;;; kinmedai-eye-sim.scm
;;; For CHICKEN Scheme 5.x — Uses only standard eggs
;;; eggs: gl, glut (Install: chicken-install gl glut)
;;; Usage: csi -script kinmedai-eye-sim.scm

;;;;
;;;; Dependencies (use ...) and Declarations
;;;;
(use chicken base)        ; Utilize procedures from chicken.base (e.g., foldl)
(use gl)                  ; OpenGL binding egg
(use glut)                ; GLUT egg

;;;;
;;;; Basic Output and Utilities
;;;;
;; Random Numbers: Simple LCG implemented locally to avoid external dependencies
(define *rng-state* (let ((s (inexact->exact (truncate (* (current-seconds) 1000))))) (if (= s 0) 1 s)))

(define (lcg-seed! s)
  (set! *rng-state* (exact->inexact (modulo s 2147483647)))
  (void))

(define (lcg-next)
  ;; 32-bit-ish LCG: X_{n+1} = (A * X_n + C) mod M
  (let* ((a 1103515245)
         (c 12345)
         (m 2147483647)
         (s (inexact->exact (truncate *rng-state*)))
         (ns (modulo (+ (* a s) c) m)))
    (set! *rng-state* ns)
    (/ (exact->inexact ns) (exact->inexact m)))) ; [0,1)

(define (rand) (lcg-next))
(define (rand-range lo hi) (+ lo (* (- hi lo) (rand))))
(define (rand-normal)
  ;; Box-Muller using two uniform LCG draws
  (let ((u1 (max 1e-12 (rand))) (u2 (rand)))
    (let ((r (sqrt (* -2.0 (log u1))))
          (t (* 2.0 3.141592653589793 u2)))
      (* r (cos t)))))

;;;;
;;;; Parameters and Growth/Optics Model (using thin lens)
;;;;
(define pi 3.141592653589793)
(define *age* 5.0)           ; Initial age (years)
(define *min-age* 0.5)
(define *max-age* 20.0)

(define (eye-diameter-from-age age)
  ;; von Bertalanffy-like saturated growth
  (let* ((L-inf 70.0)   ; mm
         (k 0.18)       ; annual rate
         (t0 0.0))
    (* L-inf (- 1.0 (exp (* -k (- age t0)))))))

(define (pupil-diameter-from-eye eye-d)
  ;; Ratio relative to eye diameter
  (* 0.45 eye-d))

(define (focal-length-from-eye eye-d)
  ;; Simple proportional model (scales with eye radius)
  (* 0.9 (/ eye-d 2.0)))

(define (image-distance f s)
  ;; Thin lens equation: 1/f = 1/s + 1/s'
  ;; s: object distance (positive), f: focal length (positive)
  (let ((den (- (/ 1.0 f) (/ 1.0 s))))
    (if (zero? den) 1e6 (/ 1.0 den))))

(define (calc-retina-y pupil-y object-distance effective-f)
  ;; Calculate retinal position using magnification m = - s' / s
  (let* ((s object-distance)
         (s-prime (image-distance effective-f s))
         (m (- (/ s-prime s))))
    (* m pupil-y)))

(define (get-lens-jitter age)
  ;; Age-dependent lens jitter (in mm)
  (let ((base 0.02) (slope 0.01))
    (+ base (* slope (max 0.0 (- age 5.0))))))

(define (distance-attenuate intensity distance color)
  ;; Simple distance attenuation (softened 1/r^2) and color absorption (e.g., red attenuation)
  (let* ((soft (+ 1e-3 (* 1e-4 distance)))
         (att (/ intensity (* soft soft)))
         (r (* (car color) att (exp (* -0.0008 distance))))
         (g (* (cadr color) att))
         (b (* (caddr color) att (exp (* -0.00025 distance)))))
    (list r g b)))

;;;;
;;;; OpenGL Helper Drawing Routines
;;;;
(define (draw-circle x y r segments)
  (glBegin GL_LINE_LOOP)
  (let loop ((i 0))
    (if (< i segments)
        (begin
          (let ((a (* (/ i segments) 2.0 pi)))
            (glVertex2f (+ x (* r (cos a))) (+ y (* r (sin a)))))
          (loop (+ i 1)))
        (void)))
  (glEnd))

(define (draw-line x1 y1 x2 y2)
  (glBegin GL_LINES)
  (glVertex2f x1 y1)
  (glVertex2f x2 y2)
  (glEnd))

;;;;
;;;; Sample Generation (Simple implementation to reduce variance, stratified sampling style)
;;;;
(define (stratified-apply strata per proc)
  ;; strata: number of columns, per: samples per column
  (let loop1 ((i 0))
    (when (< i strata)
      (let loop2 ((j 0))
        (when (< j per)
          (let* ((u (+ (/ i (exact->inexact strata)) (* (rand) (/ 1.0 strata))))
                 (v (rand)))
            (proc u v))
          (loop2 (+ j 1))))
      (loop1 (+ i 1)))))

;;;;
;;;; Core: Stochastic Ray Tracing (Uses thin lens, includes visualization)
;;;;
(define (stochastic-ray-trace pupil-y object-distance base-color sample-budget age eye-d pupil-d)
  ;; sample-budget: total samples (integer)
  (let* ((jitter (get-lens-jitter age))
         (f (focal-length-from-eye eye-d))
         (strata 10)
         (per (max 1 (quotient sample-budget strata)))
         (acc '()))
    (glEnable GL_BLEND)
    (glBlendFunc GL_SRC_ALPHA GL_ONE)

    (stratified-apply strata per
                      (lambda (u v)
                        (let* ((lens-shift (* (rand-normal) jitter))
                               (eff-f (+ f lens-shift))
                               (ry (calc-retina-y pupil-y object-distance eff-f))
                               (atten (distance-attenuate 1.0 object-distance base-color))
                               (r (car atten)) (g (cadr atten)) (b (caddr atten)))
                          ;; Visualization: Object -> Pupil, Pupil -> Retina (approximate lines)
                          (glColor4f r g b (/ 1.0 (* strata per)))
                          (glBegin GL_LINES)
                          (glVertex2f (- object-distance) pupil-y) ; Object position (left side)
                          (glVertex2f 0.0 pupil-y)                 ; Pupil center
                          (glVertex2f 0.0 pupil-y)                 ; From pupil to retina
                          (glVertex2f eff-f ry)
                          (glEnd)
                          (set! acc (cons (list ry r g b) acc))))))

    ;; Visualize simple retinal reflection
    (let ((reflectivity 0.25))
      (for-each (lambda (e)
                  (let ((ry (car e)) (rr (cadr e)) (rg (caddr e)) (rb (cadddr e)))
                    (glColor4f rr rg rb (* 0.12 reflectivity))
                    (draw-line eff-f ry 0.0 (* 0.6 ry))))
                acc))

    (glDisable GL_BLEND)))

;;;;
;;;; Draw Eye Structure
;;;;
(define (draw-eye-structure eye-d pupil-d)
  (glLineWidth 2.0)
  ;; Assume coordinates are in mm equivalent
  (glColor3f 0.8 0.85 0.95)
  (draw-circle 0.0 0.0 (/ eye-d 2.0) 80)
  ;; Lens approximation (circle)
  (glColor3f 1.0 0.85 0.25)
  (draw-circle 0.0 0.0 21.0 40)
  ;; Iris (outline)
  (glColor3f 0.05 0.05 0.05)
  (draw-circle 0.0 0.0 (/ pupil-d 2.0) 40)
  ;; Retinal arc (right semicircle)
  (glColor3f 1.0 0.3 0.4)
  (glBegin GL_LINE_STRIP)
  (let loop ((a (* -0.5 pi)))
    (when (<= a (* 0.5 pi))
      (glVertex2f (* (/ eye-d 2.0) (cos a)) (* (/ eye-d 2.0) (sin a)))
      (loop (+ a 0.08))))
  (glEnd))

;;;;
;;;; GLUT Callbacks
;;;;
(define (display)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glLoadIdentity)
  ;; Adjust scale and translation for better viewing
  (glScalef 0.6 0.6 1.0)
  (glTranslatef -20.0 0.0 0.0)

  (let* ((eye-d (eye-diameter-from-age *age*))
         (pupil-d (pupil-diameter-from-eye eye-d)))
    (draw-eye-structure eye-d pupil-d)
    ;; Distant blue object
    (stochastic-ray-trace 10.0 2000.0 (list 0.2 0.5 1.0) 30 *age* eye-d pupil-d)
    (stochastic-ray-trace -5.0 2000.0 (list 0.2 0.5 1.0) 30 *age* eye-d pupil-d)
    ;; Near red object
    (stochastic-ray-trace 8.0 500.0 (list 1.0 0.2 0.2) 80 *age* eye-d pupil-d)
    (stochastic-ray-trace -8.0 500.0 (list 1.0 0.2 0.2) 80 *age* eye-d pupil-d)

    ;; HUD Text (Simple)
    (glColor3f 1.0 1.0 1.0)
    (glRasterPos2f -120.0 100.0)
    (glutBitmapString GLUT_BITMAP_HELVETICA_18 (string-append "Age: " (number->string *age*)))
    (glRasterPos2f -120.0 88.0)
    (glutBitmapString GLUT_BITMAP_HELVETICA_12
                      (if (> *age* 10.0) "Status: PRESBYOPIA (High Entropy)" "Status: Young & Sharp"))))

(define (keyboard key x y)
  (cond ((= key 27) (begin (format #t "Exiting~%") (exit 0)))
        (else (void))))

(define (special-keys key x y)
  (cond ((= key GLUT_KEY_UP)   (set! *age* (min *max-age* (+ *age* 0.5))))
        ((= key GLUT_KEY_DOWN) (set! *age* (max *min-age* (- *age* 0.5)))))
  (glutPostRedisplay))

;;;;
;;;; Main
;;;;
(define (main)
  (lcg-seed! (inexact->exact (truncate (* (current-seconds) 1000.0))))
  (glutInit (u32vector 0 0 0 0))
  (glutInitDisplayMode (bitwise-ior GLUT_DOUBLE GLUT_RGB))
  (glutInitWindowSize 1000 800)
  (glutCreateWindow "Kinmedai — CHICKEN Scheme Eye Optics Sim")
  (glClearColor 0.05 0.05 0.08 1.0)
  (glEnable GL_LINE_SMOOTH)
  (glutDisplayFunc display)
  (glutKeyboardFunc keyboard)
  (glutSpecialFunc special-keys)
  (format #t "Use UP/DOWN to change age. Close window to exit.~%")
  (glutMainLoop))

(main)
