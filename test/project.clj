(defproject ultrametric-avx2-integrated "0.1.0-SNAPSHOT"
  :description "Ultrametric AVX2 integrated project"
  :url "https://example.com"
  :license {:name "EPL-2.0"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [net.mikera/core.matrix "0.62.0"]
                 [org.clojure/core.async "1.5.648"]]
  :source-paths ["src"]
  ;; Add the incubator vector module if running on a JDK that requires it
  :jvm-opts ["--add-modules" "jdk.incubator.vector"]
  :profiles {:uberjar {:aot :all}})