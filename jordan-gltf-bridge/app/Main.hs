{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GodotBridge
import LegendreGLTF
import Data.Aeson
import qualified Data.ByteString.Lazy as L8
import Control.Monad (void)

-- Example glTF data
exampleGLTF :: Value
exampleGLTF = object
  [ "vertices" .= 
    [ [1.0 :: Double, 2.0, 3.0]
    , [4.0, 5.0, 6.0]
    , [7.0, 8.0, 9.0]
    ]
  ]

main :: IO ()
main = do
  putStrLn "Jordan-glTF Bridge Example"
  putStrLn "========================="
  
  -- Test prime for Legendre symbols
  let testPrime = 17 :: Integer
  
  -- Convert glTF to Godot message
  putStrLn $ "Using prime: " ++ show testPrime
  message <- gltfToGodot exampleGLTF testPrime
  
  -- Display the message
  putStrLn "Generated Legendre Message:"
  putStrLn $ "Message type: " ++ message_type message
  putStrLn $ "Number of vertices: " ++ show (length $ legendre_vertices message)
  
  -- Show Jordan decomposition info
  case jordan_data message of
    Just jd -> do
      putStrLn $ "Jordan blocks: " ++ show (length $ blocks jd)
      putStrLn $ "Original matrix size: " ++ show (original_size jd)
      putStrLn $ "Compression ratio: " ++ show (compression_ratio jd)
    Nothing -> putStrLn "No Jordan decomposition data"
  
  -- Test the Godot receiver
  putStrLn "\nTesting Godot receiver..."
  godotReceive message
  
  putStrLn "\nExample completed successfully!"

-- Additional test functions
testLegendreEncoding :: IO ()
testLegendreEncoding = do
  let vertex = (1.5, 2.7, 3.9) :: (Double, Double, Double)
  let prime = 13 :: Integer
  
  putStrLn $ "Original vertex: " ++ show vertex
  
  let encoded = encodeVertex vertex prime
  putStrLn $ "Encoded: " ++ show encoded
  
  case decodeToGodot encoded of
    Just decoded -> putStrLn $ "Decoded: " ++ show decoded
    Nothing -> putStrLn "Failed to decode"

testJordanDecomposition :: IO ()
testJordanDecomposition = do
  let vertices = [Vector3 1.0 0.0 0.0, Vector3 0.0 1.0 0.0, Vector3 0.0 0.0 1.0]
  
  putStrLn $ "Test vertices: " ++ show vertices
  
  jordan <- vertexMatrixToJordan vertices
  putStrLn $ "Jordan decomposition: " ++ show jordan
  
  let meshDecomp = jordanToMesh jordan
  putStrLn $ "Mesh decomposition hulls: " ++ show (length $ hulls meshDecomp)