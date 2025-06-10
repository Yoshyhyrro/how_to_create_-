{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module LegendreGLTF (
  LegendreEncodable(..),
  LegendreVertex(..),
  LegendreMessage(..),
  JordanBlock(..),
  JordanDecomp(..),
  Vector3(..),
  encodeVertex,
  decodeToGodot,
  legendreSymbol,
  reconstructCoords,
  extractVertices,
  quadraticFormFromPrime,
  computeJordan,
  reconstruct,
  vertexMatrixToJordan
) where 

import Data.Aeson (ToJSON, FromJSON, Value(..), Object)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Numeric.LinearAlgebra
import Data.Complex
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Scientific (fromFloatDigits, toRealFloat)

-- Vector3 type definition
data Vector3 = Vector3 
  { x :: Double
  , y :: Double  
  , z :: Double
  } deriving (Generic, Show, Eq)

instance ToJSON Vector3
instance FromJSON Vector3

-- Legendre symbol encodable class
class LegendreEncodable a where
  toLegendre :: a -> Integer -> Int
  fromLegendre :: [Int] -> Integer -> Maybe a

-- Legendre vertex encoding
data LegendreVertex = LegendreVertex
  { prime :: Integer
  , symbols :: [Int]
  , residue_class :: Integer
  } deriving (Generic, Show)

instance ToJSON LegendreVertex
instance FromJSON LegendreVertex

-- Jordan decomposition types
data JordanBlock = JordanBlock
  { eigenvalue :: Complex Double
  , size :: Int
  , geometric_mult :: Int
  } deriving (Generic, Show)

instance ToJSON JordanBlock
instance FromJSON JordanBlock

data JordanDecomp = JordanDecomp
  { blocks :: [JordanBlock]
  , transform_matrix :: [[Complex Double]]
  , original_size :: Int
  , compression_ratio :: Double
  } deriving (Generic, Show)

instance ToJSON JordanDecomp
instance FromJSON JordanDecomp

-- Communication protocol
data LegendreMessage = LegendreMessage
  { message_type :: String
  , jordan_data :: Maybe JordanDecomp
  , legendre_vertices :: [LegendreVertex]
  , quadratic_form :: [[Integer]]
  } deriving (Generic, Show)

instance ToJSON LegendreMessage
instance FromJSON LegendreMessage

-- Legendre symbol computation
legendreSymbol :: Integer -> Integer -> Int
legendreSymbol a p
  | a `mod` p == 0 = 0
  | otherwise = if (a ^ ((p - 1) `div` 2)) `mod` p == 1 then 1 else -1

-- Encode vertex using Legendre symbols
encodeVertex :: (Double, Double, Double) -> Integer -> LegendreVertex
encodeVertex (x, y, z) p = 
  let coords = [floor (x * 1000), floor (y * 1000), floor (z * 1000)]
      symbols = map (\n -> legendreSymbol n p) coords
      residue = sum coords `mod` p
  in LegendreVertex p symbols residue

-- Decode to Godot coordinates
decodeToGodot :: LegendreVertex -> Maybe (Double, Double, Double)
decodeToGodot (LegendreVertex p syms res) = 
  reconstructCoords syms p res

-- Reconstruct coordinates from Legendre symbols
reconstructCoords :: [Int] -> Integer -> Integer -> Maybe (Double, Double, Double)
reconstructCoords syms p res = 
  let coords = map (\s -> if s == 0 then 0 else fromIntegral (s * res `mod` p)) syms
  in if all (\c -> c >= -1000 && c <= 1000) coords && length coords >= 3
     then Just (head coords / 1000, coords !! 1 / 1000, coords !! 2 / 1000)
     else Nothing

-- Generate quadratic form matrix from prime
quadraticFormFromPrime :: Integer -> [[Integer]]
quadraticFormFromPrime p = 
  let n = min 10 (fromIntegral p)  -- Limit size for practical purposes
  in [[(fromIntegral i * fromIntegral j) `mod` p | j <- [0..n-1]] | i <- [0..n-1]]

-- Extract vertices from glTF Value
extractVertices :: Value -> IO [(Double, Double, Double)]
extractVertices (Object obj) = do
  case HM.lookup "vertices" obj of
    Just (Array arr) -> return $ V.toList $ V.mapMaybe parseVertex arr
    _ -> return []
  where
    parseVertex (Array v) 
      | V.length v >= 3 = do
          x <- parseNumber (v V.! 0)
          y <- parseNumber (v V.! 1) 
          z <- parseNumber (v V.! 2)
          return (x, y, z)
      | otherwise = Nothing
    parseVertex _ = Nothing
    
    parseNumber (Number n) = Just $ toRealFloat n
    parseNumber _ = Nothing

extractVertices _ = return []

-- Convert coordinates to Vector3 list
extractVerticesAsVector3 :: Value -> IO [Vector3]
extractVerticesAsVector3 val = do
  coords <- extractVertices val
  return $ map (\(x, y, z) -> Vector3 x y z) coords

-- Jordan decomposition computation
computeJordan :: Matrix Double -> JordanDecomp
computeJordan m = 
  let n = rows m
      -- Simplified eigenvalue computation
      eigenvals = case n of
        0 -> []
        _ -> eigenvaluesSH m
      
      blocks_list = map (\ev -> JordanBlock (ev :+ 0) 1 1) (toList eigenvals)
      p_matrix = toLists $ ident n
      p_complex = map (map (:+ 0)) p_matrix
      
      original_bytes = n * n * 8
      compressed_bytes = length blocks_list * 3 * 8
      ratio = if original_bytes > 0 
              then fromIntegral compressed_bytes / fromIntegral original_bytes
              else 0.0
  in JordanDecomp blocks_list p_complex n ratio

-- Reconstruct matrix from Jordan decomposition
reconstruct :: JordanDecomp -> Matrix (Complex Double)
reconstruct (JordanDecomp blocks p_list n _) =
  let j_diag = concatMap (\(JordanBlock ev sz _) -> replicate sz ev) blocks
      j_matrix = if null j_diag then fromLists [[]] else diag (fromList j_diag)
  in j_matrix

-- Convert vertex matrix to Jordan decomposition
vertexMatrixToJordan :: [Vector3] -> IO JordanDecomp
vertexMatrixToJordan [] = return $ JordanDecomp [] [] 0 0.0
vertexMatrixToJordan vertices = do
  let mat = fromLists [[x v, y v, z v] | v <- vertices] :: Matrix Double
  let jordan_decomp = computeJordan mat
  return jordan_decomp

-- Instance implementations
instance LegendreEncodable Vector3 where
  toLegendre (Vector3 x y z) p = 
    let coord_sum = floor (x * 1000) + floor (y * 1000) + floor (z * 1000)
    in legendreSymbol coord_sum p
    
  fromLegendre syms p = 
    case reconstructCoords syms p 0 of
      Just (x, y, z) -> Just (Vector3 x y z)
      Nothing -> Nothing