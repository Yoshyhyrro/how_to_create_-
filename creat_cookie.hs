{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty
import Network.HTTP.Types (status200)
import Web.Cookie
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 as L8 (pack)
import Data.ByteString.Char8 as BS (unpack)
import Numeric.LinearAlgebra
import Data.Complex

-- ■ ジョルダン分解用データ型
data JordanBlock = JordanBlock
  { eigenvalue :: Complex Double  -- 複素固有値も扱える
  , size       :: Int
  , geometric_mult :: Int         -- 幾何的重複度
  } deriving (Generic, Show)

instance ToJSON JordanBlock
instance FromJSON JordanBlock

data JordanDecomp = JordanDecomp
  { blocks          :: [JordanBlock]
  , transform_matrix :: [[Complex Double]]  -- 変換行列 P
  , original_size   :: Int                  -- 元の行列のサイズ
  , compression_ratio :: Double             -- 圧縮率
  } deriving (Generic, Show)

instance ToJSON JordanDecomp
instance FromJSON JordanDecomp

-- ■ 実際のジョルダン分解を行う関数（簡易版）
computeJordan :: Matrix Double -> JordanDecomp
computeJordan m = 
  let n = rows m
      -- 固有値計算（実装を簡略化）
      eigenvals = eigenvaluesSH m  -- 対称行列用だが例として使用
      -- ここで本来はより複雑なジョルダン標準形の計算を行う
      
      -- サンプルとして固有値から JordanBlock を構築
      blocks_list = map (\ev -> JordanBlock (ev :+ 0) 1 1) (toList eigenvals)
      
      -- 変換行列（ここでは単位行列で代用）
      p_matrix = toLists $ ident n
      p_complex = map (map (:+ 0)) p_matrix
      
      -- 圧縮率計算（元のサイズ vs ジョルダンブロック表現のサイズ）
      original_bytes = n * n * 8  -- Double のバイト数
      compressed_bytes = length blocks_list * 3 * 8  -- 各ブロック3つの値
      ratio = fromIntegral compressed_bytes / fromIntegral original_bytes
      
  in JordanDecomp blocks_list p_complex n ratio

-- ■ ジョルダン分解からの復元
reconstruct :: JordanDecomp -> Matrix (Complex Double)
reconstruct (JordanDecomp blocks p_list n _) =
  let -- P行列の構築
      p_matrix = fromLists p_list
      
      -- ジョルダンブロックからJ行列を構築（簡易版）
      j_diag = concatMap (\(JordanBlock ev sz _) -> replicate sz ev) blocks
      j_matrix = diag (fromList j_diag)
      
      -- P^(-1) * J * P の計算（本来の復元）
  in j_matrix  -- 簡略化して J のみ返す

-- ■ Base64エンコーディング用ヘルパー（簡易版）
encodeForCookie :: JordanDecomp -> String
encodeForCookie jd = BS.unpack . toStrict . encode $ jd

decodeFromCookie :: String -> Maybe JordanDecomp
decodeFromCookie s = decode . L8.pack $ s

-- ■ サーバー本体
main :: IO ()
main = scotty 3000 $ do
  
  -- ジョルダン分解を計算してクッキーに保存
  post "/compress" $ do
    -- リクエストから行列データを受け取る（実装は簡略化）
    let mat = (3 >< 3) [ 4, 1, 0
                       , 0, 4, 1  
                       , 0, 0, 4 ] :: Matrix Double  -- ジョルダンブロックを持つ行列
    
    let jordan_decomp = computeJordan mat
        cookie_value = encodeForCookie jordan_decomp
        cookie = defaultSetCookie
          { setCookieName   = "jordan_decomp"
          , setCookieValue  = BS.pack cookie_value
          , setCookiePath   = Just "/"
          , setCookieMaxAge = Just 3600
          , setCookieSecure = True
          , setCookieHttpOnly = True
          }
    
    setHeader "Set-Cookie" (BS.unpack . toStrict . toLazyByteString $ renderSetCookie cookie)
    json jordan_decomp
  
  -- クッキーからジョルダン分解を復元
  get "/decompress" $ do
    cookies <- getCookies
    case lookup "jordan_decomp" cookies of
      Nothing -> do
        status status404
        text "No Jordan decomposition found in cookies"
      Just cookie_val -> 
        case decodeFromCookie (BS.unpack cookie_val) of
          Nothing -> do
            status status400  
            text "Invalid Jordan decomposition data"
          Just jordan_decomp -> do
            let reconstructed = reconstruct jordan_decomp
            json jordan_decomp
  
  -- 統計情報エンドポイント
  get "/stats" $ do
    cookies <- getCookies
    case lookup "jordan_decomp" cookies of
      Nothing -> json $ object ["error" .= ("No data" :: String)]
      Just cookie_val ->
        case decodeFromCookie (BS.unpack cookie_val) of
          Nothing -> json $ object ["error" .= ("Invalid data" :: String)]
          Just (JordanDecomp blocks _ n ratio) -> 
            json $ object [ "num_blocks" .= length blocks
                          , "original_size" .= n
                          , "compression_ratio" .= ratio
                          , "eigenvalues" .= map eigenvalue blocks
                          ]

-- ■ ヘルパー関数
getCookies :: ActionM [(BS.ByteString, BS.ByteString)]
getCookies = do
  cookie_header <- header "Cookie"
  case cookie_header of
    Nothing -> return []
    Just cookies -> return $ parseCookies $ BS.pack $ show cookies
