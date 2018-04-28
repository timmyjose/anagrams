{-# LANGUAGE DeriveGeneric #-}

module ParserConf (
    getconf
  , getfplxcn
  , getfpangms
  , getextnfile
  , getfpdir
)
  where

import           GHC.Generics
import           Data.Aeson
import qualified Data.Yaml as DY (decode)
import qualified Data.ByteString.Char8 as DBC8

getconf :: DBC8.ByteString -> Maybe Conf
getconf b = DY.decode b

getfplxcn :: Maybe Conf -> String
getfplxcn Nothing = ""
getfplxcn (Just (Conf l a e d)) = l

getfpangms :: Maybe Conf -> String
getfpangms Nothing = ""
getfpangms (Just (Conf l a e d)) = a

getextnfile :: Maybe Conf -> String
getextnfile Nothing = ""
getextnfile (Just (Conf l a e d)) = e

getfpdir :: Maybe Conf -> String
getfpdir Nothing = ""
getfpdir (Just (Conf l a e d)) = d

data Conf = Conf {
    fplexicon  :: String
  , fpanagrams :: String
  , extnfile   :: String
  , directory  :: String
}
  deriving (
    Generic  -- Lets the compiler provide a default generic implementation of FromJSON.
  )

instance FromJSON Conf where
--  parseJSON (Object v) = Conf <$>
--                             v .: "fplexicon"
--                         <*> v .: "fpanagrams"
--                         <*> v .: "extnfile"
--                         <*> v .: "directory"
--  parseJSON invalid    = typeMismatch "Conf" invalid

instance Show Conf where
  show (Conf fpl fpa fe d) = fpl ++ ", " ++ fpa ++ ", " ++ fe ++ ", " ++ d
