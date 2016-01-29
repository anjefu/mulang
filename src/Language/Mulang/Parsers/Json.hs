module Language.Mulang.Parsers.Json (parseJson) where

import Language.Mulang

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as LBS (pack)

parseJson :: String -> Maybe Expression
parseJson  = decode . LBS.pack

instance FromJSON Equation
instance ToJSON Equation

instance FromJSON EquationBody
instance ToJSON EquationBody

instance FromJSON GuardedBody
instance ToJSON GuardedBody

instance FromJSON Expression
instance ToJSON Expression

instance FromJSON Pattern
instance ToJSON Pattern

instance FromJSON ComprehensionStatement
instance ToJSON ComprehensionStatement

instance FromJSON Alternative
instance ToJSON Alternative

instance FromJSON GuardedAlternatives
instance ToJSON GuardedAlternatives

instance FromJSON GuardedAlternative
instance ToJSON GuardedAlternative
