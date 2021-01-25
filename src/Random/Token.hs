module Random.Token
  ( Token
  , tokenToAscii
  , tokenToText
  , asciiToToken
  , textToToken
  , generateToken
  , unsafeCoerceToken
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Bifunctor
import Data.ByteString as BS
import Data.ByteString.Base64.URL as B64
import Data.List as L
import Data.Text as T
import Data.Text.Encoding as T
import Data.Typeable
import Test.QuickCheck.Arbitrary
import Web.HttpApiData

import Crypto.Random as R
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

-- | 16 bytes lenght random value parametrized by label.
newtype Token (a :: k) = Token ByteString
  deriving (Ord, Eq, Show, NFData)

instance Arbitrary (Token a) where
  arbitrary = fmap (Token . BS.pack) $ sequence $ L.replicate 16 arbitrary

instance FromJSON (Token a) where
  parseJSON v = do
    t <- parseJSON v
    either fail return $ textToToken t

instance ToJSON (Token a) where
  toJSON = toJSON . tokenToText

instance forall a. (Typeable (Token a), Typeable a) => FromField (Token a) where
  fromField fld bs = do
    t <- fromField fld bs
    case textToToken t of
      Left e  -> returnError ConversionFailed fld e
      Right a -> return a

instance ToField (Token a) where
  toField = toField . tokenToText

generateToken :: (MonadRandom m) => m (Token a)
generateToken = Token <$> R.getRandomBytes 16

instance FromHttpApiData (Token a) where
  parseQueryParam = first T.pack . textToToken

instance ToHttpApiData (Token a) where
  toQueryParam = tokenToText

tokenToAscii :: Token a -> ByteString
tokenToAscii (Token bs) = B64.encode bs

tokenToText :: Token a -> Text
tokenToText = T.decodeUtf8 . tokenToAscii

asciiToToken :: ByteString -> Either String (Token a)
asciiToToken bs = Token <$> B64.decode bs

textToToken :: Text -> Either String (Token a)
textToToken = asciiToToken . T.encodeUtf8

unsafeCoerceToken :: Token a -> Token b
unsafeCoerceToken (Token a) = Token a
