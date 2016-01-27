-----------------------------------------------------------------------------
-- |
-- Module      :  Emails
-- Copyright   :  (c) Domagoj Begusic
--
-----------------------------------------------------------------------------

module Emails (compileTemplate,
               readConfig,
               sendMail
              ) where

-- * Types and utilities

import Data.Text (Text,pack,unpack)
import Data.Text.Lazy (fromStrict)
import qualified Network.Mail.SMTP as NMS
import Data.Map (Map)
import qualified Data.Map as Map
import TemplateParser

-- | An alias for the template contents
type Template = Text

-- | Path to configuration file
confPath :: String
confPath = "email.config"

-- | Some configuration object mirroring a file.
data Configuration = Configuration {
  host :: String,
  port :: Integer,
  sender :: String,
  username :: String,
  password :: String,
  cmap :: Map String String
 } deriving Show

-- | Parses data Configuration from String
parseConf :: String -> Configuration
parseConf xs = Configuration hst (read prt) sndr usrn pswd cmp
 where (hst:prt:sndr:usrn:pswd:_) = map (last . words) $ lines xs
       cmp =  Map.fromList (read (getline 5) :: [(String,String)])
       getline :: Int -> String
       getline n = last $ words $ head $ drop n $ lines xs

-- | Converts string address to Network.Mail.SMTP.Address
atoA :: String -> NMS.Address
atoA x = NMS.Address Nothing (pack x)

-- * Implementation

-- | Parses email template using parsing tools from TemplateParser.
compileTemplate :: Template -> Map String String -> Text
compileTemplate t m = pack $ compileIfs m $ compileSubs (unpack t) m

-- | Reads the e-mail configuration object from a file, using some
-- | default path to config file.
readConfig :: IO Configuration
readConfig = do
 doc <- readFile confPath
 return $ parseConf doc

-- | Sends an e-mail with given text to list of e-mail addresses
-- | using given configuration. Throws appropriate error upon failure.
sendMail :: Configuration -> Text -> [String] -> IO ()
sendMail conf@(Configuration ho po se us pa cm) txt as = do
 let myAdrs = atoA $ se
     toAdrs = map (atoA) as
     template = compileTemplate txt cm
     mail = NMS.simpleMail myAdrs toAdrs [] [] (pack "TEST") [NMS.plainTextPart (Data.Text.Lazy.fromStrict $ template)]
 NMS.sendMailWithLogin' ho (fromInteger $ po) us pa mail