{-# LANGUAGE OverloadedStrings #-}
module System.FS.RDFS.DBUS where

import DBus.Bus
import DBus.Connection
import DBus.Message
import DBus.Types
import qualified Data.Set as Set
import Data.List (sort)

import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.XPath.XPathEval
import Text.XML.HXT.DOM.TypeDefs
import Data.Tree.NTree.TypeDefs


listServices :: IO [String]
listServices = do
  (bus, name) <- getSessionBus
  putStrLn $ "Connected as: " ++ show name

  -- Request a list of connected clients from the bus
  Right serial <- send bus return MethodCall
               { methodCallPath = "/org/freedesktop/DBus"
	       , methodCallMember = "ListNames"
	       , methodCallInterface = Just "org.freedesktop.DBus"
	       , methodCallDestination = Just "org.freedesktop.DBus"
	       , methodCallFlags = Set.empty
	       , methodCallBody = []
	       }

  -- Wait for the reply
  reply <- waitForReply bus serial

  -- Pull out the body, and convert it to [String]
  let Just names = fromArray =<< fromVariant (head (messageBody reply))
  return names

listDir :: String -> IO [String]
listDir path = do
  (bus, name) <- getSessionBus
  putStrLn $ "Connected as: " ++ show name

  -- Request a list of connected clients from the bus
  Right serial <- send bus return MethodCall
                 { methodCallPath = "/"
	         , methodCallMember = "Introspect"
	         , methodCallInterface = Just "org.freedesktop.DBus.Introspectable"
	         , methodCallDestination = Just "org.kde.amarok"
	         , methodCallFlags = Set.empty
	         , methodCallBody = []
	         }

  -- Wait for the reply
  reply <- waitForReply bus serial

  -- print $ reply

  let Just n = fromVariant (head (methodReturnBody reply))
  let f = unlines (drop 2 (lines n))
  let xml = head (xread f)

  let interfaces = getXPath "node/interface" xml
  --  print $ interfaces

  let nodes = getXPath "node/node/attribute::name/text()" xml
  let methods = [let NTree (XText met) _ = n in met | n <- nodes]
  --  print $ methods
  return methods

waitForReply :: Connection -> Serial -> IO MethodReturn
waitForReply bus serial = wait where
    wait = do
      received <- receive bus
      case received of
	Right (ReceivedMethodReturn _ _ ret) ->
	    if methodReturnSerial ret == serial
	    then return ret
	    else wait
	Right _ -> wait
	Left err -> error $ show err

