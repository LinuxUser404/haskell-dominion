{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Dominion Client
-}


import System.IO
import Text.ParserCombinators.Parsec
import Data.List

--import DominionNotifications -- for testing
import DominionTypes (Notification)
import DominionPlayers (dumbPlayer, okayPlayer_v1, playerResponse)

import DominionParser (parseNotification)

main = notificationHandler

myPlayer = DominionPlayers.okayPlayer_v1

notificationHandler :: IO ()
notificationHandler = do
	closure <- readClosure 0 0            -- reads a closure from stdin
	case stringToNotification closure of  -- parses a closure into either error or notification(one of the Dominion types)
		Left e -> return ()           -- parse error
		Right notification -> do
			-- client response to the notification
			case playerResponse myPlayer notification of
				Nothing -> return()
				Just response -> putStr response
	-- make sure everything is written to stdout
	hFlush stdout                         
	notificationHandler

readClosure :: Int -> Int -> IO String
readClosure bra ket = do
	c <- getChar
	case c of
	  '(' -> do 
		cs <- (readClosure (bra + 1) ket)
		return (c:cs)
	  ')'
	    | (bra == ket + 1) -> do
		let cs = []
		return (c:cs)
	    | otherwise -> do
		cs <- (readClosure bra (ket + 1))
		return (c:cs)
	  _   -> do
		cs <- (readClosure bra ket)
		return (c:cs)
		


	
stringToNotification :: String -> Either ParseError DominionTypes.Notification
stringToNotification input = parse DominionParser.parseNotification "(Unknown Dominion Notification)" input

