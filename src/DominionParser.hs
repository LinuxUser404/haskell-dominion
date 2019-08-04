{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Dominion Client
-}

module DominionParser (parseNotification) where

import Text.ParserCombinators.Parsec
import DominionTypes (Notification(..), State(..), Move(..), Play(..), Name, Deck, readPlay)
import DominionCards (readCard)

-- Notification parser implementation
myWords :: GenParser Char st [String]
myWords = do
  str <- many (noneOf "()")
  return $ words str


parseNotification :: GenParser Char st DominionTypes.Notification
parseNotification = spaces >>
  ((StateNotification <$> parseState) <|> (MoveNotification <$> parseMove))


parseState :: GenParser Char st DominionTypes.State
parseState = do
  char '(';
  spaces;
  string "move"
  spaces
  char '('
  spaces
  players  <- parsePlayers
  spaces
  supply   <- parseDeck   "supply"
  spaces
  trash    <- parseDeck   "trash"
  spaces
  actions  <- parsePoints "actions"
  spaces
  buys     <- parsePoints "buys"
  spaces
  coins    <- parsePoints "coins"
  spaces
  deck     <- parseDeck   "deck"
  spaces
  hand     <- parseDeck   "hand"
  spaces
  plays    <- parseDeck   "plays"
  spaces
  discards <- parseDeck   "discards"
  spaces
  char ')'
  spaces;
  char ')';
  return $ DominionTypes.State players supply trash actions buys coins deck hand plays discards


parseDeck :: String -> GenParser Char st Deck
parseDeck deckName = do
  char '('
  spaces
  string deckName
  temp <- myWords
  spaces
  char ')'
  return $ map readCard temp

parsePoints :: String -> GenParser Char st Int
parsePoints pointsName = do
  char '('
  spaces
  string pointsName
  spaces
  temp <- many (noneOf "( )\n")
  spaces
  char ')'
  return $ read temp


parsePlayers :: GenParser Char st [Name]
parsePlayers = do
  char '('
  spaces
  string "players"
  temp <- myWords
  char ')'
  return temp


parseMove :: GenParser Char st DominionTypes.Move
parseMove = do
  char '('
  spaces
  string "moved"
  spaces
  name <- many (noneOf "( )\n")
  spaces
  play <- parsePlay
  spaces
  char ')'
  return $ DominionTypes.Move name play

parsePlay :: GenParser Char st DominionTypes.Play
parsePlay = do
  char '('
  temp <- myWords
  char ')'
  return $ readPlay temp
