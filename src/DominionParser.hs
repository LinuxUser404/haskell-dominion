{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Dominion Client
-}

module DominionParser (parseNotification) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity (Identity)
import DominionTypes (Notification(..), State(..), Move(..), Play(..), Name, Deck, readPlay)
import DominionCards (readCard)

-- Notification parser implementation
myWords :: GenParser Char st [String]
myWords = words <$> many (noneOf "()")

parseNotification :: GenParser Char st DominionTypes.Notification
parseNotification = spaces >>
  ((StateNotification <$> parseState) <|> (MoveNotification <$> parseMove))

parseState :: GenParser Char st DominionTypes.State
parseState = braces $ string "move" *> spaces *> do
  char '(' *> spaces
  players'  <- parsePlayers
  supply'   <- spaces *> parseDeck   "supply"
  trash'    <- spaces *> parseDeck   "trash"
  actions'  <- spaces *> parsePoints "actions"
  buys'     <- spaces *> parsePoints "buys"
  coins'    <- spaces *> parsePoints "coins"
  deck'     <- spaces *> parseDeck   "deck"
  hand'     <- spaces *> parseDeck   "hand"
  plays'    <- spaces *> parseDeck   "plays"
  discards' <- spaces *> parseDeck   "discards"
  spaces <* char ')'
  return $ DominionTypes.State players' supply' trash' actions' buys' coins' deck' hand' plays' discards'

parseDeck :: String -> GenParser Char st Deck
parseDeck deckName = braces $ map readCard <$> (string deckName *> myWords)

parsePoints :: String -> GenParser Char st Int
parsePoints pointsName = braces $ read <$> (string pointsName *> spaces *> many (noneOf "( )\n"))

parsePlayers :: GenParser Char st [Name]
parsePlayers = braces $ string "players" *> myWords

parseMove :: GenParser Char st DominionTypes.Move
parseMove = braces $ do
  string "moved" *> spaces
  name' <- many (noneOf "( )\n")
  DominionTypes.Move name' <$> (spaces *> parsePlay)

parsePlay :: GenParser Char st DominionTypes.Play
parsePlay = braces $ readPlay <$> myWords

braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = try . between (char '(' *> spaces) (spaces <* char ')')
