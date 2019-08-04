{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Dominion
-}

module DominionTypes (Notification(..), State(..), Deck(..), Name,
  Move(..), Play(..), PlayType(..), readPlay) where

import Data.List
import DominionCards (Card(..), readCard)

type Name = String

type Deck = [Card]


data Notification = StateNotification  State
  | MoveNotification   Move
  | AttackNotification Attack
  | DefendNotification Defend

data Attack = Attack {
}

data Defend = Defend {
}

data State = State {
  players  :: [Name],
  supply   :: Deck,
  trash    :: Deck,
  actions  :: Int,
  buys     :: Int,
  coins    :: Int,
  deck     :: Deck,
  hand     :: Deck,
  plays    :: Deck,
  discards :: Deck
} deriving Eq

instance Show State where
  show myState = "(" ++ unwords ["move",
    "((" ++ unwords ("players"  :          players  myState  ) ++ ")",
    "("  ++ unwords ("supply"   :map show (supply   myState) ) ++ ")",
    "("  ++ unwords ("trash"    :map show (trash    myState) ) ++ ")",
    "("  ++ unwords ("actions"  :[   show (actions  myState)]) ++ ")",
    "("  ++ unwords ("buys"     :[   show (buys     myState)]) ++ ")",
    "("  ++ unwords ("coins"    :[   show (coins    myState)]) ++ ")",
    "("  ++ unwords ("deck"     :map show (deck     myState) ) ++ ")",
    "("  ++ unwords ("hand"     :map show (hand     myState) ) ++ ")",
    "("  ++ unwords ("plays"    :map show (plays    myState) ) ++ ")",
    "("  ++ unwords ("discards" :map show (discards myState) ) ++ ")"] ++ "))"


data Move = Move {
  name :: Name,
  play :: Play
} deriving Show

data Play = Play {
  playType :: PlayType,
  cards :: Deck
}

instance Show Play where
  show myPlay = "(" ++ unwords (show (playType myPlay) : map show (cards myPlay)) ++ ")"

data PlayType = ActMine
  | ActCellar | ActMarket | ActRemodel | ActSmithy | ActVillage| ActWoodcutter | ActWorkshop
  | ActMilitia | ActMoat
  | AddTreasure | BuyCard | Clean

readPlay :: [String] -> Play
-- protocol v1
readPlay ("act":"mine":strs) = Play ActMine     (map readCard strs)
readPlay ("add"       :strs) = Play AddTreasure (map readCard strs)
readPlay ("buy"       :strs) = Play BuyCard     (map readCard strs)
readPlay ("clean"     :strs) = Play Clean       (map readCard strs)
-- protocol v2
readPlay ("act":"cellar"    :strs) = Play ActCellar     (map readCard strs)
readPlay ("act":"market"    :strs) = Play ActMarket     (map readCard strs)
readPlay ("act":"remodel"   :strs) = Play ActRemodel    (map readCard strs)
readPlay ("act":"smithy"    :strs) = Play ActSmithy     (map readCard strs)
readPlay ("act":"village"   :strs) = Play ActVillage    (map readCard strs)
readPlay ("act":"woodcutter":strs) = Play ActWoodcutter (map readCard strs)
readPlay ("act":"workshop"  :strs) = Play ActWorkshop   (map readCard strs)
-- protocol v3
readPlay ("act":"militia":strs) = Play ActMilitia (map readCard strs)
readPlay ("act":"moat"   :strs) = Play ActMoat    (map readCard strs)


instance Show PlayType where
  -- protocol v1
  show ActMine     = "act mine"
  show AddTreasure = "add"
  show BuyCard     = "buy"
  show Clean       = "clean"
  -- protocol v2
  show ActCellar     = "act cellar"
  show ActMarket     = "act market"
  show ActRemodel    = "act remodel"
  show ActSmithy     = "act smithy"
  show ActVillage    = "act village"
  show ActWoodcutter = "act woodcutter"
  show ActWorkshop   = "act workshop"
  -- protocol v3
  show ActMilitia = "act militia"
  show ActMoat    = "act moat"
