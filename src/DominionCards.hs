{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Dominion Client
-}

module DominionCards (Card(..), readCard, treasureValue, victoryValue, cardCost) where

data Card
  -- protocol v1
  -- base cards
  = Copper | Silver | Gold
  | Estate | Duchy | Province
  | Curse
  -- kingdom cards
  | Mine
  -- protocol v2
  | Cellar | Village | Woodcutter | Workshop | Remodel | Smithy | Market
  -- protocol v3
  | Moat | Militia
  -- extra
  | Chapel | Chancellor | Feast | Bureaucrat| Moneylender | Thief | Council | Festival | Laboratory | Adventurer
  | UnknownCard -- compatibility card
  deriving Eq

instance Show Card where
  -- protocol v1
  show Copper   = "copper"
  show Silver   = "silver"
  show Gold     = "gold"
  show Estate   = "estate"
  show Duchy    = "duchy"
  show Province = "province"
  show Curse    = "curse"
  show Mine     = "mine"
  -- protocol v2
  show Cellar     = "cellar"
  show Village    = "village"
  show Woodcutter = "woodcutter"
  show Workshop   = "workshop"
  show Remodel    = "remodel"
  show Smithy     = "smithy"
  show Market     = "market"
  -- protocol v3
  show Moat    = "moat"
  show Militia = "militia"
  -- extra
  show Chapel      = "Chapel"
  show Chancellor  = "Chancellor"
  show Feast       = "Feast"
  show Bureaucrat  = "Bureaucrat"
  show Moneylender = "Moneylender"
  show Thief       = "Thief"
  show Council     = "Council"
  show Festival    = "Festival"
  show Laboratory  = "Laboratory"
  show Adventurer  = "Adventurer"
  show UnknownCard = "UnknownCard"


-- TODO: overload read function instead
readCard :: String -> Card
-- protocol v1
readCard "copper"   = Copper
readCard "silver"   = Silver
readCard "gold"     = Gold
readCard "estate"   = Estate
readCard "duchy"    = Duchy
readCard "province" = Province
readCard "curse"    = Curse
readCard "mine"     = Mine
-- protocol v2
readCard "cellar"     = Cellar
readCard "village"    = Village
readCard "woodcutter" = Woodcutter
readCard "workshop"   = Workshop
readCard "remodel"    = Remodel
readCard "smithy"     = Smithy
readCard "market"     = Market
-- protocol v3
readCard "moat"    = Moat
readCard "militia" = Militia
-- extra
readCard "Chapel"      = Chapel
readCard "Chancellor"  = Chancellor
readCard "Feast"       = Feast
readCard "Bureaucrat"  = Bureaucrat
readCard "Moneylender" = Moneylender
readCard "Thief"       = Thief
readCard "Council"     = Council
readCard "Festival"    = Festival
readCard "Laboratory"  = Laboratory
readCard "Adventurer"  = Adventurer
readCard _ = UnknownCard

treasureValue :: Card -> Int
treasureValue Copper   = 1
treasureValue Silver   = 2
treasureValue Gold     = 3
treasureValue _        = 0

victoryValue :: Card -> Int
victoryValue Curse    = -1
victoryValue Estate   =  1
victoryValue Duchy    =  3
victoryValue Province =  6
victoryValue _        =  0

cardCost :: Card -> Int
-- Treasure cards
cardCost Copper    = 0
cardCost Silver    = 3
cardCost Gold      = 6
-- Victory cards
cardCost Curse     = 0
cardCost Estate    = 2
cardCost Duchy     = 5
cardCost Province  = 8
-- Action cards
-- protocol v1
cardCost Mine        = 5 -- Trash a Treasure card from your hand. Gain a Trasure card costing up to 3 more; put it in into your hand
-- protocol v2
cardCost Cellar     = 2 -- +1A, "Discards any number of cards. +1 Card per card discarded"
cardCost Village    = 3 -- +1 Card, +2 Actions
cardCost Woodcutter = 3 -- +1 Buy, +2 Coins
cardCost Workshop   = 3 -- Gain a card with cost up to 4
cardCost Remodel    = 4 -- Gain a card with cost up to 2 more than the trashed card
cardCost Smithy     = 4 -- +3 Cards,
cardCost Market     = 5 -- +1 Card, +1 Action, +1 Buy, +1 Coin
-- extra
cardCost Chapel      = 2
cardCost Bureaucrat  = 2
cardCost Feast       = 2
cardCost Moneylender = 2
cardCost Chancellor  = 2
cardCost Thief       = 4
cardCost Militia     = 4  -- +2 Coin "Each other player discards down to 3 cards in his hand"
cardCost Council     = 5
cardCost Festival    = 5
cardCost Laboratory  = 5
cardCost Adventurer  = 5
cardCost _           = 0
