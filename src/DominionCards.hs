{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Dominion Client
-}



module DominionCards (Card(..), readCard, treasureValue, victoryValue, cardCost) where

data Card = 
	-- protocol v1
	Copper | Silver | Gold
	| Estate | Duchy | Province | Curse
	| Mine
	-- protocol v2
	| Cellar | Village | Woodcutter | Workshop | Remodel | Smithy | Market
	-- protocol v3
	| Moat | Militia
	-- extra 
	| Chapel | Chancellor | Feast | Bureaucrat| Moneylender | Thief |Council | Festival |Laboratory | Adventurer
	| UnknownCard
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
treasureValue card
	| card == Copper   = 1
	| card == Silver   = 2
	| card == Gold     = 3
	| otherwise        = 0

victoryValue :: Card -> Int
victoryValue card
	| card == Curse    = -1
	| card == Estate   =  1
	| card == Duchy    =  3
	| card == Province =  6
	| otherwise        =  0

cardCost :: Card -> Int
cardCost card
	-- Treasure cards
	| card == Copper    = 0
	| card == Silver    = 3
	| card == Gold      = 6
	-- Victory cards
	| card == Curse     = 0
	| card == Estate    = 2
	| card == Duchy     = 5
	| card == Province  = 8
	-- Action cards
	-- protocol v1
	| card == Mine        = 5 -- Trash a Treasure card from your hand. Gain a Trasure card costing up to 3 more; put it in into your hand
	-- protocol v2
	| card == Cellar     = 2 -- +1A, "Discards any number of cards. +1 Card per card discarded"
	| card == Village    = 3 -- +1 Card, +2 Actions
	| card == Woodcutter = 3 -- +1 Buy, +2 Coins
	| card == Workshop   = 3 -- Gain a card costing up to 4
	| card == Remodel    = 4 -- gain a card consting up to 2 more than the trashed card
	| card == Smithy     = 4 -- +3 Cards, 
	| card == Market     = 5 -- +1 Card, +1 Action, +1 Buy, +1 Coin
	-- extra
	| card == Chapel      = 2
	| card == Bureaucrat  = 2
	| card == Feast       = 2
	| card == Moneylender = 2
	| card == Chancellor  = 2
	| card == Thief       = 4
	| card == Militia     = 4  -- +2 Coin "Each other player discards down to 3 cards in his hand"
	| card == Council     = 5
	| card == Festival    = 5
	| card == Laboratory  = 5
	| card == Adventurer  = 5
	| otherwise           = 0
