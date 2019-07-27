{-
Author: Nick(Mykola) Pershyn
Language: Haskell
Program: Dominion Client
-}

module DominionPlayers where


import DominionTypes
import DominionCards

type PlayStrategy = (DominionTypes.State -> DominionTypes.Play)
--type MovedAction = (DominionTypes.Notification -> Maybe DominionTypes.Play)

-- TODO: State monad?
data DominionPlayer = DominionPlayer {
--	respondToNotification :: Response,
  playStrategy :: PlayStrategy
}

playerResponse :: DominionPlayer -> DominionTypes.Notification -> Maybe String
playerResponse player (MoveNotification  _    ) = Nothing
playerResponse player (StateNotification state) = Just $ show $ (playStrategy player) state


-- actual players
dumbPlayer = DominionPlayer doNothing
okayPlayer_v1 = DominionPlayer bigMoney  -- plays bigMoney
-- randy -- plays random startegy




doNothing :: PlayStrategy
doNothing state
  | hand state == [] = Play Clean []
  | otherwise        = Play Clean ((head (hand state)):[])

-- strategy that relies only on Treasure and Victory cards.
bigMoney :: PlayStrategy
bigMoney state
  -- have treasures in my hand => play them
  | ((deckTreasureValue (hand state)) > 0) = Play AddTreasure ((head (getDeckTreasures (hand state))):[])
  -- can buy Province and good on treasure cards => buy Provinve
  | (canBuyCard Province state) &&
  (((numberOfCardsInDeck (getFullPlayerDeck state) Gold  ) >  0) ||
  ((numberOfCardsInDeck (getFullPlayerDeck state) Silver) >= 5)) = Play BuyCard [Province]
  -- can buy Gold and game is far from the end => buy Gold
  | (canBuyCard Gold state) &&
  ((numberOfCardsInDeck (supply state) Province) > 4) = Play BuyCard [Gold]
  -- can buy Duchy and game is near the end => buy Duchy
  | (canBuyCard Duchy state) &&
  ((numberOfCardsInDeck (supply state) Province) < 6) = Play BuyCard [Duchy]
  -- can buy Silver and game is far from the end => buy Silver
  | (canBuyCard Silver state) &&
  ((numberOfCardsInDeck (supply state) Province) > 2) = Play BuyCard [Silver]
  -- can buy Estate and game is near the end => buy Estate
  | (canBuyCard Estate state) &&
  ((numberOfCardsInDeck (supply state) Province) < 4) = Play BuyCard [Estate]
  -- well, otherwise just end the turn
  | otherwise = doNothing state

deckTreasureValue :: DominionTypes.Deck -> Int
deckTreasureValue deck = sum $ map treasureValue deck

getDeckTreasures :: DominionTypes.Deck -> DominionTypes.Deck
getDeckTreasures deck = filter (\card -> (treasureValue card) > 0) deck

getFullPlayerDeck :: DominionTypes.State -> DominionTypes.Deck
getFullPlayerDeck state = (deck state) ++ (hand state) ++ (plays state) ++ (discards state)

numberOfCardsInDeck :: Deck -> Card -> Int
numberOfCardsInDeck deck card = length (filter (==card) deck)

canBuyCard :: Card -> State -> Bool
canBuyCard card state = ((numberOfCardsInDeck (supply state) card > 0) && ((coins state) >= (cardCost card)) && ((buys state) > 0))
