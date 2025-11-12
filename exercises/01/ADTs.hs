-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

module ADTs where
-- TODO: talk about
-- - join + write on discord
-- - solutions (linked in discord)
-- - first homework this week
-- - `TODO` -> stuff for me
--   `TASK` -> stuff for you
-- TODO: show
-- - pragmas on the top of files
-- - `undefined` vs `error` vs `_`
-- - "taking arguments" (via `C`-style signatures)
-- - operators and sections
-- - `let in` and `where`
-- - `case` and pattern matching
-- - guards
-- - mention `deriving (Show)`

----------------------------------------------
-- RPS (Rock, Paper, Scissors) and Booleans --
----------------------------------------------

-- RPS - enum example
-- This will be our type representing Rock, Paper, and Scissors
-- TODO: show `case` here
data RPS = Rock | Paper | Scissors
  deriving (Show)

-- `beats` function: Implement the logic for which RPS beats another
-- TODO:
-- Use pattern matching and ignore some cases to demonstrate the use of _

-- >>> beats Rock Paper
-- False
-- >>> beats Paper Rock
-- True
beats :: RPS -> RPS -> Bool
beats Paper Rock = True
beats Rock Scissors = True
beats Scissors Paper = True
beats _ _ = False

-- TASK:
-- Define the "next" throw you can do in the "usual" ordering of RPS
-- i.e. `next x` should be the throw that beats x

-- >>> next Rock
-- Paper
next :: RPS -> RPS
next Rock = Paper
next Paper = Scissors
next Scissors = Rock

-- TASK:
-- Define what it means for two RPS values to be equal
-- Use pattern matching and use _ matches!

-- >>> eqRPS Rock Rock
-- True
-- >>> eqRPS Rock Paper
-- False
eqRPS :: RPS -> RPS -> Bool
eqRPS Rock Rock = True
eqRPS Scissors Scissors = True
eqRPS Paper Paper = True
eqRPS _ _ = False

-- TASK:
-- Define a shorter version of beats using next and eqRPS

-- >>> beats' Rock Paper
-- True
-- >>> beats' Paper Scissors
-- True

beats' :: RPS -> RPS -> Bool
beats' x y = next x `eqRPS` y

------------
-- Points --
------------

-- Record syntax for a point in 2D discrete space
data Point = MkPoint Integer Integer
  deriving (Show)

-- Check if a point is in the first quadrant (both x and y are positive)
isInFirstQuadrant :: Point -> Bool
isInFirstQuadrant (MkPoint x y) = x > 0 && y > 0

inWhichQuadrantIsIn :: Point -> Integer
inWhichQuadrantIsIn (MkPoint x y) = if x > 0 && y > 0 then 1 else undefined

inWhichQuadrantIsIn' :: Point -> Integer
inWhichQuadrantIsIn' (MkPoint x y)
  | x >= 0 && y >= 0 = 1
  | x  < 0 && y >= 0 = 2
  | x  < 0 && y  < 0 = 3
  | x >= 0 && y  < 0 = 4
  | otherwise = error "the universe has stopped working"

-- Invert a point by swapping the signs of x and y
invert :: Point -> Point
invert (MkPoint x y) = MkPoint (-x) (-y)

----------------------------------------
-- Natural Numbers (Peano Arithmetic) --
----------------------------------------

-- Encoding for natural numbers using Peano arithmetic
data Nat = Zero | Succ Nat
  deriving (Show)

-- 2
-- Succ (Succ Zero)
-- 1+ 1+ 0

-- TASK:
-- Convert an Integer to Nat

-- >>> integerToNat 3
-- Succ (Succ (Succ Zero))

integerToNat :: Integer -> Nat
integerToNat x =
  if x == 0
  then Zero
  else Succ (integerToNat (x - 1))

-- TASK:
-- Convert a Nat back to an Integer

-- >>> natToInteger (Succ (Succ Zero))
-- 2

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- TASK:
-- Add two Nats

-- >>> addNat (Succ Zero) (Succ Zero)
-- Succ (Succ Zero)

addNat :: Nat -> Nat -> Nat
addNat Zero n2 = n2
addNat (Succ n1) n2 = Succ (addNat n1 n2)

-- addNat (Succ (Succ Zero))    (Succ Zero)
--              ^^^^^^^^^^^ n1  ^^^^^^^^^^^ n2
-- Succ (addNat (Succ Zero) (Succ Zero))
--                    ^^^ n1 ^^^^^^^^^ n2
-- Succ (Succ (addNat Zero (Succ Zero)))
-- Succ (Succ (Succ Zero))

-- TASK:
-- Multiply two Nats

-- >>> multNat (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

multNat :: Nat -> Nat -> Nat
multNat Zero _ = Zero
multNat (Succ n1) n2 = n2 `addNat` (n1 `multNat` n2)

-- TASK:
-- Compare two Nats, returning an Ordering

-- >>> compareNat (Succ Zero) Zero
-- GT

compareNat :: Nat -> Nat -> Ordering
compareNat Zero Zero = EQ
compareNat Zero (Succ _) = LT
compareNat (Succ _) Zero = GT
compareNat (Succ n1) (Succ n2) = compareNat n1 n2

-- TASK:
-- Return the maximum of two Nats

-- >>> maxNat (Succ Zero) (Succ (Succ Zero))
-- Succ (Succ Zero)

maxNat :: Nat -> Nat -> Nat
maxNat Zero n2 = n2
maxNat n1 Zero = n1
maxNat (Succ n1) (Succ n2) = Succ (maxNat n1 n2)

-----------------
-- Expressions --
-----------------

-- A simple expression language for a calculator
data Expr
  = Val Integer
  | Plus Expr Expr
  | Mult Expr Expr
  deriving (Show)

infixr 7 `Plus`
infixr 8 `Mult`

-- TASK:
-- Evaluate an expression in the Expr language

-- >>> eval (Val 3)
-- 3
-- >>> eval (Plus (Val 3) (Val 4))
-- 7
-- >>> eval (Mult (Val 3) (Val 4))
-- 12

eval :: Expr -> Integer
eval (Val n) = n
eval (Plus n1 n2) = eval n1 + eval n2
eval (Mult n1 n2) = eval n1 * eval n2

-- TASK:
-- Extend the Expr language with If expressions
-- Interpret 0 as "false" and any non-zero value as "true"

------------
-- Belote --
------------

-- Data type for Ranks in a card game
data Rank = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show)

-- Data type for Suits in a card game
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show)

-- TASK:
-- Check if two suits are equal
suitEquals :: Suit -> Suit -> Bool
suitEquals Clubs Clubs = True
suitEquals Diamonds Diamonds = True
suitEquals Hearts Hearts = True
suitEquals Spades Spades = True
suitEquals _ _ = False

-- Record syntax for representing a Card
data Card = MkCard {rank :: Rank, suit :: Suit}
  deriving (Show)

-- Data type for Contracts in the Belote game
data Contract = Trumps Suit | NoTrump | AllTrumps
  deriving (Show)

-- TASK:
-- Check if a card is of a trump suit based on a given contract
isTrump :: Contract -> Card -> Bool
isTrump (Trumps trumpSuit) (MkCard _rank cardSuit) = suitEquals trumpSuit cardSuit
isTrump AllTrumps _ = True
isTrump NoTrump _ = False

-- TASK:
-- Assign a numerical power value to a card based on the contract
-- Ensure that higher power values represent stronger cards
cardPower :: Contract -> Card -> Integer
cardPower contract card@(MkCard rank _suit) =
  if isTrump contract card
    then rankTrump
    else rankNoTrump
  where
    rankTrump :: Integer
    rankTrump =
      case rank of
        Seven -> 0
        Eight -> 1
        Queen -> 2
        King -> 3
        Ten -> 4
        Ace -> 5
        Nine -> 6
        Jack -> 7

    rankNoTrump :: Integer
    rankNoTrump =
      case rank of
        Seven -> 0
        Eight -> 1
        Nine -> 2
        Jack -> 3
        Queen -> 4
        King -> 5
        Ten -> 6
        Ace -> 7


-- TASK:
-- A data type to describe the different ways two cards can relate, given a contract
-- See the 'sameSuit' and 'relateCards' functions below to get a better sense of how
-- you'll be producing this data type, and hence what constructors it should have
--
-- This data type exists mainly because it's useful as a tool to implement the 'fight' function
-- As such, it might be the case that your version of CardRelation is different from what I intended
--
-- The way to think about this is as follows:
-- Imagine you're in a situation where someone has played a card, and you've just played a card
-- You now need to decide which card would beat the other one
-- 'CardRelation' expresses the first thing you need to calculate in regards to the two cards
-- *before* you can start checking their 'cardPower's, e.g. is one of them a trump and so on
--
-- HINT:
-- The intended solution has 4 constructors
data CardRelation = FirstIsTrump | SecondIsTrump | SameSuit | DifferentSuit
  deriving (Show)

-- TASK:
-- Given a contract, calculate how two cards relate
relateCards :: Contract -> Card -> Card -> CardRelation
relateCards contract card1@(MkCard _ suit1) card2@(MkCard _ suit2) =
  case (isTrump contract card1, isTrump contract card2) of
    (False, False) ->
      if suit1 `suitEquals` suit2
        then SameSuit
        else DifferentSuit
    (True, False) -> FirstIsTrump
    (False, True) -> SecondIsTrump
    (True, True) -> SameSuit

-- TASK:
-- Given a contract and two cards, return the winning card
-- Assume the first card is played first
fight :: Contract -> Card -> Card -> Card
fight contract card1 card2 =
  case relateCards contract card1 card2 of
    FirstIsTrump -> card1
    SecondIsTrump -> card2
    DifferentSuit -> card1
    SameSuit ->
      if cardPower contract card1 <= cardPower contract card2
        then card2
        else card1

-- Data type for a trick (игра, разигравка, ръка, рунд), consisting of four cards
data Trick = MkTrick Card Card Card Card
  deriving (Show)

-- TASK:
-- Given a contract and a Trick, determine the winning card
-- Remember that the leftmost card was played first
winner :: Contract -> Trick -> Card
winner contract (MkTrick card1 card2 card3 card4) =
  ((card1 `fight'` card2) `fight'` card3) `fight'` card4
  where
    fight' = fight contract

-- TASK:
-- Check if a Trick could have been played according to the rules of Belote
isValid :: Contract -> Trick -> Bool
isValid contract (MkTrick card1 card2 card3 card4) = 
  isValidCard card2 card1 &&
  isValidCard card3 card1 &&
  isValidCard card4 card1
  where
    getSuit (MkCard _ suit) = suit

    isValidCard :: Card -> Card -> Bool
    isValidCard current leading = 
      let currSuit = getSuit current
          leadSuit = getSuit leading
          followsLeading = currSuit `suitEquals` leadSuit
          mustFollowLead = isTrump contract leading
      in followsLeading || (not followsLeading && isTrump contract current) || True

-- Example
exampleContract :: Contract
exampleContract = Trumps Spades

exampleTrick :: Trick
exampleTrick = MkTrick exampleCard1 exampleCard2 exampleCard3 exampleCard4
  where
    exampleCard1, exampleCard2, exampleCard3, exampleCard4 :: Card
    exampleCard1 = MkCard Ace Spades
    exampleCard2 = MkCard Jack Hearts
    exampleCard3 = MkCard King Spades
    exampleCard4 = MkCard Seven Diamonds

-- isValid exampleContract exampleTrick
-- True
