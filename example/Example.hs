{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# OPTIONS -fplugin=IdiomsPlugin #-}
-- | Hack idiom-brackets using Source Plugin.
--
-- As nobody (?) writes their lists as `([1, 2, 3])`,
-- we can steal that syntax!
module Main (main) where

import Control.Applicative (some, (<|>))
import Data.String         (IsString (..))
import Data.Traversable    (foldMapDefault)
import Test.HUnit          ((@?=))
import Text.Parsec         (parse)
import Text.Parsec.Char    (alphaNum, char, spaces)
import Text.Parsec.String  (Parser)

main :: IO ()
main = do
    -- Basics
    ([ mappend (Just "foo") (Just "bar") ])
        @?= Just "foobar"
    ([ const (Just "foo") Nothing ])
        @?= Nothing
    ([ (+) (Just 1) (Just (2 :: Int)) ])
        @?= Just 3

    -- Works with operators:"
    ([ Just 1 + Just (2 :: Int) ])
        @?= Just 3
    ([ [True,False] && [True,False] || [True,False] ])
        @?= [True,True,True,False,True,False,True,False]

    -- Explicitly void-ed expressions are skipped
    ([ subtract (Just 2) (void Nothing) (Just 3) ])
        @?= Nothing
    ([ subtract (Just 2) (void (Just True)) (Just 3) ])
        @?= Just 1

    -- ParallelListComp for Alternatives:
    ([ [True, False] || [True, False]
     | [True, False] && [True, False]
     ]) @?= [True,True,True,False,True,False,False,False]

    ([ [True, False] || [True, False]
     | [True, False] && [True, False]
     -- recall: void skips, but effects are visible
     | not (void [(), ()]) [True, False]
     ]) @?= [True,True,True,False,True,False,False,False,False,True,False,True]

    -- Works with lists, sometimes:
    --
    -- because `pure 1` can be specialised to [1]
    -- and non singleton lists are not transformed, this works
    --
    ([ 1 ]) ++ ([2, 3]) ++ ([])
        @?= [1,2,3]

    -- Parser examples
    parse parser "<test>" "foo" @?= Right (Leaf "foo")
    parse parser "<test>" "((x y) (u v))" @?= Right
        (Branch (Branch "x" "y") (Branch "u" "v"))

-------------------------------------------------------------------------------
-- Functor and Traversable are very similar
-------------------------------------------------------------------------------

-- | Binary tree
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show)

-- Compare Functor and Traversable instances:

instance Functor Tree where
    fmap f = go where
        go (Leaf a)     = Leaf (f a)
        go (Branch l r) = Branch (go l) (go r)

instance Traversable Tree where
    traverse f = go where
        go (Leaf a)     = ([ Leaf (f a) ])
        go (Branch l r) = ([ Branch (go l) (go r) ])

instance Foldable Tree where foldMap = foldMapDefault

instance IsString a => IsString (Tree a) where
    fromString = Leaf . fromString

-------------------------------------------------------------------------------
-- Parsec
-------------------------------------------------------------------------------

parser :: Parser (Tree String)
parser =
    ([ Leaf (some alphaNum)
     | Branch (void (char '(')) parser (void spaces) parser (void (char ')'))
     ])
