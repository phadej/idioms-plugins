{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS -fplugin=IdiomsPlugin #-}
-- | Hack idiom-brackets using Source Plugin.
--
-- As nobody (?) writes their lists as `([1, 2, 3])`,
-- we can steal that syntax!
module Main (main) where

import Control.Applicative ((<|>))

main :: IO ()
main = do
    putStrLn "Basics:"
    -- Just "foobar"
    print ([ mappend (Just "foo") (Just "bar") ])
    -- Nothing
    print ([ const (Just "foo") Nothing ])
    -- Just 3
    print ([ (+) (Just 1) (Just (2 :: Int)) ])

    putStrLn "Works with operators:"
    -- [True,True,True,False,True,False,True,False]
    print ([ [True,False] && [True,False] || [True,False] ])

    putStrLn "Explicitly void-ed expressions are skipped"
    -- Nothing
    -- Just 1
    print ([ subtract (Just 2) (void Nothing) (Just 3) ])
    print ([ subtract (Just 2) (void (Just True)) (Just 3) ])

    putStrLn "ParallelListComp for Alternatives:"
    -- [True,True,True,False,True,False,False,False]
    -- [True,True,True,False,True,False,False,False,False,True,False,True]
    print ([ [True, False] || [True, False]
           | [True, False] && [True, False]
           ])
    print ([ [True, False] || [True, False]
           | [True, False] && [True, False]
           -- recall: void skips, but effects are visible
           | not (void [(), ()]) [True, False]
           ])

    -- because `pure 1` can be specialised to [1]
    -- and non singleton lists are not transformed, this works
    putStrLn "Works with lists, sometimes:"
    -- [1,2,3]
    print $ ([ 1 ]) ++ ([2, 3]) ++ ([])
