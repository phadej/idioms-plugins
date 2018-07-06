# idioms-plugin

Hack idiom-brackets using Source Plugin.

As nobody (?) writes their lists as `([1, 2, 3])`, we can steal that syntax!

```haskell
{-# OPTIONS -fplugin=IdiomsPlugin #-}

module Main (main) where

main :: IO ()
main = do
    -- Just "foobar"
    print ([ mappend (Just "foo") (Just "bar") ])
    -- Nothing
    print ([ const (Just "foo") Nothing ])
    -- Just 3
    print ([ (+) (Just 1) (Just (2 :: Int)) ])
```
