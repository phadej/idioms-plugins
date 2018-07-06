{-# OPTIONS -fplugin=IdiomsPlugin #-}
-- | Hack idiom-brackets using Source Plugin.
--
-- As nobody (?) writes their lists as `([1, 2, 3])`,
-- we can steal that syntax!
module Main (main) where

main :: IO ()
main = do
    -- Just "foobar"
    print ([ mappend (Just "foo") (Just "bar") ])
    -- Nothing
    print ([ const (Just "foo") Nothing ])
    -- Just 3
    print ([ (+) (Just 1) (Just (2 :: Int)) ])

    -- Just True
    print ([ Just True || Just False ])

    -- [1,2,3], because `pure 1` can be specialised to [1]
    -- and non singleton lists are not transformed
    print $ ([ 1 ]) ++ ([2, 3]) ++ ([])
