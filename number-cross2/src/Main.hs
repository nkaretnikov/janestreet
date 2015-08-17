{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Data.List hiding (transpose)
import Data.List.Split
import Data.Matrix
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Z3.Monad

main :: IO ()
main =
  evalZ3With Nothing opts script >>= \case
    (r,Nothing) -> putStrLn $ "no solution: " <> show r
    (_,Just xs) -> do
      let chunksOf9    = chunksOf 9
          splitOn0     = splitOn [0]
          sumProduct x = (x, sum x, product x)
          clues        = fmap sumProduct . concatMap (filter (not . null) . splitOn0) . chunksOf9
          toMatrix     = fromList 9 9
          indexes      = [1, 14, 22, 2, 23, 3, 20, 4, 26, 12, 5, 15, 6, 21, 7, 18, 8, 19, 28] :: [Int]
          across       = clues xs
          down         = clues $ toList $ transpose $ toMatrix xs
          down'        = map snd $ sortBy (comparing fst) $ zip indexes down
      putStrLn $ show $ toMatrix xs

      putStrLn "Across:"
      F.forM_ across $ print
      putStr "\n"

      putStrLn "Down:"
      F.forM_ down' $ print
      putStr "\n"

      putStrLn $ "Answer: " <> show (sum xs)
  where
    opts = opt "MODEL" True

mkCell :: MonadZ3 m => String -> m AST
mkCell str = do
  _1 <- mkInteger 1
  _9 <- mkInteger 9
  x <- mkFreshIntVar str
  assert =<< mkAnd =<< T.sequence
    [mkLe _1 x, mkLe x _9]
  return x

mkClue' :: MonadZ3 m => [AST] -> Integer -> m ()
mkClue' xs x = do
  s <- mkAdd xs
  p <- mkMul xs
  i <- mkInteger x
  sEq <- mkEq s i
  pEq <- mkEq p i
  assert =<< mkOr [sEq,pEq]

data Across = AcrossAST { unAcrossAST :: [AST] }
            | AcrossInt { unAcrossInt :: Integer } 

data Down = DownAST { unDownAST :: [AST] }
          | DownInt { unDownInt :: Integer } 

class Clue a where
  mkClue :: MonadZ3 m => a -> a -> m ()

instance Clue Across where
  mkClue xs x = mkClue' (unAcrossAST xs) (unAcrossInt x)

instance Clue Down where
  mkClue xs x = mkClue' (unDownAST xs) (unDownInt x)

a1  = AcrossInt 9   :: Across
a5  = AcrossInt 35  :: Across
a9  = AcrossInt 10  :: Across
a10 = AcrossInt 30  :: Across
a11 = AcrossInt 7   :: Across
a13 = AcrossInt 10  :: Across
a14 = AcrossInt 42  :: Across
a16 = AcrossInt 21  :: Across
a17 = AcrossInt 25  :: Across
a18 = AcrossInt 15  :: Across
a20 = AcrossInt 120 :: Across
a22 = AcrossInt 25  :: Across
a24 = AcrossInt 35  :: Across
a25 = AcrossInt 21  :: Across
a27 = AcrossInt 9   :: Across
a29 = AcrossInt 5   :: Across
a30 = AcrossInt 8   :: Across

d1  = DownInt 45 :: Down
d2  = DownInt 20 :: Down
d3  = DownInt 48 :: Down
d4  = DownInt 72 :: Down
d5  = DownInt 18 :: Down
d6  = DownInt 24 :: Down
d7  = DownInt 27 :: Down
d8  = DownInt 26 :: Down
d12 = DownInt 12 :: Down
d14 = DownInt 18 :: Down
d15 = DownInt 32 :: Down
d18 = DownInt 45 :: Down
d19 = DownInt 20 :: Down
d20 = DownInt 30 :: Down
d21 = DownInt 12 :: Down
d22 = DownInt 70 :: Down
d23 = DownInt 12 :: Down
d26 = DownInt 2  :: Down
d28 = DownInt 36 :: Down

script :: Z3 (Result, Maybe [Integer])
script = do
  _1_1   <- mkCell "1 1"
  _1_2   <- mkCell "1 2"
  _1_3   <- mkCell "1 3"
  _1_4   <- mkCell "1 4"
  let wa1 = AcrossAST [_1_1, _1_2, _1_3, _1_4]

  _5_5   <- mkCell "5 5"
  _5_6   <- mkCell "5 6"
  _5_7   <- mkCell "5 7"
  _5_8   <- mkCell "5 8"
  let wa5 = AcrossAST [_5_5, _5_6, _5_7, _5_8]

  _9_1   <- mkCell "9 1"
  _9_2   <- mkCell "9 2"
  _9_3   <- mkCell "9 3"
  _9_4   <- mkCell "9 4"
  let wa9 = AcrossAST [_9_1, _9_2, _9_3, _9_4]

  _10_5  <- mkCell "10 5"
  _10_6  <- mkCell "10 6"
  _10_7  <- mkCell "10 7"
  _10_8  <- mkCell "10 8"
  let wa10 = AcrossAST [_10_5, _10_6, _10_7, _10_8]

  _11_2  <- mkCell "11 2"
  _11_3  <- mkCell "11 3"
  _11_4  <- mkCell "11 4"
  _11_12 <- mkCell "11 12"
  let wa11 = AcrossAST [_11_2, _11_3, _11_4, _11_12]

  _13_6  <- mkCell "13 6"
  _13_7  <- mkCell "13 7"
  _13_8  <- mkCell "13 8"
  let wa13 = AcrossAST [_13_6, _13_7, _13_8]

  _14_14 <- mkCell "14 14"
  _14_2  <- mkCell "14 2"
  _14_3  <- mkCell "14 3"
  _14_4  <- mkCell "14 4"
  _14_12 <- mkCell "14 12"
  _14_15 <- mkCell "14 15"
  _14_6  <- mkCell "14 6"
  let wa14 = AcrossAST [_14_14, _14_2, _14_3, _14_4, _14_12, _14_15, _14_6]

  _16_14 <- mkCell "16 14"
  _16_2  <- mkCell "16 2"
  let wa16 = AcrossAST [_16_14, _16_2]

  _17_4  <- mkCell "17 4"
  _17_12 <- mkCell "17 12"
  _17_15 <- mkCell "17 15"
  let wa17 = AcrossAST [_17_4, _17_12, _17_15]

  _18_18 <- mkCell "18 18"
  _18_19 <- mkCell "18 19"
  let wa18 = AcrossAST [_18_18, _18_19]

  _20_20 <- mkCell "20 20"
  _20_4  <- mkCell "20 4"
  _20_12 <- mkCell "20 12"
  _20_15 <- mkCell "20 15"
  _20_21 <- mkCell "20 21"
  _20_18 <- mkCell "20 18"
  _20_19 <- mkCell "20 19"
  let wa20 = AcrossAST [_20_20, _20_4, _20_12, _20_15, _20_21, _20_18, _20_19]

  _22_22 <- mkCell "22 22"
  _22_23 <- mkCell "22 23"
  _22_20 <- mkCell "22 20"
  let wa22 = AcrossAST [_22_22, _22_23, _22_20]

  _24_12 <- mkCell "24 12"
  _24_15 <- mkCell "24 15"
  _24_21 <- mkCell "24 21"
  _24_18 <- mkCell "24 18"
  let wa24 = AcrossAST [_24_12, _24_15, _24_21, _24_18]

  _25_22 <- mkCell "25 22"
  _25_23 <- mkCell "25 23"
  _25_20 <- mkCell "25 20"
  _25_26 <- mkCell "25 26"
  let wa25 = AcrossAST [_25_22, _25_23, _25_20, _25_26]

  _27_15 <- mkCell "27 15"
  _27_21 <- mkCell "27 21"
  _27_18 <- mkCell "27 18"
  _27_28 <- mkCell "27 28"
  let wa27 = AcrossAST [_27_15, _27_21, _27_18, _27_28]

  _29_22 <- mkCell "29 22"
  _29_23 <- mkCell "29 23"
  _29_20 <- mkCell "29 20"
  _29_26 <- mkCell "29 26"
  let wa29 = AcrossAST [_29_22, _29_23, _29_20, _29_26]

  _30_15 <- mkCell "30 15"
  _30_21 <- mkCell "30 21"
  _30_18 <- mkCell "30 18"
  _30_28 <- mkCell "30 28"
  let wa30 = AcrossAST [_30_15, _30_21, _30_18, _30_28]

  let wd1  = DownAST [_1_1, _9_1]
      wd2  = DownAST [_1_2, _9_2, _11_2, _14_2, _16_2]
      wd3  = DownAST [_1_3, _9_3, _11_3, _14_3]
      wd4  = DownAST [_1_4, _9_4, _11_4, _14_4, _17_4, _20_4]
      wd5  = DownAST [_5_5, _10_5]
      wd6  = DownAST [_5_6, _10_6, _13_6, _14_6]
      wd7  = DownAST [_5_7, _10_7, _13_7]
      wd8  = DownAST [_5_8, _10_8, _13_8]
      wd12 = DownAST [_11_12, _14_12, _17_12, _20_12, _24_12]
      wd14 = DownAST [_14_14, _16_14]
      wd15 = DownAST [_14_15, _17_15, _20_15, _24_15, _27_15, _30_15]
      wd18 = DownAST [_18_18, _20_18, _24_18, _27_18, _30_18]
      wd19 = DownAST [_18_19, _20_19]
      wd20 = DownAST [_20_20, _22_20, _25_20, _29_20]
      wd21 = DownAST [_20_21, _24_21, _27_21, _30_21]
      wd22 = DownAST [_22_22, _25_22, _29_22]
      wd23 = DownAST [_22_23, _25_23, _29_23]
      wd26 = DownAST [_25_26, _29_26]
      wd28 = DownAST [_27_28, _30_28]

  mkClue wa1  a1
  mkClue wa5  a5
  mkClue wa9  a9
  mkClue wa10 a10
  mkClue wa11 a11
  mkClue wa13 a13
  mkClue wa14 a14
  mkClue wa16 a16
  mkClue wa17 a17
  mkClue wa18 a18
  mkClue wa20 a20
  mkClue wa22 a22
  mkClue wa24 a24
  mkClue wa25 a25
  mkClue wa27 a27
  mkClue wa29 a29
  mkClue wa30 a30

  mkClue wd1 d1
  mkClue wd2 d2
  mkClue wd3 d3
  mkClue wd4 d4
  mkClue wd5 d5
  mkClue wd6 d6
  mkClue wd7 d7
  mkClue wd8 d8
  mkClue wd12 d12
  mkClue wd14 d14
  mkClue wd15 d15
  mkClue wd18 d18
  mkClue wd19 d19
  mkClue wd20 d20
  mkClue wd21 d21
  mkClue wd22 d22
  mkClue wd23 d23
  mkClue wd26 d26
  mkClue wd28 d28

  _XXX <- mkInteger 0   -- used for filtering
  withModel $ \m ->
    catMaybes <$> mapM (evalInt m)
      [ _1_1  , _1_2  , _1_3  , _1_4  , _XXX  , _5_5  , _5_6  , _5_7  , _5_8
      , _9_1  , _9_2  , _9_3  , _9_4  , _XXX  , _10_5 , _10_6 , _10_7 , _10_8
      , _XXX  , _11_2 , _11_3 , _11_4 , _11_12 , _XXX  , _13_6 , _13_7 , _13_8
      , _14_14, _14_2 , _14_3 , _14_4 , _14_12, _14_15, _14_6 , _XXX  , _XXX
      , _16_14, _16_2 , _XXX  , _17_4 , _17_12, _17_15, _XXX  , _18_18, _18_19
      , _XXX  , _XXX  , _20_20, _20_4 , _20_12, _20_15, _20_21, _20_18, _20_19
      , _22_22, _22_23, _22_20, _XXX  , _24_12, _24_15, _24_21, _24_18, _XXX
      , _25_22, _25_23, _25_20, _25_26, _XXX  , _27_15, _27_21, _27_18, _27_28
      , _29_22, _29_23, _29_20, _29_26, _XXX  , _30_15, _30_21, _30_18, _30_28 ]
