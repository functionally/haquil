
# Data.Qubit
# Data.Int.Util
# Language.Quil.Types
# Language.Quil.Tests

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Arrow ((&&&), (***), second)
import Data.Foldable
import Data.Complex
import Data.List
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util
import Numeric.LinearAlgebra.Tensor
import qualified Data.Vector.Storable as V

isqrt :: Int -> Int
isqrt n = snd . head . dropWhile ((< n) . fst) $ ((^2) &&& id) <$> [0..]

ilog2 :: Int -> Int
ilog2 n = snd . head . dropWhile ((< n) . fst) $ ((2^) &&& id) <$> [0..]

ilog2sqrt :: Int -> Int
ilog2sqrt n = snd . head . dropWhile ((< n) . fst) $ ((^2) . (2^) &&& id) <$> [0..]

type Qubit = Int
type Amplitude = Complex Double
newtype Wavefunction = Wavefunction {wavefunction :: Tensor Amplitude}
  deriving (Eq, Floating, Fractional, Num)

instance Show Wavefunction where
  show =
    showTensor wavefunction
      $ \_ (k, v) ->
      showAmplitude v ++ "|" ++ concatMap show k ++ ">"

indexPrefix :: Variant -> Char
indexPrefix Contra = 'm'
indexPrefix Co     = 'n'
indexLabels :: Variant -> [Qubit] -> [(String, String)]
indexLabels variant qubits =
  let
    n = length qubits
    o =
      case variant of
        Contra -> 0
        Co     -> n
  in
    zipWith (\k v -> (show k, indexPrefix variant : show v)) [(1+o)..] qubits
wavefunctionLabels :: Int -> [(String, String)]
wavefunctionLabels n = indexLabels Contra [0..(n-1)]
operatorLabels :: [Qubit] -> [(String, String)]
operatorLabels qubits = indexLabels Contra qubits ++ indexLabels Co qubits

toWavefunction :: [Amplitude] -> Wavefunction
toWavefunction amplitudes =
  let
    n = ilog2 $ length amplitudes
  in
    Wavefunction
      . renameExplicit (wavefunctionLabels n)
      $ listTensor (replicate n 2) amplitudes
groundState :: Qubit -> Wavefunction
groundState = toWavefunction . (1 :) . (`replicate` 0) . (+ (-1)) . (2^)

newtype Operator = Operator {operator :: Tensor (Complex Double)}
  deriving (Eq, Floating, Fractional, Num)

showAmplitude :: Amplitude -> String
showAmplitude (a :+ b)
  | b == 0    = show a
  | a == 0    = show b ++ "j"
  | otherwise = "(" ++ show a ++ "+" ++ show b ++ "j)"

showTensor :: (a -> Tensor Amplitude) -> (Int -> ([Int], Amplitude) -> String) -> a -> String
showTensor toTensor format x =
    let
      x' = toTensor x
      n = order x'
    in
      (++ (filter (/= '"') . show . fmap tail . filter ((== indexPrefix Contra) . head) $ names x'))
      . (++ " @ ")
      . intercalate " + "
        . fmap (format n)
        . filter ((/= 0) . snd)
        . zip (fmap reverse . sequence $ replicate n [0, 1])
        . V.toList
        $ coords x'

instance Show Operator where
  show =
    showTensor operator
      $ \n (k, v) ->
      let
        (k0, k1) = splitAt (n `div` 2) $ concatMap show k
      in
        showAmplitude v ++ "|" ++ k0 ++ "><" ++ k1 ++ "|"

asOperator :: [Qubit] -> [Amplitude] -> Operator
asOperator qubits amplitudes =
  let
    n = length qubits
  in
    Operator
      . renameExplicit (operatorLabels qubits)
      $ listTensor (replicate n 2 ++ replicate n (-2)) amplitudes

mult :: Tensor Amplitude -> Tensor Amplitude -> Tensor Amplitude
mult x y =
  let
    lft  = filter ((== 'n') . head) $ names x
    rght = filter ((== 'm') . head) $ names y
    cmmn = fmap tail lft `intersect` fmap tail rght
    x' = renameExplicit [(i, 'p' : tail i) | i <- filter ((`elem` cmmn) . tail) lft ] x
    y' = renameExplicit [(i, 'p' : tail i) | i <- filter ((`elem` cmmn) . tail) rght] y
  in
    x' * y' 

(.*.) :: Operator -> Operator -> Operator
Operator x .*. Operator y = Operator $ x `mult` y
(.*) :: Operator -> Wavefunction -> Wavefunction
Operator x .* Wavefunction y = Wavefunction $ x `mult` y
(..*) :: Foldable t => t Operator -> Wavefunction -> Wavefunction
(..*) = flip . foldl $ flip (.*)

data Gate =
    I Qubit
  | X Qubit
  | Y Qubit
  | Z Qubit
  | H Qubit
  | PHASE Double Qubit
  | S Qubit
  | T Qubit
  | CPHASE00 Double Qubit Qubit
  | CPHASE01 Double Qubit Qubit
  | CPHASE10 Double Qubit Qubit
  | CPHASE Double Qubit Qubit
  | RX Double Qubit
  | RY Double Qubit
  | RZ Double Qubit
  | CNOT Qubit Qubit
  | CCNOT Qubit Qubit Qubit 
  | PSWAP Double Qubit Qubit
  | SWAP Qubit Qubit
  | ISWAP Qubit Qubit
  | CSWAP Qubit Qubit Qubit
  | CZ Qubit Qubit
  deriving (Eq, Ord, Read, Show)

fromGate :: Gate -> Operator

fromGate (I i) =
  asOperator [i]
    [
      1, 0
    , 0, 1
    ]

fromGate (X i) =
  asOperator [i]
    [
      0, 1
    , 1, 0
    ]

fromGate (Y i) =
  asOperator [i]
    [
      0     , 0 :+ (-1)
    , 0 :+ 1, 0
    ]

fromGate (Z i) =
  asOperator [i]
    [
      1, 0
    , 0, -1
    ]

fromGate (H i) =
  asOperator [i]
    [
      1, 1
    , 1, -1
    ] / sqrt 2

fromGate (PHASE theta i) =
  asOperator [i]
    [
      1, 0
    , 0, cis theta
    ]

fromGate (S i) =
  asOperator [i]
    [
      1, 0
    , 0, 0 :+ 1
    ]

fromGate (T i) =
  asOperator [i]
    [
      1, 0
    , 0, (1 :+ 1) / sqrt 2
    ]

fromGate (CPHASE00 theta i j) =
  asOperator [i, j]
    [
      cis theta, 0, 0, 0
    , 0        , 1, 0, 0
    , 0        , 0, 1, 0
    , 0        , 0, 0, 1
    ]

fromGate (CPHASE01 theta i j) =
  asOperator [i, j]
    [
      1, 0        , 0, 0
    , 0, cis theta, 0, 0
    , 0, 0        , 1, 0
    , 0, 0        , 0, 1
    ]

fromGate (CPHASE10 theta i j) =
  asOperator [i, j]
    [
      1, 0, 0        , 0
    , 0, 1, 0        , 0
    , 0, 0, cis theta, 0
    , 0, 0, 0        , 1
    ]

fromGate (CPHASE theta i j) =
  asOperator [i, j]
    [
      1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , 0, 0, 0, cis theta
    ]

fromGate (RX theta i) =
  asOperator [i]
    [
      cos (theta / 2) :+ 0        , 0 :+ (- sin (theta / 2))
    , 0 :+ (- sin (theta / 2)), cos (theta / 2) :+ 0
    ]

fromGate (RY theta i) =
  asOperator [i]
    [
      cos (theta / 2) :+ 0, (- sin (theta / 2)) :+ 0
    , sin (theta / 2) :+ 0, cos (theta / 2) :+ 0
    ]

fromGate (RZ theta i) =
  asOperator [i]
    [
      cis (- theta / 2), 0
    , 0                , cis (theta / 2)
    ]

fromGate (CNOT i j) =
  asOperator [i, j]
    [
      1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 0, 1
    , 0, 0, 1, 0
    ]

fromGate (CCNOT i j k) =
  asOperator [i, j, k]
    [
      1, 0, 0, 0, 0, 0, 0, 0
    , 0, 1, 0, 0, 0, 0, 0, 0
    , 0, 0, 1, 0, 0, 0, 0, 0
    , 0, 0, 0, 1, 0, 0, 0, 0
    , 0, 0, 0, 0, 1, 0, 0, 0
    , 0, 0, 0, 0, 0, 1, 0, 0
    , 0, 0, 0, 0, 0, 0, 0, 1
    , 0, 0, 0, 0, 0, 0, 1, 0
    ]
    
fromGate (PSWAP theta i j) =
  asOperator [i, j]
    [
      1, 0        , 0        , 0
    , 0, 0        , cis theta, 0
    , 0, cis theta, 0        , 0
    , 0, 0        , 0        , 1
    ]
    
fromGate (SWAP i j) =
  asOperator [i, j]
    [
      1, 0, 0, 0
    , 0, 0, 1, 0
    , 0, 1, 0, 0
    , 0, 0, 0, 1
    ]

fromGate (ISWAP i j) =
  asOperator [i, j]
    [
      1, 0     , 0     , 0
    , 0, 0     , 0 :+ 1, 0
    , 0, 0 :+ 1, 0     , 0
    , 0, 0     , 0     , 1
    ]
    
fromGate (CSWAP i j k) =
  asOperator [i, j, k]
    [
      1, 0, 0, 0, 0, 0, 0, 0
    , 0, 1, 0, 0, 0, 0, 0, 0
    , 0, 0, 1, 0, 0, 0, 0, 0
    , 0, 0, 0, 1, 0, 0, 0, 0
    , 0, 0, 0, 0, 1, 0, 0, 0
    , 0, 0, 0, 0, 0, 0, 1, 0
    , 0, 0, 0, 0, 0, 1, 0, 0
    , 0, 0, 0, 0, 0, 0, 0, 1
    ]
    
fromGate (CZ i j) =
  asOperator [i, j]
    [
      1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , 0, 0, 0, -1
    ]

theta = 0.54321
sequence_
  [
    do
      putStrLn l
      sequence_
        [
          putStrLn $ show (fromGate (o 0) .* e) ++ " : " ++ s
        |
          (s, e) <- second toWavefunction  <$> [("<0|", [1,0]), ("<1", [0,1])]
        ]
  |
    (l, o) <- [
                ("I", I)
              , ("X", X)
              , ("Y", Y)
              , ("Z", Z)
              , ("H", H)
              , ("PHASE", PHASE theta)
              , ("S", S)
              , ("T", T)
              , ("RX", RX theta)
              , ("RY", RY theta)
              , ("RZ", RZ theta)
              ]
  ]

sequence_
  [
    do
      putStrLn l
      sequence_
        [
          putStrLn $ show (fromGate (o 0 1) .* e) ++ " : " ++ s
        |
          (s, e) <- second toWavefunction  <$> [
                                                 ("<00|", [1,0,0,0])
                                               , ("<10|", [0,1,0,0])
                                               , ("<01|", [0,0,1,0])
                                               , ("<11|", [0,0,0,1])
                                               ]
        ]
  |
    (l, o) <- [
                ("CPHASE00", CPHASE00 theta)
              , ("CPHASE01", CPHASE01 theta)
              , ("CPHASE10", CPHASE10 theta)
              , ("CPHASE", CPHASE theta)
              , ("CNOT", CNOT)
              , ("PSWAP", PSWAP theta)
              , ("SWAP", SWAP)
              , ("ISWAP", ISWAP)
              , ("CZ", CZ)
              ]
  ]

sequence_
  [
    do
      putStrLn l
      sequence_
        [
          putStrLn $ show (fromGate (o 0 1 2) .* e) ++ " : " ++ s
        |
          (s, e) <- second toWavefunction  <$> [
                                                 ("<000|", [1,0,0,0,0,0,0,0])
                                               , ("<100|", [0,1,0,0,0,0,0,0])
                                               , ("<010|", [0,0,1,0,0,0,0,0])
                                               , ("<110|", [0,0,0,1,0,0,0,0])
                                               , ("<001|", [0,0,0,0,1,0,0,0])
                                               , ("<101|", [0,0,0,0,0,1,0,0])
                                               , ("<011|", [0,0,0,0,0,0,1,0])
                                               , ("<111|", [0,0,0,0,0,0,0,1])
                                               ]
        ]
  |
    (l, o) <- [
                ("CCNOT", CCNOT)
              , ("CSWAP", CSWAP)
              ]
  ]
