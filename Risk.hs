{-# LANGUAGE RebindableSyntax, NoImplicitPrelude, PatternSynonyms, LambdaCase,
             NamedFieldPuns, ScopedTypeVariables #-}

import Data.Function
import Data.List
import Data.Array
import Data.Monoid
import Numeric
import System.CPUTime
import System.IO

import ProbDist
import Dice

data Unit  = Footmen | Archers | Cavalry | Siege deriving (Eq, Ord, Show)
data Side  = Attacker | Defender deriving (Eq, Ord, Show)
data Rank  = RSiege | RVolley | RCavalry | RGeneral
type Strat = [Unit]

data Units
  = Units{ uFootmen, uArchers, uCavalry, uSiege :: Int }
  deriving (Eq, Ord)
              
data Army
  = Army{ aUnits::Units, aStrat::Strat }
  deriving Show

pattern Dead :: Units
pattern Dead <- Units 0 0 0 0

type Pr   = Float'
type D  a = Dist Pr a
type AD a = ApproxDist Pr a

(maxGenA, maxGenD) = (3, 2) :: (Int, Int)
aumpa = [Footmen] :: Strat

main = do
    hSetBuffering stdout LineBuffering
    sequence_ $ do
        f <- [0, 1]
        k <- [1, 3, 10, 30]
        let a1 = Army units{ uArchers=3*k, uFootmen=f*k } aumpa
        let a2 = Army units{ uCavalry=2*k, uFootmen=f*k } aumpa
        (aA,aD) <- [(a1,a2), (a2,a1)]
        return $ do
            putStrLn $ show (aUnits aA) ++ " vs " ++ show (aUnits aD)
            start <- getCPUTime
            print $ fight 0.00005 (aA,aD)
            end <- getCPUTime
            let elapsed = fromInteger (end - start) / 10^12
            putStrLn $ "(" ++ showFFloat (Just 2) elapsed " s)\n"

units :: Units
units = Units 0 0 0 0

uTotal :: Units -> Int
uTotal (Units a b c d) = a + b + c + d

fight :: Pr -> (Army, Army) -> D (Maybe Side)
fight threshold (armyA, armyD)
  = fight' threshold (stratA, stratD) (pure (unitsA, unitsD)) mempty
  where
    Army{ aUnits=unitsA, aStrat=stratA } = armyA
    Army{ aUnits=unitsD, aStrat=stratD } = armyD

fight' :: Pr -> (Strat,Strat) -> D (Units,Units) -> D (Maybe Side) -> D (Maybe Side)
fight' threshold strats unitsDist resultDist
  | finished  = resultDist''
  | otherwise = fight' threshold strats (fight'' strats unitsDist') resultDist''
  where
    (resultDist', unitsDist') = splitDist unitsDist $ \case
        (Dead, Dead) -> Left Nothing
        (_,    Dead) -> Left (Just Attacker)
        (Dead, _)    -> Left (Just Defender)
        units        -> Right units
    resultDist'' = resultDist <> resultDist'
    finished | threshold > 0 = measureDist unitsDist' < threshold
             | otherwise     = nullDist unitsDist'

fight'' :: (Strat, Strat) -> D (Units, Units) -> D (Units, Units)
fight'' strats unitsDist
 = unitsDist >>= fightR RSiege   strats
             >>= fightR RVolley  strats
             >>= fightR RCavalry strats
             >>= fightG          strats

fightR :: Rank -> (Strat, Strat) -> (Units, Units) -> D (Units, Units)
fightR rank (stratA, stratD) (unitsA, unitsD) = do
    hitA <- hitR rank unitsA
    let unitsD' = iterate (sacrifice stratD) unitsD !! hitA
    hitD <- hitR rank unitsD
    let unitsA' = iterate (sacrifice stratA) unitsA !! hitD
    return (unitsA', unitsD')

hitR :: Rank -> Units -> D Int
hitR rank units = case rank of
    RSiege   -> binomial23 !! (2 * uSiege units)
    RVolley  -> binomial13 !! uArchers units
    RCavalry -> binomial23 !! uCavalry units

fightG :: (Strat, Strat) -> (Units, Units) -> D (Units, Units)
fightG (stratA, stratD) (unitsA, unitsD) = do
    let diceA = maxGenA `min` uTotal unitsA
    let diceD = maxGenD `min` uTotal unitsD
    (hitA, hitD) <- hitG ! (diceA, diceD)
    let unitsA' = iterate (sacrifice stratA) unitsA !! hitD
    let unitsD' = iterate (sacrifice stratD) unitsD !! hitA
    return (unitsA', unitsD')

hitG :: Array (Int, Int) (D (Int, Int))
hitG
  = array ixs [((a,d), hitG' a d) | (a,d) <- range ixs]
  where
    ixs = ((0,0), (maxGenA,maxGenD))
    hitG' dA dD = do
        rsA <- (take 2 . reverse . sort) <$> replicateM dA (die 6)
        rsD <- (take 2 . reverse . sort) <$> replicateM dD (die 6)
        return $ foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0)
               $ map (\case GT -> (1,0); EQ -> (0,1); LT -> (0,1))
               $ zipWith compare rsA rsD

binomial13, binomial23 :: [D Int]
binomial13 = map (flip binomial (1/3)) [0..]
binomial23 = map (flip binomial (2/3)) [0..]

sacrifice :: Strat -> Units -> Units
sacrifice strat units = case units of
    Dead -> units
    _    -> sacrificeL (sacrificeT strat units) units

sacrificeL :: Unit -> Units -> Units
sacrificeL Siege   us@Units{uSiege=n}   = us{uSiege=n-1}
sacrificeL Archers us@Units{uArchers=n} = us{uArchers=n-1}
sacrificeL Cavalry us@Units{uCavalry=n} = us{uCavalry=n-1}
sacrificeL Footmen us@Units{uFootmen=n} = us{uFootmen=n-1}

sacrificeT :: Strat -> Units -> Unit
sacrificeT strat units
  | total == 0              = error "sacrificeT: zero units"
  | uSiege   units == total = Siege
  | uArchers units == total = Archers
  | uCavalry units == total = Cavalry
  | uFootmen units == total = Footmen
  where total = uTotal units
sacrificeT strat units
  = case [u | u <- strat, field u units > 0] of
        u : _ -> u
        []    -> error $ "sacrificeT: ambiguous strategy"
  where field unit = case unit of {
    Siege   -> uSiege;   Archers -> uArchers;
    Cavalry -> uCavalry; Footmen -> uFootmen }

instance Show Units where
    show units
      = "units{" ++ intercalate ", " fs ++ "}"
      where fs = [ s ++ "=" ++ show (f units)
                 | (s,f) <- [("uFootmen",uFootmen), ("uArchers",uArchers),
                             ("uCavalry",uCavalry), ("uSiege",uSiege)],
                   f units /= 0]
