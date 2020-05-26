module Main
    ( main
    ) where

import Control.Monad        (replicateM)
import Control.Monad.Random (MonadRandom (..))
import Data.Foldable
import Data.Function        (on)
import Data.List            (sort, sortBy)
import Numeric.Natural      (Natural)
import Text.Printf          (printf)

data Config = Config
    { cfgN           :: !Int    -- ^ number of players
    , cfgK           :: !Int    -- ^ desired number of pools
    , cfgA0          :: !Double -- ^ pool stake versus leader stake
    , cfgMinCost     :: !Double -- ^ minimal player cost
    , cfgMaxCost     :: !Double -- ^ maximal player cost
    , cfgParetoAlpha :: !Double -- ^ Pareto alpha
    } deriving (Show, Read, Eq, Ord)

data Player = Player
    { plStake :: !Rational
    , plCost  :: !Double
    } deriving (Show, Read, Eq, Ord)

paretoWeight :: MonadRandom m => Double -> m Int
paretoWeight alpha = do
    u <- (1 -) <$> getRandomR (0, 1)
    return $ round $ recip $ u ** recip alpha

mkPlayers :: MonadRandom m => Config -> m [Player]
mkPlayers cfg = do
    let n   = cfgN cfg
        k   = cfgK cfg
        z0' = 1 / fromIntegral k
        z0  = fromRational z0'
        a0  = cfgA0 cfg
        mw  = paretoWeight $ cfgParetoAlpha cfg
        mc  = getRandomR (cfgMinCost cfg, cfgMaxCost cfg)
    ws <- replicateM n mw
    cs <- replicateM n mc
    let cc  = sort cs !! k
        potential lam c = (z0 + a0 * lam) / (1 + a0) - c
        q               = fromIntegral (sum ws)
        ws'             = [fromIntegral w / q | w <- ws]
        wcs             = zip ws' cs
        h (w, c)        = - (potential (fromRational w) c)
        wcs'            = sortBy (compare `on` h) wcs
        g i (w, c)      = (i, Player w c)
        ps              = uncurry Player <$> wcs'
    return ps

main :: IO ()
main = pledge
    35000
    200
    5
    31112087041.174194
    3400000
    0.054084
    6000
    20000
    1.16
    0.1

pledge :: Natural -- ^ number of ada holders
       -> Natural -- ^ pool count
       -> Double  -- ^ days per epoch
       -> Double  -- ^ ada in circulation
       -> Double  -- ^ ada rewards per day
       -> Double  -- ^ dollars per ada
       -> Double  -- ^ min cost dollars per year
       -> Double  -- ^ max cost dollars per year
       -> Double  -- ^ Pareto alpha
       -> Double  -- ^ pledge influence
       -> IO ()
pledge adaHolders poolCount daysPerEpoch adaInCirculation adaRewardsPerDay dollarsPerAda minCostDollarsPerYear maxCostDollarsPerYear paretoAlpha a0  = do
    printf "number of ada holders:     %11d\n"   adaHolders
    printf "number of pools:           %11d\n"   poolCount
    printf "days per epoch:            %13.1f\n" daysPerEpoch
    printf "ada in circulation:        %11.0f\n" adaInCirculation
    printf "ada rewards per day:       %11.0f\n" adaRewardsPerDay
    printf "dollars per ada:           %16.4f\n" dollarsPerAda
    printf "min dollars cost per year: %11.0f\n" minCostDollarsPerYear
    printf "max dollars cost per year: %11.0f\n" maxCostDollarsPerYear
    printf "Pareto alpha:              %14.2f\n" paretoAlpha
    printf "pledge influence:          %14.2f\n" a0
    printf "\n"
    printf "ada rewards per epoch:     %11.0f\n" adaRewardsPerEpoch
    printf "min relative costs:        %18.6f\n" (cfgMinCost cfg)
    printf "max relative costs:        %18.6f\n" (cfgMaxCost cfg)
    printf "\n"

    ps <- mkPlayers cfg

    let whales     = [p | p <- ps, plStake p >= z0 / 10]
        operators' = take (succ $ fromIntegral poolCount) $ drop (length whales) $ toList ps
        loser      = last operators'
        operators  = take (fromIntegral poolCount) operators'
        oms        = [(p, margin p loser) | p <- operators]
        richest    = maximumBy (compare `on` plStake) operators
        poorest    = minimumBy (compare `on` plStake) operators

    printOperators oms
    printf "\n"

    printf "number of whales:            %11d\n"   $ length whales
    printf "richest pool operator stake: %11.0f\n" $ stakeToAda $ plStake richest
    printf "poorest pool operator stake: %11.0f\n" $ stakeToAda $ plStake poorest
  where
    cfg :: Config
    cfg = Config
        { cfgN           = fromIntegral adaHolders
        , cfgK           = fromIntegral poolCount
        , cfgA0          = a0
        , cfgMinCost     = dollarsPerYearToRelative minCostDollarsPerYear
        , cfgMaxCost     = dollarsPerYearToRelative maxCostDollarsPerYear
        , cfgParetoAlpha = paretoAlpha
        }

    z0 :: Rational
    z0 = 1 / fromIntegral poolCount

    epochsPerYear, adaRewardsPerEpoch :: Double
    epochsPerYear      = 365 / daysPerEpoch
    adaRewardsPerEpoch = adaRewardsPerDay * daysPerEpoch

    dollarsToAda :: Double -> Double
    dollarsToAda = (/ dollarsPerAda)

    dollarsPerYearToRelative :: Double -> Double
    dollarsPerYearToRelative d = dollarsToAda d / epochsPerYear / adaRewardsPerEpoch

    relativeToDollarsPerYear :: Double -> Double
    relativeToDollarsPerYear = (* (adaRewardsPerEpoch * epochsPerYear * dollarsPerAda))

    stakeToAda :: Rational -> Double
    stakeToAda = (* adaInCirculation) . fromRational

    printOperators :: [(Player, Double)] -> IO ()
    printOperators ps = do
        printf "pool     pledge (ada)   cost per year ($)   pool rewards per epoch (ada) potential pool profit per epoch (ada)    margin  operator profit per epoch (ada)  ROI (percent)\n\n"
        forM_ (zip [1 :: Int ..] ps) $ \(i, (p, m)) -> do
            let stake = stakeToAda $ plStake p
                cost  = relativeToDollarsPerYear $ plCost p
                spr   = satPoolRewards (plStake p) * adaRewardsPerEpoch
                ppp   = poolPotential p * adaRewardsPerEpoch
                op    = operatorProfit p m * adaRewardsPerEpoch
                roi   = 100 * op * epochsPerYear / (fromRational (plStake p) * adaInCirculation)
            printf "%6d    %11.0f               %5.0f                       %8.0f                              %8.0f  %8.6f                         %8.0f          %5.2f\n" i stake cost spr ppp m op roi

    satPoolRewards :: Rational -> Double
    satPoolRewards lam =
        let lam' = fromRational $ min lam z0
            beta = fromRational z0
        in  1 / (1 + a0) * (beta + lam' * a0)

    poolPotential :: Player -> Double
    poolPotential p = satPoolRewards (plStake p) - plCost p

    margin :: Player -> Player -> Double
    margin p q =
        let potP = poolPotential p
            potQ = poolPotential q
        in  1 - potQ / potP

    operatorProfit :: Player -> Double -> Double
    operatorProfit p m =
        let ppp = poolPotential p
            x   = m * ppp
            r   = (ppp - x) * fromRational (plStake p/ z0)
        in  x + r
    
