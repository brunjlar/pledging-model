{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators     #-}

module Main
    ( main
    ) where

import Control.Monad        (replicateM)
import Control.Monad.Random (MonadRandom (..))
import Data.Foldable
import Data.Function        (on)
import Data.List            (sortBy, unfoldr)
import Data.Maybe           (fromMaybe)
import Numeric.Natural      (Natural)
import Options.Generic
import Text.Printf          (printf)

data Args w = Args
    { players   :: w ::: Maybe Natural <?> "number of players (default: 35000)"
    , pools     :: w ::: Maybe Natural <?> "desired number of pools (default: 200)"
    , epoch     :: w ::: Maybe Double  <?> "days per epoch (default: 5)"
    , total     :: w ::: Maybe Double  <?> "ada in circulation (default: 31112087041.174194)"
    , rewards   :: w ::: Maybe Double  <?> "ada rewards per day (default: 3400000)"
    , rate      :: w ::: Maybe Double  <?> "exchange rate ($/ada) (default: 0.08)"
    , minCost   :: w ::: Maybe Double  <?> "min cost per year ($) (default: 6000)"
    , maxCost   :: w ::: Maybe Double  <?> "max cost per year ($) (default: 20000)"
    , pareto    :: w ::: Maybe Double  <?> "Pareto alpha (default: 1.16)"
    , whale     :: w ::: Maybe Double  <?> "relative whale threshold (default: 0.0005)"
    , influence :: w ::: Maybe Double  <?> "pledge influence (default: 0.5)"
    } deriving Generic

deriving instance ParseRecord (Args Wrapped)

main :: IO ()
main = do
    args <- unwrapRecord "Cardano Pledging Model" :: IO (Args Unwrapped)
    pledge
        (fromMaybe 35000              $ players   args)
        (fromMaybe 200                $ pools     args)
        (fromMaybe 5                  $ epoch     args)
        (fromMaybe 31112087041.174194 $ total     args)
        (fromMaybe 3400000            $ rewards   args)
        (fromMaybe 0.08               $ rate      args)
        (fromMaybe 6000               $ minCost   args)
        (fromMaybe 20000              $ maxCost   args)
        (fromMaybe 1.16               $ pareto    args)
        (fromMaybe 0.0005             $ whale     args)
        (fromMaybe 0.5                $ influence args)

data Config = Config
    { cfgN              :: !Int      -- ^ number of players
    , cfgK              :: !Int      -- ^ desired number of pools
    , cfgA0             :: !Double   -- ^ pool stake versus leader stake
    , cfgMinCost        :: !Double   -- ^ minimal player cost
    , cfgMaxCost        :: !Double   -- ^ maximal player cost
    , cfgParetoAlpha    :: !Double   -- ^ Pareto alpha
    , cfgWhaleThreshold :: !Rational -- ^ threshold of stake for a whale
    } deriving (Show, Read, Eq, Ord)

data Player = Player
    { plStake :: !Rational
    , plCost  :: !Double
    } deriving (Show, Read, Eq, Ord)

-- | Randomly draw a weight from the Pareto distribution with the given alpha.
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
    let potential lam c = (z0 + a0 * lam) / (1 + a0) - c
        q               = fromIntegral (sum ws)
        ws'             = [fromIntegral w / q | w <- ws]
        wcs             = zip ws' cs
        h (w, c)        = - (potential (fromRational w) c)
        wcs'            = sortBy (compare `on` h) wcs
        ps              = uncurry Player <$> wcs'
    return ps

pledge :: Natural  -- ^ number of ada holders
       -> Natural  -- ^ pool count
       -> Double   -- ^ days per epoch
       -> Double   -- ^ ada in circulation
       -> Double   -- ^ ada rewards per day
       -> Double   -- ^ dollars per ada
       -> Double   -- ^ min cost dollars per year
       -> Double   -- ^ max cost dollars per year
       -> Double   -- ^ Pareto alpha
       -> Double   -- ^ whale threshold
       -> Double   -- ^ pledge influence
       -> IO ()
pledge adaHolders poolCount daysPerEpoch adaInCirculation adaRewardsPerDay dollarsPerAda minCostDollarsPerYear maxCostDollarsPerYear paretoAlpha whaleThreshold a0  = do
    printf "number of ada holders:     %11d\n"   adaHolders
    printf "number of pools:           %11d\n"   poolCount
    printf "days per epoch:            %13.1f\n" daysPerEpoch
    printf "ada in circulation:        %11.0f\n" adaInCirculation
    printf "ada rewards per day:       %11.0f\n" adaRewardsPerDay
    printf "dollars per ada:           %16.4f\n" dollarsPerAda
    printf "min dollars cost per year: %11.0f\n" minCostDollarsPerYear
    printf "max dollars cost per year: %11.0f\n" maxCostDollarsPerYear
    printf "Pareto alpha:              %14.2f\n" paretoAlpha
    printf "Whale threshold:           %16.4f\n" whaleThreshold
    printf "pledge influence:          %14.2f\n" a0
    printf "\n"
    printf "ada rewards per epoch:     %11.0f\n" adaRewardsPerEpoch
    printf "min relative costs:        %18.6f\n" (cfgMinCost cfg)
    printf "max relative costs:        %18.6f\n" (cfgMaxCost cfg)
    printf "\n"

    ps <- mkPlayers cfg

    let nonWhales  = [p | p <- ps, plStake p < cfgWhaleThreshold cfg] -- remove whales from list of players
        nonWhales' = unfoldr f nonWhales                              -- have players with higher-than-saturation stake split their stake
        operators' = take (succ $ fromIntegral poolCount) nonWhales'  -- consider top (k+1) players
        loser      = last operators'                                  -- (k+1)-st player
        operators  = take (fromIntegral poolCount) operators'         -- pick top k players as pool operators
        oms        = [(p, margin p loser) | p <- operators]           -- players and their ideal margin
        richest    = maximumBy (compare `on` plStake) operators       -- richest pool operator
        poorest    = minimumBy (compare `on` plStake) operators       -- poorest pool operator
        middle     = operators !! div (cfgK cfg) 2                    -- middle pool operator
        sybil      = sybilProtectionStake middle                      -- relative stake needed by a sybil attacker

    printOperators oms
    printf "\n"

    printf "number of whales:             %11d\n"   $ cfgN cfg - length nonWhales
    printf "richest pool operator stake (ada) : %11.0f\n" $ stakeToAda $ plStake richest
    printf "poorest pool operator stake (ada) : %11.0f\n" $ stakeToAda $ plStake poorest
    printf "sybil attacker min stake    (ada) : %11.0f\n" $ stakeToAda $ sybil
    printf "richest pool operator stake ($)   : %11.0f\n" $ dollarsPerAda * stakeToAda (plStake richest)
    printf "poorest pool operator stake ($)   : %11.0f\n" $ dollarsPerAda * stakeToAda (plStake poorest)
    printf "sybil attacker min stake    ($)   : %11.0f\n" $ dollarsPerAda * stakeToAda sybil
  where
    cfg :: Config
    cfg = Config
        { cfgN              = fromIntegral adaHolders
        , cfgK              = fromIntegral poolCount
        , cfgA0             = a0
        , cfgMinCost        = dollarsPerYearToRelative minCostDollarsPerYear
        , cfgMaxCost        = dollarsPerYearToRelative maxCostDollarsPerYear
        , cfgWhaleThreshold = toRational whaleThreshold
        , cfgParetoAlpha    = paretoAlpha
        }

    sybilProtectionStake :: Player -> Rational
    sybilProtectionStake p =
        let l = fromRational $ plStake p
            k = fromIntegral $ cfgK cfg
        in  toRational $ (l - (plCost p - cfgMinCost cfg) * (1 + 1 / cfgA0 cfg)) * k / 2

    f :: [Player] -> Maybe (Player, [Player])
    f []       = Nothing
    f (p : ps)
        | plStake p > z0 = Just (p {plStake = z0}, p {plStake = plStake p - z0} : ps)
        | otherwise      = Just (p, ps)

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

    -- | Rewards of a fully-saturated pool, given its pledge.
    satPoolRewards :: Rational -> Double
    satPoolRewards lam =
        let lam' = fromRational $ min lam z0
            beta = fromRational z0
        in  1 / (1 + a0) * (beta + lam' * a0)

    -- | Potential (i.e. rewards minus costs) of a fully-saturate pool run by the given player.
    poolPotential :: Player -> Double
    poolPotential p = satPoolRewards (plStake p) - plCost p

    -- | Ideal margin for a player, given the next player below in the ranking.
    margin :: Player -> Player -> Double
    margin p q =
        let potP = poolPotential p
            potQ = poolPotential q
        in  1 - potQ / potP

    -- | Profit for a pool operated by the given player with the given margin.
    operatorProfit :: Player -> Double -> Double
    operatorProfit p m =
        let ppp = poolPotential p
            x   = m * ppp
            r   = (ppp - x) * fromRational (plStake p/ z0)
        in  x + r

