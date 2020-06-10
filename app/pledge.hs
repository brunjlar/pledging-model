{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Main
    ( main
    ) where

import Control.Monad.Random (MonadRandom (..))
import Control.Monad.Reader
import Data.Foldable
import Data.Function        (on)
import Data.List            (sortBy)
import Data.Maybe           (fromMaybe)
import GSL.Random.Dist
import GSL.Random.Gen
import Numeric.Natural      (Natural)
import Options.Generic
import Text.Printf          (printf)

data Args w = Args
    { players   :: w ::: Maybe Natural <?> "number of players (default: 35000)"
    , pools     :: w ::: Maybe Natural <?> "desired number of pools (default: 200)"
    , epoch     :: w ::: Maybe Double  <?> "days per epoch (default: 5)"
    , total     :: w ::: Maybe Double  <?> "ada in circulation (default: 31112483745)"
    , supply    :: w ::: Maybe Double  <?> "max supply of ada (default: 45000000000)"
    , expansion :: w ::: Maybe Double  <?> "monetary expansion per epoch (default: 0.0012)"
    , treasury  :: w ::: Maybe Double  <?> "treasury ratio (default: 0.1)"
    , rate      :: w ::: Maybe Double  <?> "exchange rate ($/ada) (default: 0.08)"
    , minCost   :: w ::: Maybe Double  <?> "min cost per year ($) (default: 0)"
    , scale     :: w ::: Maybe Double  <?> "Weibull scale (default: 8684)"
    , shape     :: w ::: Maybe Double  <?> "Weibull shape (default: 2)"
    , pareto    :: w ::: Maybe Double  <?> "Pareto alpha (default: 1.16)"
    , whale     :: w ::: Maybe Double  <?> "relative whale threshold (default: 0.0005)"
    , influence :: w ::: Maybe Double  <?> "pledge influence (default: 0.5)"
    } deriving Generic

data Config = Config
    { cfgN              :: !Natural -- ^ number of players
    , cfgK              :: !Natural -- ^ desired number of pools
    , cfgEpoch          :: !Double  -- ^ days per epoch
    , cfgTotal          :: !Double  -- ^ ada in circulation
    , cfgSupply         :: !Double  -- ^ max supply of ada
    , cfgExpansion      :: !Double  -- ^ monetary expansion per epoch
    , cfgTreasury       :: !Double  -- ^ treasury ration
    , cfgRate           :: !Double  -- ^ exchage rate ($/ada)
    , cfgMinCostPerYear :: !Double  -- ^ min cost per year ($)
    , cfgWeibullScale   :: !Double  -- ^ Weibull scale
    , cfgWeibullShape   :: !Double  -- ^ Weibull shape
    , cfgParetoAlpha    :: !Double  -- ^ Pareto alpha
    , cfgWhaleThreshold :: !Double  -- ^ threshold of stake for a whale
    , cfgA0             :: !Double  -- ^ pool stake versus leader stake
    } deriving (Show, Read, Eq, Ord)

deriving anyclass instance ParseRecord (Args Wrapped)

main :: IO ()
main = do
    args <- unwrapRecord "Cardano Pledging Model" :: IO (Args Unwrapped)
    runM pledge Config
        { cfgN              = fromMaybe 35000       $ players   args
        , cfgK              = fromMaybe 200         $ pools     args
        , cfgEpoch          = fromMaybe 5           $ epoch     args
        , cfgTotal          = fromMaybe 31112483745 $ total     args
        , cfgSupply         = fromMaybe 45000000000 $ supply    args
        , cfgExpansion      = fromMaybe 0.0012      $ expansion args
        , cfgTreasury       = fromMaybe 0.1         $ treasury  args
        , cfgRate           = fromMaybe 0.08        $ rate      args
        , cfgMinCostPerYear = fromMaybe 0           $ minCost   args
        , cfgWeibullScale   = fromMaybe 8684        $ scale     args
        , cfgWeibullShape   = fromMaybe 2           $ shape     args
        , cfgParetoAlpha    = fromMaybe 1.16        $ pareto    args
        , cfgWhaleThreshold = fromMaybe 0.0005      $ whale     args
        , cfgA0             = fromMaybe 0.5         $ influence args
        }

data Player = Player
    { plStake :: !Rational
    , plCost  :: !Double
    } deriving (Show, Read, Eq, Ord)

newtype M a = M (ReaderT Config IO a)
    deriving newtype (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadRandom)

runM :: M a -> Config -> IO a
runM (M m) = runReaderT m

-- | Randomly draw a weight from the Pareto distribution with the given alpha.
paretoWeight :: MonadRandom m => Double -> m Int
paretoWeight alpha = do
    u <- (1 -) <$> getRandomR (0, 1)
    return $ round $ recip $ u ** recip alpha

mkPlayers :: M [Player]
mkPlayers = do
    n     <- asks cfgN
    k     <- asks cfgK
    a0    <- asks cfgA0
    alpha <- asks cfgParetoAlpha
    let z0' = 1 / fromIntegral k
        z   = fromRational z0'
        mw  = paretoWeight alpha
    ws <- replicateM (fromIntegral n) mw
    cs <- sampleRelCosts
    let potential lam c = (z + a0 * lam) / (1 + a0) - c
        q               = fromIntegral (sum ws)
        ws'             = [fromIntegral w / q | w <- ws]
        wcs             = zip ws' cs
        h (w, c)        = - (potential (fromRational w) c)
        wcs'            = sortBy (compare `on` h) wcs
        ps              = uncurry Player <$> wcs'
    return ps

pledge :: M ()
pledge = do
    ps                    <- mkPlayers
    poolCount             <- asks cfgK
    adaHolders            <- asks cfgN
    daysPerEpoch          <- asks cfgEpoch
    adaInCirculation      <- asks cfgTotal
    maxSupply             <- asks cfgSupply
    expansionRatio        <- asks cfgExpansion
    treasuryRatio         <- asks cfgTreasury
    dollarsPerAda         <- asks cfgRate
    minCostDollarsPerYear <- asks cfgMinCostPerYear
    weibullScale          <- asks cfgWeibullScale
    weibullShape          <- asks cfgWeibullShape
    paretoAlpha           <- asks cfgParetoAlpha
    whaleThreshold        <- asks cfgWhaleThreshold
    a0                    <- asks cfgA0
    let nonWhales  = [p | p <- ps, fromRational (plStake p) < whaleThreshold] -- remove whales from list of players
    nonWhales' <- unfoldM f nonWhales                                         -- have players with higher-than-saturation stake split their stake
    let operators' = take (succ $ fromIntegral poolCount) nonWhales'          -- consider top (k+1) players
        loser      = last operators'                                          -- (k+1)-st player
        operators  = take (fromIntegral poolCount) operators'                 -- pick top k players as pool operators
    oms <- mapM (\p -> margin p loser >>= \m -> return (p, m)) operators      -- players and their ideal margin
    let richest    = maximumBy (compare `on` plStake) operators               -- richest pool operator
        poorest    = minimumBy (compare `on` plStake) operators               -- poorest pool operator
        middle     = operators !! div (fromIntegral poolCount) 2              -- middle pool operator
    sybil <- sybilProtectionStake middle                                      -- relative stake needed by a sybil attacker
    richestA <- stakeToAda $ plStake richest
    poorestA <- stakeToAda $ plStake poorest
    sybilA   <- stakeToAda $ toRational sybil
    richestD <- stakeToDollars $ plStake richest
    poorestD <- stakeToDollars $ plStake poorest
    sybilD   <- stakeToDollars $ toRational sybil
    rewards  <- adaRewardsPerEpoch

    printOperators oms
    liftIO $ printf "\n"

    liftIO $ printf "number of ada holders: %11d\n"   adaHolders
    liftIO $ printf "number of pools:       %11d\n"   poolCount
    liftIO $ printf "days per epoch:        %13.1f\n" daysPerEpoch
    liftIO $ printf "ada in circulation:    %11.0f\n" adaInCirculation
    liftIO $ printf "max supply of ada:     %11.0f\n" maxSupply
    liftIO $ printf "monetary expansion:    %16.4f\n" expansionRatio
    liftIO $ printf "treasury ratio:        %16.4f\n" treasuryRatio
    liftIO $ printf "exchange rate ($/ada): %16.4f\n" dollarsPerAda
    liftIO $ printf "min cost per year ($): %11.0f\n" minCostDollarsPerYear
    liftIO $ printf "Weibull scale:         %11.0f\n" weibullScale
    liftIO $ printf "Weibull shape:         %14.2f\n" weibullShape
    liftIO $ printf "Pareto alpha:          %14.2f\n" paretoAlpha
    liftIO $ printf "Whale threshold:       %16.4f\n" whaleThreshold
    liftIO $ printf "pledge influence:      %14.2f\n" a0
    liftIO $ printf "\n"
    liftIO $ printf "number of whales:                   %11d\n"   $ fromIntegral adaHolders - length nonWhales
    liftIO $ printf "rewards per epoch (ada)           : %11.0f\n" rewards
    liftIO $ printf "richest pool operator stake (ada) : %11.0f\n" richestA
    liftIO $ printf "poorest pool operator stake (ada) : %11.0f\n" poorestA
    liftIO $ printf "sybil attacker min stake    (ada) : %11.0f\n" sybilA
    liftIO $ printf "richest pool operator stake ($)   : %11.0f\n" richestD
    liftIO $ printf "poorest pool operator stake ($)   : %11.0f\n" poorestD
    liftIO $ printf "sybil attacker min stake    ($)   : %11.0f\n" sybilD
  where
    sybilProtectionStake :: Player -> M Rational
    sybilProtectionStake p = do
        k  <- fromIntegral <$> asks cfgK
        mc <- asks cfgMinCostPerYear >>= dollarsPerYearToRelative
        a0 <- asks cfgA0
        let l = fromRational $ plStake p
        return $ toRational $ (l - (plCost p - mc) * (1 + 1 / a0)) * k / 2

    f :: [Player] -> M (Maybe (Player, [Player]))
    f []       = return Nothing
    f (p : ps) = do
        z <- z0
        return $ Just $ if plStake p > z then (p {plStake = z}, p {plStake = plStake p - z} : ps)
                                         else (p, ps)

    printOperators :: [(Player, Double)] -> M ()
    printOperators ps = do
        liftIO $ printf "pool     pledge (ada)   cost per year ($)   pool rewards per epoch (ada) potential pool profit per epoch (ada)    margin  operator profit per epoch (ada)  ROI (percent)\n\n"
        forM_ (zip [1 :: Int ..] ps) $ \(i, (p, m)) -> do
            stake <- stakeToAda $ plStake p
            cost  <- relativeToDollarsPerYear $ plCost p
            spr   <- (*) <$> adaRewardsPerEpoch <*> satPoolRewards (plStake p)
            ppp   <- (*) <$> poolPotential p <*> adaRewardsPerEpoch
            op    <- (*) <$> operatorProfit p m <*> adaRewardsPerEpoch
            roi   <- do
                e <- epochsPerYear
                c <- asks cfgTotal
                return $ 100 * op * e / (fromRational (plStake p) * c)
            liftIO $ printf "%6d    %11.0f               %5.0f                       %8.0f                              %8.0f  %8.6f                         %8.0f          %5.2f\n" i stake cost spr ppp m op roi

epochsPerYear :: M Double
epochsPerYear = (365 /) <$> asks cfgEpoch

adaRewardsPerEpoch :: M Double
adaRewardsPerEpoch = do
    cfg <- ask
    let t = cfgTreasury  cfg
        e = cfgExpansion cfg
        s = cfgSupply    cfg
        c = cfgTotal     cfg
    return $ (1 - t) * e * (s - c)

dollarsToAda :: Double -> M Double
dollarsToAda d = (d /) <$> asks cfgRate

dollarsPerYearToRelative :: Double -> M Double
dollarsPerYearToRelative d = do
    e <- epochsPerYear
    r <- adaRewardsPerEpoch
    a <- dollarsToAda d
    return $ a / e / r

relativeToDollarsPerYear :: Double -> M Double
relativeToDollarsPerYear r = do
    a <- adaRewardsPerEpoch
    e <- epochsPerYear
    x <- asks cfgRate
    return $ r * a * e * x

z0 :: M Rational
z0 = do
    k <- asks cfgK
    return $ 1 / fromIntegral k

stakeToAda :: Rational -> M Double
stakeToAda s = do
    c <- asks cfgTotal
    return $ c * fromRational s

stakeToDollars :: Rational -> M Double
stakeToDollars s = (*) <$> asks cfgRate <*> stakeToAda s

sampleRelCosts :: M [Double]
sampleRelCosts = do
    rng <- liftIO $ newRNG mt19937
    n   <- asks cfgN
    sc  <- asks cfgWeibullScale
    sh  <- asks cfgWeibullShape
    mc  <- asks cfgMinCostPerYear
    cs  <- map (max mc) <$> replicateM (fromIntegral n) (liftIO $ getWeibull rng sc sh)
    mapM dollarsPerYearToRelative cs

-- | Rewards of a fully-saturated pool, given its pledge.
satPoolRewards :: Rational -> M Double
satPoolRewards lam = do
    z  <- z0
    a0 <- asks cfgA0
    let lam' = fromRational $ min lam z
        beta = fromRational z
    return $ 1 / (1 + a0) * (beta + lam' * a0)

-- | Potential (i.e. rewards minus costs) of a fully-saturate pool run by the given player.
poolPotential :: Player -> M Double
poolPotential p = do
    r <- satPoolRewards (plStake p)
    return $ r - plCost p

-- | Ideal margin for a player, given the next player below in the ranking.
margin :: Player -> Player -> M Double
margin p q = do
    potP <- poolPotential p
    potQ <- poolPotential q
    return $ 1 - potQ / potP

-- | Profit for a pool operated by the given player with the given margin.
operatorProfit :: Player -> Double -> M Double
operatorProfit p m = do
    ppp <- poolPotential p
    z   <- z0
    let x = m * ppp
        r = (ppp - x) * fromRational (plStake p/ z)
    return $ x + r

unfoldM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM f b = do
    m <- f b
    case m of
        Nothing      -> return []
        Just (a, b') -> (a :) <$> unfoldM f b'
