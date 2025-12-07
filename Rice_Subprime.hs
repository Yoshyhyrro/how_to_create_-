{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RiceSubprimeModelFixed where

import Control.Lens 
-- | Core parameter record.
data Params = Params
  { _alphaD  :: Double
  , _betaD   :: Double
  , _gammaD  :: Double
  , _alphaS  :: Double
  , _betaS   :: Double
  , _gammaS  :: Double
  , _tau     :: Int
  }
  deriving (Show, Eq)

-- lensの自動生成
makeLenses ''Params

-- | State for a single period.
data PeriodState = PeriodState
  { _tIndex   :: Int
  , _pPolicy  :: Double
  , _pMarket  :: Double
  , _qDemand  :: Double
  , _qSupply  :: Double
  , _stock    :: Double
  , _evalLoss :: Double
  }
  deriving (Show, Eq)
makeLenses ''PeriodState

-- | JA Financial State
data JAState = JAState
  { _loans     :: Double
  , _capital   :: Double
  , _badRatio  :: Double
  }
  deriving (Show, Eq)
makeLenses ''JAState

data FarmTerms = FarmTerms
  { _qFarm     :: Double
  , _costFarm  :: Double
  , _repayFarm :: Double
  }
  deriving (Show, Eq)
makeLenses ''FarmTerms

-- | Supply function (Determined by LAGGED price)
-- | Q_s(t) depends on P(t - tau)
qSupplyLinear :: Params -> Double -> Double -> Double
qSupplyLinear p pLag xS = 
    (p^.alphaS) + (p^.betaS) * pLag + (p^.gammaS) * xS

-- | Demand function
qDemandLinear :: Params -> Double -> Double -> Double
qDemandLinear p pCurr xD = 
    (p^.alphaD) + (p^.betaD) * pCurr + (p^.gammaD) * xD

-- | [BUG FIX]
-- | Solve for Market Clearing Price (Short-run equilibrium).
-- | In a cobweb model, supply is fixed for the period. Price adjusts so Q_d = Q_s_fixed.
-- |
-- | Q_fixed = alphaD + betaD * P + gammaD * xD
-- | => P = (Q_fixed - alphaD - gammaD * xD) / betaD
solveClearingPrice :: Params -> Double -> Double -> Either String Double
solveClearingPrice params qFixed xD =
  let bD = params^.betaD
  in if abs bD < 1e-12
       then Left "Demand slope (betaD) is too close to zero."
       else Right $ (qFixed - (params^.alphaD) - (params^.gammaD) * xD) / bD

-- | Default proxy (Logistic)
defaultProxy :: Double -> FarmTerms -> Double -> Double
defaultProxy pMarket terms scale =
  let cf = pMarket * (terms^.qFarm) - (terms^.costFarm)
      z  = ((terms^.repayFarm) - cf) / max 1e-6 scale
  in 1 / (1 + exp (-z))

-- | One-step update
stepPeriod
  :: Params
  -> FarmTerms
  -> JAState
  -> Double        -- ^ Previous Stock
  -> Double        -- ^ Lagged Price (P_{t-tau}) determining current supply
  -> Double        -- ^ Policy Price
  -> Double        -- ^ Demand covariate X_d
  -> Double        -- ^ Supply covariate X_s
  -> Double        -- ^ Gov intervention G
  -> Double        -- ^ Sensitivity
  -> Either String (PeriodState, JAState, Double) -- ^ Use Either for safety
stepPeriod params farm ja sPrev pLag pPol xD xS g sens = do
    -- 1. Determine fixed supply based on lagged price
    let qS = qSupplyLinear params pLag xS

    -- 2. Determine clearing price (fix: solve for price that clears qS)
    pMkt <- solveClearingPrice params qS xD

    -- 3. Calculate Demand (should equal qS in equilibrium, but we calculate explicitly)
    let qD = qDemandLinear params pMkt xD
    
    -- 4. Update Stock (S_t+1 = S_t + Q_s - Q_d - G)
    --    Note: If market clears perfectly, qS - qD = 0, so stock changes only by G.
    let sNew = sPrev + qS - qD - g

    -- 5. Calculate Valuation Loss
    let loss = sNew * max 0 (pPol - pMkt)

    -- 6. Update JA State (using Lens)
    let jaNew = ja & capital -~ (loss * sens)
                   
    -- 7. Calculate Default Proxy
    let dpx = defaultProxy pMkt farm 1.0
        jaFinal = jaNew & badRatio .~ dpx -- Update observed bad ratio

    let pState = PeriodState 
          { _tIndex = 0 -- Should be passed or incremented
          , _pPolicy = pPol
          , _pMarket = pMkt
          , _qDemand = qD
          , _qSupply = qS
          , _stock = sNew
          , _evalLoss = loss 
          }

    return (pState, jaFinal, dpx)
