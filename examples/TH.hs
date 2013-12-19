{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
import Data.Table
import Control.Lens
import Data.List

data P t = P { _pId :: Int, _a,_b,_c :: Double, _t :: t }
         deriving (Show)
makeLenses ''P

makeTable ''P '_pId ['_a `isA` 'Candidate, '_b `isA` 'Candidate, '_c `isA` 'Candidate]


test :: Table (P Bool)
test = zipWith5 P [1 .. ]
    [0.27,1.75,1.13,-0.86,-0.25,-0.07,-0.09,0.3,-0.13,1.2,1.1,0.68,-1.08,0.74,-0.21,1.53,-1.21,-0.9,1.18,-1.94]
    [-0.51,0.5,0.37,0.34,0.29,0.38,-0.86,-0.21,-0.69,-0.29,0,2.03,2.09,0.35,-0.12,1.76,-0.49,0,-0.86,-0.29]
    [-1.11,-1.47,-1.09,0.22,-0.05,0.39,1.49,-0.29,-2.43,0.23,-0.49,-0.24,1.09,1.03,-0.78,0.42,0.94,0.1,0.21,-1.56]
    [False,False,True,True,True,True,True,False,True,False,False,True,True,True,False,False,True,False,True,False]
    ^.table


main = do
    print $ test^..with P_a (<) 0.with P_b (>) 0.rows.t
