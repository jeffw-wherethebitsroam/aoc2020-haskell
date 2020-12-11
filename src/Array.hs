module Array
  ( (!),
    (//),
    A.range,
    A.bounds,
    A.Array,
    A.array,
  )
where

-- a wrapper arround Array to catch index exceptions

import Control.Exception
import qualified GHC.Arr as A

addErrorInfo info (ErrorCall str) = ErrorCall (str ++ ":" ++ info)

arr ! index = mapException (addErrorInfo (" ! " ++ show index)) $ arr A.! index

arr // idxs = mapException (addErrorInfo (" // " ++ show idxs)) $ arr A.// idxs