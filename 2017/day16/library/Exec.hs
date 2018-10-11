module Exec (exec, execAll, execAllWith) where

import Line
import Lang
import Control.Monad.State

caseIt :: Show a => a -> Maybe b -> b
caseIt inst Nothing = error $ "Failed on instruction: " ++ show inst
caseIt _ (Just l) = l

exec :: (Eq a, Show a, MonadState (Line a) m) => Instruction a -> m ()
exec (Spin i) = modify (spin i)
exec inst@(Swap i j) = modify (caseIt inst . swapIx i j)
exec inst@(Partner a b) = modify (caseIt inst . swap a b)
        
execAll :: (Eq a, Show a, MonadState (Line a) m) => [Instruction a] -> m ()
execAll = sequence_ . fmap exec

execAllWith :: (Eq a, Show a) => [Instruction a] -> Line a -> Line a
execAllWith = execState . execAll