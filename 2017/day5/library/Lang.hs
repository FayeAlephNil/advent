{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lang (runAllSteps, runAllSteps') where

import Data.List.Zipper
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

data OurState = OurState {
    _ourZipper :: Zipper Int,
    _step :: Int
} deriving (Show)

makeFieldsNoPrefix ''OurState

data OurEffect = GoneOver Int

makeClassyPrisms ''OurEffect

newtype OurExec a = OurExec {
    runExec :: ExceptT OurEffect (StateT OurState IO) a
} deriving (Functor,
            Applicative,
            Monad,
            MonadState OurState)

type HasZipperState s a = (HasStep s a, HasOurZipper s (Zipper a))
type AllButIO m r s a = (Enum a, MonadState s m, HasZipperState s a)

getZipper :: (MonadState s m, HasOurZipper s a) => m a
getZipper = fmap (^. ourZipper) get

getStep :: (MonadState s m, HasStep s a) => m a
getStep = fmap (^. step) get

modZipper :: (MonadState s m, HasOurZipper s a) => (a -> a) -> m ()
modZipper f = modify (ourZipper %~ f)

modStep :: (MonadState s m, HasStep s a) => (a -> a) -> m ()
modStep f = modify (step %~ f)

goRight :: (MonadState s m, Enum a, HasOurZipper s (Zipper a)) => m ()
goRight = modZipper right

goLeft :: (MonadState s m, Enum a, HasOurZipper s (Zipper a)) => m ()
goLeft = modZipper left

goRightFor :: (MonadState s m, Enum a, HasZipperState s a) => Int -> m ()
goRightFor n = (sequence_ $ replicate n goRight)

goLeftFor :: (MonadState s m, Enum a, HasZipperState s a) => Int -> m ()
goLeftFor n = (sequence_ $ replicate n goLeft)

jump :: (AllButIO m r s Int) => Int -> m ()
jump i
    | i == 0 = pure ()
    | i > 0 = goRightFor i
    | otherwise = goLeftFor (-i)
    

runStep :: (AllButIO m r s Int, AsOurEffect e) => ExceptT e m ()
runStep = do
    zipper <- getZipper
    stp <- getStep
    case safeCursor zipper of
        Nothing -> throwError (_GoneOver # stp)
        (Just a) -> do
            modZipper (replace (succ a))
            modStep succ
            jump a

runStep' :: (AllButIO m r s Int, AsOurEffect e) => ExceptT e m ()
runStep' = do
    zipper <- getZipper
    stp <- getStep
    case safeCursor zipper of
        Nothing -> throwError (_GoneOver # stp)
        (Just a) -> do
            modZipper . replace $ if (a >= 3) then pred a else succ a 
            modStep succ
            jump a

allSteps :: (AllButIO m r s Int, AsOurEffect e) => ExceptT e m () -> ExceptT e m ()
allSteps stepper = do
    result <- lift $ runExceptT stepper
    case result of
        (Left e) -> throwError e
        (Right a) -> pure a >> allSteps stepper
    

initialize :: [Int] ->  OurState
initialize as = OurState {
    _ourZipper = fromList as,
    _step = 0
}

runGeneral :: (AllButIO m r s Int, MonadIO m) => ExceptT OurEffect m a -> m ()
runGeneral except = do
    unexcept <- runExceptT except
    case unexcept of
        Left (GoneOver i) -> (liftIO . putStrLn $ "Exited at step " ++ show i)
        Right _ -> (liftIO . putStrLn $ "Never exited")

runStack :: [Int] -> OurExec a -> IO OurState
runStack as (OurExec stack) = execStateT (runGeneral stack) (initialize as)

runAllSteps :: [Int] -> IO ()
runAllSteps as = runStack as (OurExec $ allSteps runStep) >> pure ()

runAllSteps' :: [Int] -> IO ()
runAllSteps' as = runStack as (OurExec $ allSteps runStep') >> pure ()