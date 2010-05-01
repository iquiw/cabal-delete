module CabalDelete.Utils
    ( alignDList
    , askIf
    , msg
    , msg'
    ) where

import Control.Monad.Trans
import System.IO

alignDList :: Int -> String -> [String] -> String
alignDList maxlen dt dds = unlines (map unwords $ align m dds [dt])
  where
    k = length dt
    sp = replicate k ' '
    m = maxlen - k

    align _ [] ys         = [reverse ys]
    align n (x:xs) ys =
        let l = length x
        in case () of
            _ | l > m     -> if null ys
                             then [x] : align m xs [sp]
                             else reverse ys : align (m-l) xs [x, sp]
              | l > n     -> reverse ys : align (m-l) xs [x, sp]
              | otherwise -> align (n-l) xs (x:ys)

askIf :: (MonadIO m) => String -> m a -> m a -> m a
askIf s thenProc elseProc = do
    liftIO $ putStr s
    liftIO $ hFlush stdout
    ans <- liftIO getLine
    if ans `elem` ["", "y", "Y"]
        then thenProc
        else elseProc

msg :: (MonadIO m) => String -> m ()
msg = liftIO . putStrLn

msg' :: (MonadIO m) => String -> m ()
msg' = liftIO . putStr
