module CabalDelete.Utils
    ( alignDList
    , askIf
    , msg
    , msg'
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(), liftIO)
import Control.Monad.Trans.State (get, modify)
import Data.Char (toLower)
import System.IO (hFlush, stdout)

import CabalDelete.Types

alignDList :: Int -> String -> [String] -> String
alignDList maxlen dt dds = unlines (map unwords $ align m dds [dt])
  where
    k = length dt
    sp = replicate k ' '
    m = maxlen - k

    align _ [] ys         = [reverse ys]
    align n (x:xs) ys =
        let l = length x + 1
        in case () of
            _ | l > m     -> if null ys
                             then [x] : align m xs [sp]
                             else reverse ys : align (m-l) xs [x, sp]
              | l > n     -> reverse ys : align (m-l) xs [x, sp]
              | otherwise -> align (n-l) xs (x:ys)

askIf :: (Functor m, MonadIO m) => String -> CDM m a -> CDM m a -> CDM m a
askIf s thenProc elseProc = do
    y <- yesToAll <$> get
    if y then liftIO (query >> putStrLn "A") >> thenProc else qandgo
  where
    qandgo = do
        ans <- liftIO getAns
        case ans of
            "y" -> thenProc
            "n" -> elseProc
            "a" -> modify (\x -> x { yesToAll = True }) >> thenProc
            _   -> qandgo

    query = do
        putStr s
        putStr " [Y]es, [N]o, [A]ll: "
        hFlush stdout

    getAns = query >> map toLower <$> getLine

msg :: (MonadIO m) => String -> m ()
msg = liftIO . putStrLn

msg' :: (MonadIO m) => String -> m ()
msg' = liftIO . putStr
