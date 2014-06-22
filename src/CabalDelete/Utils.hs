module CabalDelete.Utils
    ( alignDList
    , askIf
    , chomp
    , msg
    , msg'
    ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM2)
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

    align _ [] [_]    = []
    align _ [] ys     = [reverse ys]
    align n (x:xs) ys =
        let l = length x + 1
        in case () of
            _ | l > m     -> case ys of
                []  -> [x] : align m xs [sp]
                [y] -> [y, x] : align m xs [sp]
                _   -> reverse ys : align (m-l) xs [x, sp]
              | l > n     -> reverse ys : align (m-l) xs [x, sp]
              | otherwise -> align (n-l) xs (x:ys)

askIf :: (Functor m, MonadIO m) => String -> CDM m a -> CDM m a -> CDM m a
askIf s thenProc elseProc = do
    y <- cdYesToAll <$> get
    if y then liftIO (query >> putStrLn "A") >> thenProc else qandgo
  where
    qandgo = do
        ans <- liftIO getAns
        case ans of
            "y" -> thenProc
            "n" -> elseProc
            "a" -> modify (\x -> x { cdYesToAll = True }) >> thenProc
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

chomp :: String -> String
chomp = takeWhile $ liftM2 (&&) (/= '\r') (/= '\n')
