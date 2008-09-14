{-# OPTIONS -fglasgow-exts #-}
module Main where
import System
import Data.Char
import Text.ParserCombinators.Parsec hiding (parse, parseTest)
import qualified Text.ParserCombinators.Parsec as P (parse, parseTest)
import qualified System.IO.UTF8 as U
import Control.Monad.State

data Inst = App Int Int | Abs Int [Inst] deriving Show

type Code = [Inst]
data Func = F (Code, Environment) | Succ | Out | Char Char | In  deriving Show
type Environment = [Func]
type DStack = [Func]


data GrassState = GS {
  code :: Code, env :: Environment, dstack :: DStack
}

mkGS c = GS c [Out,Succ,Char 'w',In] [F ([App 1 1],[]), F([],[])]

instance Show GrassState where
  show (GS a b c) = show (a, b, c)

parse p src = case runParser  prog () "" src of
                Left  err -> error(show err)
                Right rsl -> rsl

small = char 'w' <|> (char 'ｗ' <?> "\"ｗ\"")
big   = char 'W' <|> (char 'Ｗ' <?> "\"Ｗ\"")
v     = char 'v' <|> (char 'ｖ' <?> "\"ｖ\"")

abst = do args <- many1 small
          body <- many appl
          return $ Abs (length args) body

appl = do func <- many1 big
          arg  <- many1 small
          return $ App (length func) (length arg)

prog =try(do
          r <- abst
          eof
          return [r])
  <|>  do head <- abst
          v
          rest <- (try(abst) <|> try(appl)) `sepBy` v
          eof
          return (head:rest)

trans (GS (App m n:c) e d) = do let fn = e !! (n-1)
                                    fm = e !! (m-1)
                                case fm of
                                  (F (cm, em)) -> return $ GS cm (fn:em) (F (c, e):d)
                                  _            -> do  f <- (mproc fm fn)
                                                      return $ GS c (f:e) (F (c, e):d)
  where mproc Succ (Char k)  = return $ Char (chr $ (ord k+1)`mod`256)
        mproc Succ _         = error "Not char"
        mproc Out c@(Char k) = U.putStr [k] >> (return$Char k)
        mproc Out _         = error "Not char"
        mproc In  c@(Char k) = (getChar >>= (return . Char))-- `catch` (return c)
        mproc In _         = error "Not char"
        mproc (Char k) (Char v) = return$Char k

trans (GS (Abs 1 c':c) e d) = return $ GS c (F (c',e):e) d
trans (GS (Abs n c':c) e d) = return $ GS c (F ([Abs(n-1)c'],e):e) d
trans (GS [] (f:_) (F (c', e'):d)) = return $ GS c' (f:e') d

main = do (x:_) <- getArgs
          src <- U.readFile x
          let gs = mkGS $ parse prog src
              go gs = do  gs <- (trans gs)
                          case gs of
                            (GS [] [x] []) -> return ()
                            _ -> go gs
          go gs