{-# OPTIONS -fglasgow-exts #-}
module Main where
import System
import Data.Char
import Text.ParserCombinators.Parsec hiding (parse, parseTest)
import qualified Text.ParserCombinators.Parsec as P (parse, parseTest)
import qualified System.IO.UTF8 as U
import Control.Monad.State

data Inst = App Int Int | Abs Int [Inst] deriving Show

data Code = C [Inst] | Succ | Out | Char Char | In  deriving Show
data Func = F (Code, Environment) deriving Show
type Environment = [Func]
type Dump = [Func]


data GrassState = GS {
  code :: Code, env :: Environment, dump :: Dump
}

mkGS c = GS{
  code = C c,
  env  = [F (Out, []),F (Succ, []),F (Char 'w', []),F (In, [])],
  dump = [F(C [App 1 1], []), F(C [], [])]
}

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

trans (GS (C(App m n:c)) e d) = do  let fn = e !! (n-1)
                                        (F (cm, em)) = e !! (m-1)
                                    return $ GS cm (fn:em) ((F(C c, e)):d)

trans (GS (C(Abs 1 c':c)) e d) = return $ GS (C c) ((F (C c',e)):e) d
trans (GS (C(Abs n c':c)) e d) = return $ GS (C c) ((F (C [Abs(n-1)c'],e)):e) d
trans (GS (C[]) (f:_) ((F (c', e')):d)) = return $ GS c' (f:e') d

trans (GS prim e@(F(arg,_):_) d) = do rs <- mproc prim
                                      return $ GS (C []) (F(rs, e):e) d
  where mproc Succ = charP (\k -> return $ Char (chr $ (ord k+1)`mod`256) )
        mproc Out = charP (\k -> U.putStr [k] >> (return arg))
        mproc In  = (getChar >>= (return . Char) )`catch` (\_->return arg)
        mproc (Char k) = charP(\k -> return$Char k)
        
        charP expr = case arg of
                      (Char k) -> expr k
                      _        -> error "argument must be character"

main = do args <- getArgs
          s <- case args of
                  (x:_) -> U.readFile x
                  _     -> U.getContents
          let src = ppp s
              gs = mkGS $ parse prog src
              go gs = do  gs <- (trans gs)
                          case gs of
                            (GS (C[]) [x] []) -> return ()
                            _ -> go gs
          go gs

ppp = concatMap (\c->if c`elem`"wWvｗＷｖ"then [c]else[])