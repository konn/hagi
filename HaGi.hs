{-# OPTIONS -fglasgow-exts #-}
module Main where
import System
import Data.Char
import Text.ParserCombinators.Parsec hiding (parse, parseTest)
import qualified Text.ParserCombinators.Parsec as P (parse, parseTest)
import qualified System.IO.UTF8 as U
import Control.Monad.State
import Control.Monad.Trans

data Inst = App Int Int | Abs Int [Inst] deriving Show

data Code = C [Inst] | Succ | Out | Char Char | In  deriving Show
data Func = F (Code, Environment) deriving Show
type Environment = [Func]
type Dump = [Func]
type GState m a = StateT GrassState m a

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

letter c = skipMany$noneOf "wWvｗＷｖ" >> char letter

small = letter 'w' <|> (letter 'ｗ' <?> "\"ｗ\"")
big   = letter 'W' <|> (letter 'Ｗ' <?> "\"Ｗ\"")
v     = letter 'v' <|> (letter 'ｖ' <?> "\"ｖ\"")

abst = do args <- many1 small
          body <- many appl
          return $ Abs (length args) body

appl = do func <- many1 big
          arg  <- many1 small
          return $ App (length func) (length arg)
prog = do skipMany$noneOf "w"
          head <- abst
          rest <- option [] (v >> body)
          eof
          return (head:rest)

body = do rest <- (try(count 1 abst)<|>try(many appl))`sepBy`v
          return $ concat rest

evaluate = do gs <- get
              case gs of
                (GS (C []) [x] []) -> return [x]
                _ -> trans >> evaluate

-- trans :: GState IO ()
trans = do  gs <- get
            case gs of
              (GS (C(App m n:c)) e d) -> do let fn = e !! (n-1)
                                                (F (cm, em)) = e !! (m-1)
                                            put (GS cm (fn:em) ((F(C c, e)):d))
              (GS (C(Abs 1 c':c)) e d) -> put (GS (C c) ((F (C c',e)):e) d)
              (GS (C(Abs n c':c)) e d) -> put(GS (C c) ((F (C [Abs(n-1)c'],e)):e) d)
              (GS (C[]) (f:_) ((F (c', e')):d)) -> put(GS c' (f:e') d)
              (GS prim e@(F(arg,_):_) d) -> mproc prim arg e d
  where mproc :: Code -> Code -> Environment -> Dump -> GState IO ()
        mproc Succ = charP (\k -> return $ Char (chr $ (ord k+1)`mod`256) )
        mproc Out = charP (\k -> (liftIO$U.putStr [k])>>(return$Char k))
        mproc In  = charP (\k -> ((liftIO getChar) >>= (return.Char)))
        mproc (Char k) = (\(Char c) e d -> put ( GS (C [Abs 1 [], Abs 2 [App 3 (if k == c then 2 else 1)]]) e d))
        charP expr arg e d = case arg of
                      (Char k) -> do{t<-expr k;put $ GS (C []) (F (t, e):e) d}
                      _        -> error "argument must be character"

main = do args <- getArgs
          s <- case args of
                  (x:_) -> U.readFile x
                  _     -> U.getContents
          execStateT evaluate (mkGS$parse prog s)

 concatMap (\c->if c`elem`"wWvｗＷｖ"then [c]else[])