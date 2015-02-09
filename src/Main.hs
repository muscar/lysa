{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Data.List

type Id = Int
type Name = String

data Type = TyVar TypeVar
          | TyRowVar RowVar
          | TyCon Name [Type]
          | TyProd [TypeField]
          deriving Eq

data TypeVar = TypeVar Id
             deriving Eq

data RowVar = RowVar Id [TypeField]
             deriving Eq

data TypeField = TypeField Name Type
               deriving Eq

data LysaState = LysaState { idSupply :: Int }

data Message = MessageInfo String
             | MessageWarning String
             | MessageError String

newtype Lysa a = Lysa { unLysa :: WriterT [Message] (State LysaState) a }
  deriving (Functor, Applicative, Monad, MonadWriter [Message], MonadState LysaState)

-- Types

instance Show Type where
  show (TyVar tv) = show tv
  show (TyRowVar rv) = show rv
  show (TyCon name []) = name
  show (TyCon name tys) = name ++ "[" ++ (intercalate ", " $ map show tys) ++ "]"
  show (TyProd fs) = "{" ++ intercalate "; " (map show fs) ++ "}"

instance Show TypeVar where
  show (TypeVar id) = "t" ++ show id

instance Show RowVar where
  show (RowVar id []) = "r" ++ show id
  show (RowVar id cs) = "r" ++ show id ++ " with " ++ (intercalate " and " $ map show cs)

instance Show TypeField where
  show (TypeField name ty) = name ++ ": " ++ show ty

nextId :: Lysa Id
nextId = do
  id <- gets idSupply
  modify $ \s -> s{ idSupply = idSupply s + 1 }
  return id

freshTv :: Lysa TypeVar
freshTv = liftM TypeVar nextId

freshRv :: [TypeField] -> Lysa RowVar
freshRv cs = liftM (flip RowVar cs) nextId

-- Logging

instance Show Message where
  show (MessageInfo msg) = msg
  show (MessageWarning msg) = "warning: " ++ msg
  show (MessageError msg) = "error: " ++ msg

runLysa :: Lysa a -> (a, [Message])
runLysa l = evalState (runWriterT $ unLysa l) initialState
  where initialState = LysaState 0

report :: Message -> Lysa ()
report msg = tell [msg]

reportInfo :: String -> Lysa ()
reportInfo = report . MessageInfo

reportWarning :: String -> Lysa ()
reportWarning = report . MessageWarning

reportError :: String -> Lysa ()
reportError = report . MessageError

lysa :: Lysa Type
lysa = do
  reportInfo "Lysa v0.1 (c) Alex Muscar 2015\n" 
  reportInfo "Certainty of death, *small* chance of success... What are we waiting for?\n"
  TyRowVar <$> freshRv [TypeField "x" $ TyCon "Int" [], TypeField "y" $ TyCon "Int" []]

main :: IO ()
main = print $ runLysa lysa
