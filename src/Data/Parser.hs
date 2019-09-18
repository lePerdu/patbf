{-# LANGUAGE LambdaCase #-}

module Data.Parser
    ( Parser
    , runParser
    , runParser'
    , satisfy
    , token
    , getToken
    )
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail

import           Control.Arrow                  ( first )

type Result = Either String

newtype Parser t a = Parser { unP :: [t] -> (Result a, [t]) }

success a ts = (Right a, ts)
failure msg ts = (Left msg, ts)

runParser :: Parser t a -> [t] -> (Result a, [t])
runParser = unP

runParser' :: Parser t a -> [t] -> Result a
runParser' p = fst . runParser p

instance Functor (Parser t) where
    fmap f (Parser p) = Parser (first (fmap f) . p)

instance Applicative (Parser t) where
    pure a = Parser (success a)

    Parser pf <*> Parser pa = Parser $ \ts -> case pf ts of
        (Right f, ts') -> case pa ts' of
            (Right a, ts'') -> (Right (f a), ts'')
            (Left  e, ts'') -> (Left e, ts'')
        (Left e, ts') -> (Left e, ts')

instance Alternative (Parser t) where
    empty = Parser (failure "empty")

    Parser pa <|> Parser pb = Parser $ \ts -> case pa ts of
        (Right a, ts') -> (Right a, ts')
        _ -> pb ts

instance Monad (Parser t) where
    Parser pa >>= f = Parser $ \ts -> case pa ts of
        (Right a, ts') -> runParser (f a) ts'
        (Left  e, ts') -> (Left e, ts')

    fail msg = Parser (failure msg)

instance MonadPlus (Parser t) where

instance MonadFail (Parser t) where
    fail msg = Parser (failure msg)

instance Semigroup a => Semigroup (Parser t a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Parser t a) where
    mempty = Parser (success mempty)

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \case
    [] -> failure "end of stream" []
    ts@(t : tt) ->
        if p t then (Right t, tt) else failure "failed to satisfy" ts

token :: Eq t => t -> Parser t t
token = satisfy . (==)

getToken :: Parser t t
getToken = satisfy (const True)

