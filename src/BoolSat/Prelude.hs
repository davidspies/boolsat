module BoolSat.Prelude
  ( (<&>)
  , module X
  )
where

import           Prelude                       as X
                                         hiding ( (!!)
                                                , head
                                                , tail
                                                )

import           Control.Applicative           as X
import           Control.Monad.Extra           as X
                                                ( mapMaybeM )
import           Control.Monad                 as X
import           Data.Bifunctor                as X
                                                ( first
                                                , second
                                                )
import           Data.List                     as X
                                         hiding ( (!!)
                                                , head
                                                , tail
                                                )
import           Data.Map                      as X
                                                ( Map )
import           Data.Maybe                    as X
                                         hiding ( fromJust )
import           Data.Proxy                    as X
                                                ( Proxy(Proxy) )
import           Data.Set                      as X
                                                ( Set )

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
