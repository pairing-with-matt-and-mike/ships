module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.List (findIndex, index, deleteAt, reverse, List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd, Tuple(..))

data Day = Monday | Tuesday | Wednesday | Thursday | Friday
derive instance genericDay :: Generic Day _
instance showDay :: Show Day where
  show = genericShow
instance eqDay :: Eq Day where
  eq = genericEq

type Leg = { from :: String, to :: String, day :: Day }

legs :: List Leg
legs = ({from: "PDX", to: "SEA", day: Monday} :
        {from: "PDX", to: "SFO", day: Tuesday} :
        {from: "SEA", to: "DEN", day: Tuesday} :
        {from: "DEN", to: "PDX", day: Wednesday} :
        {from: "PDX", to: "DEN", day: Thursday} :
        {from: "DEN", to: "JFK", day: Friday} : Nil)

succ :: Day -> Maybe Day
succ Monday = Just Tuesday
succ Tuesday = Just Wednesday
succ Wednesday = Just Thursday
succ Thursday = Just Friday
succ Friday = Nothing

joinable :: Leg -> Leg -> Boolean
joinable la lb = la.to == lb.from && succ la.day == (Just lb.day)

type Route = List Leg

pickBy :: forall a. (a -> Boolean) -> List a -> Maybe (Tuple a (List a))
pickBy p as = do i <- findIndex p as
                 e <- index as i
                 as' <- deleteAt i as
                 pure $ Tuple e as'

findRoute :: List Leg -> Tuple (List Leg) Route
findRoute Nil = Tuple Nil Nil
findRoute (l:ls) = go ls l Nil
  where go ls' r rs =
          case pickBy (joinable r) ls' of
            Just (Tuple next ls'') -> go ls'' next (r:rs)
            Nothing -> Tuple ls' $ reverse (r:rs)

findRoutes :: List Leg -> List Route
findRoutes ls = go ls Nil
  where go Nil rs = rs
        go ls rs = let Tuple ls' r = findRoute ls
                       in go ls' (r : rs)

main :: Effect Unit
main = do
  log $ "You should add some tests." <> show legs
  log $ "Route " <> (show $ snd $ findRoute legs)
  log $ "Routes " <> (show $ findRoutes legs)
--  log $ "Route" <> (show $ pickBy (\x -> x == 2) $ 1 : 2 : 3 : Nil)
  log $
    (show $ joinable leg1 leg2) <> " " <>
    (show $ joinable leg1 leg3)
    where leg1 = {from: "DEN", to: "PDX", day: Wednesday}
          leg2 = {from: "PDX", to: "DEN", day: Thursday}
          leg3 = {from: "PDX", to: "DEN", day: Friday}
