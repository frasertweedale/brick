{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.), over, both, set)
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Data.List
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

newtype L a = L [a]
  deriving (Functor, Foldable, Traversable)

instance L.Splittable L where
  splitAt i (L xs) = over both L (Data.List.splitAt i xs)

drawUI :: (Show a) => (chan, L.GenericList () L a) -> [Widget ()]
drawUI (_, l) = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.L.listSelectedL of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str "?"
        box = B.borderWithLabel label $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press Esc to exit."
                              ]

appEvent
  :: (BChan (), L.GenericList () L Int)
  -> T.BrickEvent () e
  -> T.EventM () (T.Next (BChan (), L.GenericList () L Int))
appEvent s@(chan, l) ev = case ev of
  T.VtyEvent (V.EvKey V.KEsc []) -> M.halt s
  T.VtyEvent vtyEv -> M.continue . (chan,) . pruneList =<< L.handleListEvent vtyEv l
  _ -> M.continue s

pruneList
  :: (L.Splittable t)
  => L.GenericList n t a -> L.GenericList n t a
pruneList l =
  case l ^. L.listSelectedL of
    Nothing -> l
    Just i ->
      let
        i' = max 0 (i - 999)
      in
        ($l) $
          over L.listElementsL (snd . L.splitAt i')
          . set L.listSelectedL (Just (i - i'))

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> selStr (show a)

initialState :: L.GenericList () L Int
initialState = L.list () (L (take 10000000 [0..])) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (BChan (), L.GenericList () L Int) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 32
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ M.customMain initialVty buildVty (Just chan) theApp (chan, initialState)
