{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.Map.Lazy as Map
import qualified Graphics.Vty as V
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Main
    ( App(..)
    , defaultMain
    , resizeOrQuit
    , neverShowCursor
    , showFirstCursor
    , halt
    , continue
    , suspendAndResume
    )
import Brick.Markup (markup, (@?))
import Brick.Types
  ( Widget
  , Padding(..)
  , Location(..)
  , Next
  , EventM
  , BrickEvent(..)
  )
import Brick.Util (on, fg)
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , padLeft
  , padTop
  , str
  , vBox
  , hBox
  , showCursor
  )
import Brick.Widgets.Border
  ( vBorder
  , hBorder
  )
import Control.Monad (void, forever)
import Data.List
import Data.Monoid ((<>))
import Data.Text (singleton)
import Data.Text.Markup ((@@), Markup)
import Lens.Micro ((.~), (^.), (&), (%~), over)
import Lens.Micro.TH (makeLenses)

data Fchar = Fchar {_ch :: Char, _attr :: V.Attr}
data St = St {
    _scrollback :: [[Fchar]]
    , _input :: String
    , _cursor :: Int
    , _bindings :: Map.Map V.Event (St -> EventM () (Next St))
    }
makeLenses ''Fchar
makeLenses ''St

drawScrollback :: [[Fchar]] -> Widget()
drawScrollback lines = foldr (<=>) (str "") $ map drawScrollbackLine lines
    where drawScrollbackLine = markup . mconcat . map fcharToMarkup
          fcharToMarkup = \t -> (singleton $ _ch t) @@ (_attr t)

drawUI :: St -> [Widget()]
drawUI (St {_scrollback = sb, _input = input, _cursor = cursor}) =
    [vBox [ drawScrollback sb
          , padTop Max $ hBorder
          , showCursor () (Location (cursor, 0))
              (if length input > 0 then str input else str " ")
          ]]

createBindings :: [(V.Event, (St -> EventM () (Next St)))] ->
    Map.Map V.Event (St -> EventM () (Next St))
createBindings = (mconcat . map createBinding)
    where createBinding (e, b) = Map.insert e b Map.empty

handleEvent :: St -> BrickEvent () e -> EventM () (Next St)
handleEvent st (VtyEvent e) =
    case Map.lookup e (_bindings st) of
        Just b -> b st
        Nothing -> continue st
handleEvent st _ = continue st

testText :: String -> [Fchar]
testText = map (\c -> Fchar {_ch = c, _attr = V.Attr {V.attrStyle = V.Default, V.attrForeColor = V.SetTo V.blue, V.attrBackColor = V.Default}})

addKey :: St -> Char -> St
addKey st k = (st & input .~ ((_input st) ++ [k])) & cursor .~ ((_cursor st) + 1)

delKey :: St -> St
delKey st =
    if length (_input st) == 0 then
        st
    else
        (st & input .~ (init (_input st))) & cursor .~ ((_cursor st) - 1)

rawKeyBinding :: Char -> (V.Event, (St -> EventM () (Next St)))
rawKeyBinding c = ((V.EvKey (V.KChar c) []), \st -> continue $ addKey st c)

rawKeys :: String
rawKeys = "abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-=_+[]\\;',./{}|:\"<>? `~"

app :: App St e ()
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr []
        , appStartEvent = return
        , appChooseCursor = showFirstCursor
        }

initialState =
    St {
        _scrollback = replicate 10 $ testText "hello"
        , _input = ""
        , _cursor = 0
        , _bindings = createBindings
            ([ ((V.EvKey V.KEsc []), \st -> halt st)
            , ((V.EvKey V.KBS []), \st -> continue $ delKey st)
            ] ++ map rawKeyBinding rawKeys)
        }

main :: IO ()
main = void $ defaultMain app initialState
