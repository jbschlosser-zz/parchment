{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

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
data St = St {_scrollback :: [[Fchar]], _input :: String, _cursor :: Int}
makeLenses ''Fchar
makeLenses ''St

fcharToMarkup :: Fchar -> Markup V.Attr
fcharToMarkup = \t -> (singleton $ _ch t) @@ (_attr t)

drawScrollbackLine :: [Fchar] -> Widget()
drawScrollbackLine = markup . mconcat . map fcharToMarkup

drawScrollback :: [[Fchar]] -> Widget()
drawScrollback lines = foldr (<=>) (str "") $ map drawScrollbackLine lines

drawUI :: St -> [Widget()]
drawUI (St {_scrollback = sb, _input = input, _cursor = cursor}) =
    [vBox [ drawScrollback sb
          , padTop Max $ hBorder
          , showCursor () (Location (cursor, 0)) (str input)
          ]]

addKey :: St -> Char -> St
addKey st k = (st & input .~ ((_input st) ++ [k])) & cursor .~ ((_cursor st) + 1)

handleEvent :: St -> BrickEvent () e -> EventM () (Next St)
handleEvent st (VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> halt st
        V.EvKey (V.KChar k) [] -> continue $ addKey st k
        _ -> continue st
handleEvent st _ = continue st

app :: App St e ()
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr []
        , appStartEvent = return
        , appChooseCursor = showFirstCursor
        }

testText :: String -> [Fchar]
testText = map (\c -> Fchar {_ch = c, _attr = V.Attr {V.attrStyle = V.Default, V.attrForeColor = V.SetTo V.blue, V.attrBackColor = V.Default}})

initialState =
    St {
        _scrollback = replicate 10 $ testText "hello"
        , _input = " "
        , _cursor = 1
        }

main :: IO ()
main = void $ defaultMain app initialState
