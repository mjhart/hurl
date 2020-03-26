{-# LANGUAGE OverloadedStrings #-}

module MyLib
  ( someFunc,
  )
where

import Brick
import Brick.AttrMap
import Brick.Main
import qualified Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Graphics.Vty.Attributes

data Header
  = Header
      { _name :: T.Text,
        _value :: T.Text
      }
  deriving (Show)

data Focus
  = Top
  | Method
  | Url
  | FocusHeadersBrowser
  | FocusHeaderNameEditor
  | FocusHeaderValueEditor
  | FocusBodyEditor
  deriving (Eq, Show)

data Headers = Headers (List Name Header) (Editor T.Text Name) deriving (Show)

data Hurl
  = Hurl
      { _focus :: Focus,
        _method :: Editor T.Text Name,
        _url :: Editor T.Text Name,
        _headers :: Headers,
        _body :: Editor T.Text Name
      }
  deriving (Show)

drawInstructionLine :: Focus -> Widget Name
drawInstructionLine Top = txt "h: headers, b: body, q: quit"
drawInstructionLine Method = txt "enter: leave"
drawInstructionLine Url = txt "enter: leave"
drawInstructionLine FocusHeadersBrowser = txt "enter: leave"
drawInstructionLine FocusHeaderNameEditor = txt "enter: leave"
drawInstructionLine FocusHeaderValueEditor = txt "enter: leave"
drawInstructionLine FocusBodyEditor = txt "enter: leave"

drawHeader :: Focus -> (Editor T.Text Name) -> Bool -> Header -> Widget Name
drawHeader FocusHeadersBrowser _ True (Header name value) =
  hBox
    [ txt ">",
      txt name,
      txt ": ",
      txt value
    ]
drawHeader FocusHeadersBrowser _ False (Header name value) =
  hBox
    [ txt name,
      txt ": ",
      txt value
    ]
drawHeader FocusHeaderValueEditor _ False (Header name value) =
  hBox
    [ txt name,
      txt ": ",
      txt value
    ]
drawHeader FocusHeaderNameEditor editor True (Header name value) =
  hBox
    [ renderEditor (txt . head) True editor,
      txt ": ",
      txt value
    ]
drawHeader FocusHeaderValueEditor editor True (Header name value) =
  hBox
    [ txt name,
      txt ": ",
      renderEditor (txt . head) True editor
    ]
drawHeader _ _ _ (Header name value) =
  hBox
    [ txt name,
      txt ": ",
      txt value
    ]

draw (Hurl focus method url (Headers headers editor) body) =
  [ vBox
      [ hBox
          [ hLimit (textWidth . head $ getEditContents method) $ renderEditor (txt . head) (focus == Method) method,
            txt " ",
            renderEditor (txt . head) (focus == Url) url
          ],
        renderList (drawHeader focus editor) (focus == FocusHeadersBrowser || focus == FocusHeaderValueEditor) headers,
        renderEditor (txt . head) (focus == FocusBodyEditor) body,
        drawInstructionLine focus
      ]
  ]

type Event = ()

data Name = Name | MethodEditor | UrlEditor | HeadersEditor | BodyEditor deriving (Eq, Ord, Show)

initialHurl :: Hurl
initialHurl =
  let method = editorText MethodEditor (Just 1) "GET"
      url = editorText UrlEditor (Just 1) ""
      headers = Headers (list Name (V.fromList [Header "foo" "bar"]) 1) (editorText HeadersEditor (Just 1) "")
      body = editorText BodyEditor (Just 1) ""
   in Hurl Url method url headers body

handleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
handleEvent s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
handleEvent hurl@(Hurl _ _ _ _ _) (VtyEvent (Vty.EvKey Vty.KEnter [])) = continue $ hurl {_focus = Top}
handleEvent hurl@(Hurl Top _ _ _ _) (VtyEvent (Vty.EvKey (Vty.KChar 'h') [])) = continue $ hurl {_focus = FocusHeadersBrowser}
handleEvent hurl@(Hurl Top _ _ _ _) (VtyEvent (Vty.EvKey (Vty.KChar 'm') [])) = continue $ hurl {_focus = Method}
handleEvent hurl@(Hurl Top _ _ _ _) (VtyEvent (Vty.EvKey (Vty.KChar 'u') [])) = continue $ hurl {_focus = Url}
handleEvent hurl@(Hurl Top _ _ _ _) (VtyEvent (Vty.EvKey (Vty.KChar 'b') [])) = continue $ hurl {_focus = FocusBodyEditor}
handleEvent hurl@(Hurl Url _ url _ _) (VtyEvent e) = handleEditorEvent e url >>= \newEditor -> continue $ hurl {_url = newEditor}
handleEvent hurl@(Hurl Method method _ _ _) (VtyEvent e) = handleEditorEvent e method >>= \newEditor -> continue $ hurl {_method = newEditor}
handleEvent hurl@(Hurl FocusHeadersBrowser _ _ (Headers headers editor) _) (VtyEvent (Vty.EvKey (Vty.KChar 'o') [])) =
  continue $
    hurl
      { _focus = FocusHeaderNameEditor,
        _headers =
          let newIndex = (fst . fromJust $ listSelectedElement headers) + 1
              newEditor = editorText HeadersEditor (Just 1) ""
           in Headers (listMoveTo newIndex . listInsert newIndex (Header "" "") $ headers) newEditor
      }
handleEvent hurl@(Hurl FocusHeadersBrowser _ _ (Headers headers editor) _) (VtyEvent (Vty.EvKey (Vty.KChar 'd') [])) =
  continue $ hurl {_headers = Headers (listRemove (fromJust $ listSelected headers) headers) editor}
handleEvent hurl@(Hurl FocusHeadersBrowser _ _ (Headers headers editor) _) (VtyEvent e) =
  handleListEvent e headers >>= \newHeaders -> continue $ hurl {_headers = Headers newHeaders editor}

handleEvent hurl@(Hurl FocusHeaderNameEditor _ _ (Headers headers editor) _) (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) =
  continue $ hurl {
    _focus = FocusHeaderValueEditor,
    _headers = Headers headers $ editorText HeadersEditor (Just 1) (_value (snd . fromJust $ listSelectedElement headers))
  }
handleEvent hurl@(Hurl FocusHeaderNameEditor _ _ (Headers headers editor) _) (VtyEvent e) =
  handleEditorEvent e editor >>= \newEditor ->
    continue $
      hurl
        {
          _headers = Headers (listModify (\header -> header {_name = head $ getEditContents newEditor}) headers) newEditor
        }
handleEvent hurl@(Hurl FocusHeaderValueEditor _ _ (Headers headers editor) _) (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) =
  continue $ hurl {
    _focus = FocusHeadersBrowser
  }
handleEvent hurl@(Hurl FocusHeaderValueEditor _ _ (Headers headers editor) _) (VtyEvent e) =
  handleEditorEvent e editor >>= \newEditor ->
    continue $
      hurl
        {
          _headers = Headers (listModify (\header -> header {_value = head $ getEditContents newEditor}) headers) newEditor
        }
handleEvent hurl@(Hurl FocusBodyEditor _ _ _ body) (VtyEvent e) =
  handleEditorEvent e body >>= \newEditor ->
    continue $
      hurl
        {
          _body = newEditor
        }
handleEvent s e = continue s

chooseCursor :: Hurl -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor (Hurl Method _ _ _ _) = showCursorNamed MethodEditor
chooseCursor (Hurl Url _ _ _ _) = showCursorNamed UrlEditor
chooseCursor (Hurl FocusHeaderNameEditor _ _ _ _) = showCursorNamed HeadersEditor
chooseCursor (Hurl FocusHeaderValueEditor _ _ _ _) = showCursorNamed HeadersEditor
chooseCursor (Hurl FocusBodyEditor _ _ _ _) = showCursorNamed BodyEditor
chooseCursor _ = const Nothing

someFunc :: IO ()
someFunc = do
  let app :: App Hurl Event Name
      app =
        App
          { appDraw = draw,
            appChooseCursor = chooseCursor,
            appHandleEvent = handleEvent,
            appStartEvent = return,
            appAttrMap = const $ attrMap defAttr []
          }
      initialState = initialHurl
  defaultMain app initialState >> return ()
