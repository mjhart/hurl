{-# LANGUAGE OverloadedStrings #-}

module Hurl
  ( runHurl,
  )
where

import Brick
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Center (vCenter)
import Brick.Widgets.Edit
  ( Editor,
    editorText,
    getEditContents,
    handleEditorEvent,
    renderEditor,
  )
import Brick.Widgets.List
import Control.Monad.Catch (catch)
import qualified Data.ByteString.Lazy as B
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Graphics.Vty.Attributes
import qualified Network.HTTP.Conduit as Http
import qualified Network.HTTP.Simple as Http

type Event = ()

data Header
  = Header
      { _name :: T.Text,
        _value :: T.Text
      }
  deriving (Show)

data Name
  = Name
  | MethodEditor
  | UrlEditor
  | HeadersEditor
  | BodyEditor
  | ResponseList
  deriving (Eq, Ord, Show)

data Focus
  = Top
  | Method
  | Url
  | FocusHeadersBrowser
  | FocusHeaderNameEditor
  | FocusHeaderValueEditor
  | FocusBodyEditor
  | FocusResponse
  deriving (Eq, Show)

data Headers = Headers (List Name Header) (Editor T.Text Name) deriving (Show)

data Response = NoResponse | Failed T.Text | Success (List Name T.Text) deriving (Show)

data Hurl
  = Hurl
      { _focus :: Focus,
        _method :: Editor T.Text Name,
        _url :: Editor T.Text Name,
        _headers :: Headers,
        _body :: Editor T.Text Name,
        _response :: Response
      }
  deriving (Show)

getHeaderEntries :: Headers -> [Header]
getHeaderEntries (Headers entries _) = V.toList . listElements $ entries

drawInstructionLine :: Focus -> Widget Name
drawInstructionLine Top = txt "m: method, u: url, h: headers, b: body, enter: make request, q: quit"
drawInstructionLine Method = txt "esc/enter: back"
drawInstructionLine Url = txt "esc/enter: back"
drawInstructionLine FocusHeadersBrowser = txt "j: down, k: up, d: remove, o: add, enter: back"
drawInstructionLine FocusHeaderNameEditor = txt "esc/enter: back"
drawInstructionLine FocusHeaderValueEditor = txt "esc/enter: back"
drawInstructionLine FocusBodyEditor = txt "esc: back"
drawInstructionLine FocusResponse = txt "esc: back"


drawHeader :: Focus -> Editor T.Text Name -> Bool -> Header -> Widget Name
drawHeader FocusHeadersBrowser _ focused (Header name value) =
  let headerRow =
        hBox
          [ txt name,
            txt ": ",
            txt value
          ]
   in if focused then showCursor HeadersEditor (Location (0, 0)) headerRow else headerRow
drawHeader FocusHeaderValueEditor _ False (Header name value) =
  hBox
    [ txt name,
      txt ": ",
      txt value
    ]
drawHeader FocusHeaderNameEditor editor True (Header name value) =
  hBox
    [ editorHLimit editor $ renderEditor (txt . T.unlines) True editor,
      txt ": ",
      txt value
    ]
drawHeader FocusHeaderValueEditor editor True (Header name value) =
  hBox
    [ txt name,
      txt ": ",
      renderEditor (txt . T.unlines) True editor
    ]
drawHeader _ _ _ (Header name value) =
  hBox
    [ txt name,
      txt ": ",
      txt value
    ]

editorHLimit :: Editor T.Text n -> Widget n' -> Widget n'
editorHLimit editor = hLimit $ maximum (textWidth <$> getEditContents editor) + 1

drawMain :: Hurl -> Widget Name
drawMain (Hurl focus method url (Headers headers editor) body NoResponse) =
   vBox
      [ hBox
        [ editorHLimit method $
            renderEditor (txt . T.unlines) (focus == Method) method,
          renderEditor (txt . T.unlines) (focus == Url) url
        ],
        vLimit (V.length (listElements headers) + 1) $
          renderList
            (drawHeader focus editor)
            (focus == FocusHeadersBrowser || focus == FocusHeaderValueEditor)
            headers,
        renderEditor
          (txt . T.unlines)
          (focus == FocusBodyEditor)
          body
      ]
drawMain (Hurl _ _ _ _ _ (Success response)) = renderList (const txt) False response
drawMain (Hurl _ _ _ _ _ (Failed message)) = vCenter $ txt message

draw :: Hurl -> [Widget Name]
draw hurl@Hurl{_focus = focus} = [
  vBox [
    txt "hurl 0.0.1",
    drawMain hurl,
    hBorder,
    drawInstructionLine focus
  ]]

initialHurl :: Hurl
initialHurl =
  let method = editorText MethodEditor (Just 1) "GET"
      url = editorText UrlEditor (Just 1) ""
      headers =
        Headers
          (list Name (V.fromList []) 1)
          (editorText HeadersEditor (Just 1) "")
      body = editorText BodyEditor Nothing ""
      response = NoResponse
   in Hurl Url method url headers body response

makeRequestHeader :: Header -> Http.Header
makeRequestHeader (Header name value) =
  let requestName = CI.mk $ encodeUtf8 name
      requestValue = encodeUtf8 value
   in (requestName, requestValue)

makeRequest :: Hurl -> EventM Name (Next Hurl)
makeRequest hurl@(Hurl _ method url headers body _) =
  do
    let urlString = T.unpack . head . getEditContents $ url
        methodString = encodeUtf8 . head . getEditContents $ method
        requestHeaders = makeRequestHeader <$> getHeaderEntries headers
        requestBody = B.fromStrict . encodeUtf8 . mconcat . getEditContents $ body
    let parsedRequest = Http.parseRequest urlString
    case parsedRequest of
      Right baseRequest -> do
        let request =
              Http.setRequestMethod methodString
                . Http.setRequestHeaders requestHeaders
                . Http.setRequestBodyLBS requestBody
                $ baseRequest
            mkRequest :: EventM Name (Next Hurl)
            mkRequest = do
              response <- Http.httpLBS request
              continue $
                hurl
                  { _focus = FocusResponse,
                    _response =
                      Success
                        . (\bodyLines -> list ResponseList bodyLines 1)
                        . V.fromList
                        . T.lines
                        . decodeUtf8
                        . B.toStrict
                        . Http.getResponseBody
                        $ response
                  }
        mkRequest `catch` \e -> continue $ hurl {_response = Failed $ showHttpExceptionFriendly (e :: Http.HttpException)}
      Left e -> continue $ hurl {_focus = FocusResponse, _response = Failed "Malformed URL"}

topHandleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
topHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = continue $ hurl {_focus = Method}
topHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar 'h') [])) = continue $ hurl {_focus = FocusHeadersBrowser}
topHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar 'm') [])) = continue $ hurl {_focus = Method}
topHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar 'u') [])) = continue $ hurl {_focus = Url}
topHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar 'b') [])) = continue $ hurl {_focus = FocusBodyEditor}
topHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt hurl
topHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEnter [])) = makeRequest hurl
topHandleEvent hurl _ = continue hurl

methodHandleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
methodHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEsc [])) = continue $ hurl {_focus = Top}
methodHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = continue $ hurl {_focus = Url}
methodHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEnter [])) = continue $ hurl {_focus = Top}
methodHandleEvent hurl@Hurl {_method = method} (VtyEvent e) =
  handleEditorEvent e method >>= \newEditor -> continue $ hurl {_method = newEditor}
methodHandleEvent hurl _ = continue hurl

urlHandleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
urlHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEsc [])) = continue $ hurl {_focus = Top}
urlHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = continue $ hurl {_focus = FocusHeadersBrowser}
urlHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEnter [])) = continue $ hurl {_focus = Top}
urlHandleEvent hurl@Hurl {_url = url} (VtyEvent e) =
  handleEditorEvent e url >>= \newEditor -> continue $ hurl {_url = newEditor}
urlHandleEvent hurl _ = continue hurl

headersBrowserHandleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
headersBrowserHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEsc [])) = continue $ hurl {_focus = Top}
headersBrowserHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = continue $ hurl {_focus = FocusBodyEditor}
headersBrowserHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEnter [])) = continue $ hurl {_focus = Top}
headersBrowserHandleEvent hurl@Hurl {_headers = (Headers headers editor)} (VtyEvent (Vty.EvKey (Vty.KChar 'd') [])) =
  continue $ hurl {_headers = Headers (listRemove (fromJust $ listSelected headers) headers) editor}
headersBrowserHandleEvent hurl@Hurl {_headers = (Headers headers _)} (VtyEvent (Vty.EvKey (Vty.KChar 'o') [])) =
  continue $
    hurl
      { _focus = FocusHeaderNameEditor,
        _headers =
          let newIndex = maybe 0 ((+) 1 . fst) (listSelectedElement headers)
              newEditor = editorText HeadersEditor (Just 1) ""
           in Headers (listMoveTo newIndex . listInsert newIndex (Header "" "") $ headers) newEditor
      }
headersBrowserHandleEvent hurl@Hurl {_headers = (Headers headers editor)} (VtyEvent e) =
  handleListEventVi (const return) e headers >>= \newHeaders -> continue $ hurl {_headers = Headers newHeaders editor}
headersBrowserHandleEvent hurl _ = continue hurl

headersNameEditorHandleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
headersNameEditorHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEsc [])) = continue $ hurl {_focus = FocusHeadersBrowser}
headersNameEditorHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEnter [])) = continue $ hurl {_focus = Top}
headersNameEditorHandleEvent hurl@Hurl {_headers = (Headers headers editor)} (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) =
  continue $
    hurl
      { _focus = FocusHeaderValueEditor,
        _headers = Headers headers $ editorText HeadersEditor (Just 1) (_value (snd . fromJust $ listSelectedElement headers))
      }
headersNameEditorHandleEvent hurl@Hurl {_headers = (Headers headers editor)} (VtyEvent e) =
  handleEditorEvent e editor >>= \newEditor ->
    continue $
      hurl
        { _headers = Headers (listModify (\header -> header {_name = head $ getEditContents newEditor}) headers) newEditor
        }
headersNameEditorHandleEvent hurl _ = continue hurl

headersValueEditorHandleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
headersValueEditorHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEsc [])) = continue $ hurl {_focus = FocusHeadersBrowser}
headersValueEditorHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEnter [])) = continue $ hurl {_focus = Top}
headersValueEditorHandleEvent hurl (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = continue $ hurl {_focus = FocusHeadersBrowser}
headersValueEditorHandleEvent hurl@Hurl {_headers = (Headers headers editor)} (VtyEvent e) =
  handleEditorEvent e editor >>= \newEditor ->
    continue $
      hurl
        { _headers = Headers (listModify (\header -> header {_value = head $ getEditContents newEditor}) headers) newEditor
        }
headersValueEditorHandleEvent hurl _ = continue hurl

bodyEditorHandleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
bodyEditorHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEsc [])) = continue $ hurl {_focus = Top}
bodyEditorHandleEvent hurl@Hurl {_body = body} (VtyEvent e) =
  handleEditorEvent e body >>= \newEditor ->
    continue $
      hurl
        { _body = newEditor
        }
bodyEditorHandleEvent hurl _ = continue hurl

responseHandleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
responseHandleEvent hurl (VtyEvent (Vty.EvKey Vty.KEsc [])) = continue $ hurl {_focus = Top, _response = NoResponse}
responseHandleEvent hurl@Hurl {_response = (Success response)} (VtyEvent e) =
  handleListEventVi (const return) e response >>= \newList -> continue $ hurl {_response = Success newList}
responseHandleEvent hurl _ = continue hurl

handleEvent :: Hurl -> BrickEvent Name Event -> EventM Name (Next Hurl)
handleEvent hurl@Hurl {_focus = Top} = topHandleEvent hurl
handleEvent hurl@Hurl {_focus = Method} = methodHandleEvent hurl
handleEvent hurl@Hurl {_focus = Url} = urlHandleEvent hurl
handleEvent hurl@Hurl {_focus = FocusHeadersBrowser} = headersBrowserHandleEvent hurl
handleEvent hurl@Hurl {_focus = FocusHeaderNameEditor} = headersNameEditorHandleEvent hurl
handleEvent hurl@Hurl {_focus = FocusHeaderValueEditor} = headersValueEditorHandleEvent hurl
handleEvent hurl@Hurl {_focus = FocusBodyEditor} = bodyEditorHandleEvent hurl
handleEvent hurl@Hurl {_focus = FocusResponse} = responseHandleEvent hurl

chooseCursor :: Hurl -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor (Hurl Method _ _ _ _ _) = showCursorNamed MethodEditor
chooseCursor (Hurl Url _ _ _ _ _) = showCursorNamed UrlEditor
chooseCursor (Hurl FocusHeadersBrowser _ _ _ _ _) = showCursorNamed HeadersEditor
chooseCursor (Hurl FocusHeaderNameEditor _ _ _ _ _) = showCursorNamed HeadersEditor
chooseCursor (Hurl FocusHeaderValueEditor _ _ _ _ _) = showCursorNamed HeadersEditor
chooseCursor (Hurl FocusBodyEditor _ _ _ _ _) = showCursorNamed BodyEditor
chooseCursor _ = const Nothing

runHurl :: IO ()
runHurl = do
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

showHttpExceptionFriendly :: Http.HttpException -> T.Text
showHttpExceptionFriendly (Http.InvalidUrlException _ _) = "Bad URL"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.StatusCodeException _ _)) = error "Non-2XX status codes should not throw exceptions"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.TooManyRedirects _)) = "Too Many Redirects"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ Http.OverlongHeaders) = "Overlong Headers"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ Http.ResponseTimeout) = "Response Timeout"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ Http.ConnectionTimeout) = "Connection Timeout"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.ConnectionFailure _)) = "Connection Failure"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.InvalidStatusLine _)) = "Invalid Status Line"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.InvalidHeader _)) = "Invalid Header"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.InvalidRequestHeader _)) = "Invalid Request Header"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.InternalException _)) = "Internal Exception"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.ProxyConnectException _ _ _)) = "Proxy Connect Exception"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ Http.NoResponseDataReceived) = "No Response Data Received"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ Http.TlsNotSupported) = "TLS Not Supported"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.WrongRequestBodyStreamSize _ _)) = "Wrong Request Body Stream Size"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.ResponseBodyTooShort _ _)) = "Response Body Too Short"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ Http.InvalidChunkHeaders) = "Invlaid Chunk Headers"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ Http.IncompleteHeaders) = "Incomplete Headers"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.InvalidDestinationHost _)) = "Invalid Destination Host"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.HttpZlibException _)) = "HTTP ZLib Exception"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.InvalidProxyEnvironmentVariable _ _)) = "Invalid Proxy Environment Variable"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ Http.ConnectionClosed) = "Connection Closed"
showHttpExceptionFriendly (Http.HttpExceptionRequest _ (Http.InvalidProxySettings _)) = "Invalid Proxy Settings"
