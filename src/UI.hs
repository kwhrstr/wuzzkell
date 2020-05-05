{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module UI where

import RIO
import Brick (App(..), AttrMap(..), (<+>), (<=>))
import Brick.Types
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as BE
import qualified RIO.Text as T
import qualified Brick.Focus as BF
import qualified Brick.Forms as Form
import Brick.Forms (Form, (@@=), FormFieldState)
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Center as C 
import qualified Graphics.Vty as V
import qualified Brick as B
import Http 
import qualified Network.HTTP.Types as Http 
import qualified RIO.Vector as Vec
import RIO.List
import Control.Lens.TH
import qualified Data.Text.Zipper as Z
import Brick.BChan

-- Types

-- | DoRequest mark request
--
-- This is our custom event that handle VtyEvent handle
data DoRequest = DoRequest

-- | Named resources
data Name = NameURL
          | NameURLParam
          | NameMethod
          | NameRequestData
          | NameRequestHeaders
          | NameResponseHeaders
          | NameResponseBody
          deriving (Show, Eq, Ord, Enum, Bounded)

-- | State of the brick app. Contains the controls and any other required state
data BrickState = BrickState
  { _stEditURL :: BE.Editor Text Name -- ^ Editor for the URL
  , _stEditHeader :: BE.Editor Text Name -- ^ Editor for the Header
  , _stEditParam :: BE.Editor Text Name -- ^ Editor for the URL Param
  , _stListMethod :: BL.List Name Http.StdMethod  -- ^ List for the Http Method
  , _stEditBody :: BE.Editor Text Name -- ^ Editor for the Body
  , _stEditResponseHeader :: BE.Editor Text Name -- ^ ReadOnly Editor for the Response Header. 
  , _stEditResponseBody :: BE.Editor Text Name -- ^ ReadOnly Editor for the Response Body
  , _stFocus :: BF.FocusRing Name -- ^ Focus ring - a circular list of focusable controls
  , _stHttpReq :: HttpRequest -- ^ Request Data
  , _stIsSending :: Bool -- ^ Whether Sending request or not
  , _stAppEventChan :: Maybe (BChan DoRequest) -- ^ Chan Of App Event
  }
makeLenses ''BrickState

-- | Defines how the brick application will work / handle events
app :: App BrickState DoRequest Name
app = App
  { appDraw = drawUi
  , appChooseCursor = BF.focusRingCursor $ view stFocus
  , appHandleEvent = handleEvent
  , appStartEvent = pure
  , appAttrMap = const theMap
  }

-- | Construct the initial state values
initBrickState :: BrickState
initBrickState = BrickState
  { _stEditURL = BE.editor NameURL (Just 1) $ initialHttpReq ^. reqRawUri
  , _stEditHeader = BE.editor NameRequestHeaders Nothing $ initialHttpReq ^. reqRawHeaders
  , _stEditParam = BE.editor NameURLParam Nothing $ initialHttpReq ^. reqRawParams
  , _stListMethod = BL.list NameMethod (Vec.fromList universe) 1
  , _stEditBody = BE.editor NameRequestData Nothing $ initialHttpReq ^. reqRawBody
  , _stEditResponseHeader = BE.editor NameResponseHeaders Nothing ""
  , _stEditResponseBody = BE.editor NameResponseBody Nothing ""
  , _stFocus = BF.focusRing universe
  , _stHttpReq = initialHttpReq
  , _stIsSending = False
  , _stAppEventChan = Nothing
  }

-- | Draw the UI
drawUi :: BrickState -> [Widget Name]
drawUi st = [sendingWidget, B.padAll 1 contentBlock]
  where
    sendingWidget = if st ^. stIsSending
      then C.vCenterLayer $ C.hCenterLayer $ B.border $ B.txt "Sending request.."
      else B.emptyWidget
    contentBlock = B.withBorderStyle BS.unicode $ B.border
                 $ B.vBox [urlWidget, requests <+> results]
    requests = B.hLimitPercent 30 $ B.vBox [B.vLimitPercent 40 $ urlParamWidget <=> B.vLimit 3 methodWidget, requestDataWidget, requestHeadersWidget]
    results =  B.vBox [B.vLimitPercent 40 responseHeaderWidget, responseBodyWidget]
    editorWidget l ed = B.borderWithLabel (B.txt l) $ editor ed
    urlWidget =  editorWidget "URL" $ st ^. stEditURL
    methodList =  st ^. stListMethod
    renderingList b l = C.hCenter $ B.str $ if b then '[' : show l <> "]" else  show l
    methodWidget = B.borderWithLabel (B.padLeftRight 1 $ B.txt "Method" )
                    $ BL.renderList renderingList
                      (BF.focusGetCurrent (st ^. stFocus) == Just (B.getName methodList)) 
                      methodList
    urlParamWidget =  editorWidget "URL params" $ st ^. stEditParam
    requestDataWidget = editorWidget "Requast data" $ st ^. stEditBody
    requestHeadersWidget = editorWidget "Request headers" $ st ^. stEditHeader
    responseHeaderWidget = editorWidget "Response headers" $ st ^. stEditResponseHeader
    responseBodyWidget = editorWidget "Response body" $ st ^. stEditResponseBody
    editor ed
      = BE.renderEditor (B.txt . T.unlines)
          (BF.focusGetCurrent (st ^. stFocus) == Just (B.getName ed)) ed

-- | Main even handler for brick events
handleEvent ::  BrickState -> BrickEvent Name DoRequest -> EventM Name (Next BrickState)
handleEvent st ev = case ev of
  AppEvent DoRequest -> do
      res <- liftIO $ doRequest $ st ^. stHttpReq
      B.continue $ st
         & over stEditResponseHeader
            ( BE.applyEdit
              ( const $ Z.moveCursor (0, 0) $ Z.textZipper (T.linesCR $ res ^. responseHeader) Nothing )
            )
         & over stEditResponseBody
            ( BE.applyEdit
              ( const $ Z.moveCursor (0, 0) $ Z.textZipper (T.linesCR $ res ^. responseBody) Nothing )
            )
         & set stIsSending False
  MouseDown n _ _ _ -> B.continue (st & over stFocus (BF.focusSetCurrent n))
  MouseUp n _ _  -> B.continue (st & over stFocus (BF.focusSetCurrent n))
  VtyEvent ve@(V.EvKey k ms) ->
    case (k, ms) of
      (V.KEsc, []) -> B.halt st
      (V.KChar '\t', _) -> B.continue $ st & over stFocus BF.focusNext
      (V.KBackTab, []) -> B.continue $ st & over stFocus BF.focusPrev
      (V.KChar 'r', [V.MCtrl]) -> submitEvent st >>= B.continue
      _ ->
        case BF.focusGetCurrent $ st ^. stFocus of
          Just NameURL -> 
            case k of
              V.KEnter -> submitEvent st >>= B.continue
              _ -> do
               r <- BE.handleEditorEvent ve $ st ^. stEditURL
               B.continue $ st & set stEditURL r
                               & set (stHttpReq . reqRawUri) (T.intercalate "\n" $ BE.getEditContents r)
          Just NameURLParam -> do
            r <- BE.handleEditorEvent ve $ st ^. stEditParam
            B.continue $ st & set stEditParam r
                            & set (stHttpReq . reqRawParams) (T.intercalate "\n" $ BE.getEditContents r)
          Just NameMethod -> do
            r <- BL.handleListEvent ve $ st ^. stListMethod            
            B.continue $ st & set stListMethod r
                            & set (stHttpReq . reqMethod) 
                                  ( maybe Http.GET snd 
                                  $ BL.listSelectedElement 
                                  $ st ^. stListMethod)
          Just NameRequestHeaders -> do
            r <- BE.handleEditorEvent ve $ st ^. stEditHeader
            B.continue $ st & set stEditHeader r
                            & set (stHttpReq . reqRawHeaders) (T.intercalate "\n" $ BE.getEditContents r)
          Just NameRequestData -> do
            r <- BE.handleEditorEvent ve $ st ^. stEditBody
            B.continue $ st & set stEditBody r
                            & set (stHttpReq . reqRawBody) (T.intercalate "\n" $ BE.getEditContents r)
          Just NameResponseHeaders -> do
            r <- handleShowOnlyEditorEvent ve $ st ^. stEditResponseHeader
            B.continue $ st & set stEditResponseHeader r
          Just NameResponseBody -> do
            r <- handleShowOnlyEditorEvent ve $ st ^. stEditResponseBody
            B.continue $ st & set stEditResponseBody r
  _ -> B.continue st
  where
  submitEvent st' = case st' ^. stAppEventChan of
    Nothing -> pure st'
    Just chan -> do
      liftIO $ writeBChan chan DoRequest
      pure $ st' & set stIsSending True
                     & over stEditResponseHeader
                        ( BE.applyEdit Z.clearZipper)
                     & over stEditResponseBody
                        ( BE.applyEdit Z.clearZipper)

-- | Enum Lists
universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]
{-# INLINE universe #-}


-- | Event of editor that can not write 
handleShowOnlyEditorEvent :: (BE.DecodeUtf8 t, Eq t, Monoid t) => V.Event -> BE.Editor t n -> EventM n (BE.Editor t n)
handleShowOnlyEditorEvent e ed = case e of
  V.EvPaste bs -> pure ed
  V.EvKey V.KEnter [] -> pure ed
  V.EvKey V.KDel [] -> pure ed
  V.EvKey (V.KChar c) [] | c /= '\t' -> pure ed
  V.EvKey V.KBS [] -> pure ed
  _ -> BE.handleEditorEvent e ed 


theMap :: AttrMap
theMap = BA.attrMap V.defAttr
       [ (BE.editAttr, V.black `B.on` V.cyan)
       , (BE.editFocusedAttr, V.black `B.on` V.yellow)
       , (BL.listSelectedAttr, V.black `B.on` V.cyan)
       , (BL.listSelectedFocusedAttr , V.black `B.on` V.yellow)
       , (B.borderAttr, B.fg V.cyan)
       ]

    

