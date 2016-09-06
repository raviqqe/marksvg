{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Monad.State
import Data.List.Split
import Data.Text (pack, unpack)
import Data.String

import CMark
import Text.Blaze.Internal
import Text.Blaze.Svg11 as S hiding (textpath)
import Text.Blaze.Svg11.Attributes
import Text.Blaze.Svg.Renderer.String



main :: IO ()
main = do
  n <- readMarkdown
  forM_ (zip [1..] $ map renderSvg $ markdownToSvgs n) $ \(i, s) -> do
    writeFile (show i ++ ".svg") s


readMarkdown :: IO Node
readMarkdown = commonmarkToNode [] <$> pack <$> getContents


markdownToSvgs :: Node -> [Svg]
markdownToSvgs (Node _ DOCUMENT ns) =
  map (flip evalState initialState . nodesToSvg) $ splitIntoPages ns
  where
    splitIntoPages = splitWhen isThematicBreak
      where
        isThematicBreak (Node _ THEMATIC_BREAK _) = True
        isThematicBreak _ = False

    initialState = CompilerState {
        pageX = sideOffset defaultConfig,
        pageY = topOffset defaultConfig,
        defsElems = [],
        pathId = 0,
        currFontSize = maxFontSize defaultConfig,
        config = defaultConfig
      }
      where
        defaultConfig = CompilerConfig {
            pageWidth = 1024,
            pageHeight = 768,
            emphAttrs = [stroke "red"],
            sideOffset = 20,
            topOffset = 10,
            maxFontSize = 80,
            lineSpace = 1.2
          }
markdownToSvgs _ = error "Expect DOCUMENT"


type Pixel = Integer

data CompilerState = CompilerState {
    pageX :: Pixel,
    pageY :: Pixel,
    defsElems :: [Svg],
    pathId :: Integer,
    currFontSize :: Pixel,
    config :: CompilerConfig
  }

data CompilerConfig = CompilerConfig {
    pageHeight :: Pixel,
    pageWidth :: Pixel,
    emphAttrs :: [Attribute],
    sideOffset :: Pixel,
    topOffset :: Pixel,
    maxFontSize :: Pixel,
    lineSpace :: Float
  }

type Compiler = State CompilerState


nodesToSvg :: [Node] -> Compiler Svg
nodesToSvg ns = do
  w <- fromString <$> show <$> gets (pageWidth . config)
  h <- fromString <$> show <$> gets (pageHeight . config)
  let svgPage = docTypeSvg ! version "1.1" ! width w ! height h

  es <- nodesToElems ns
  defEs <- gets defsElems
  return . svgPage $ do
    defs $ sequence_ defEs
    sequence_ es


nodeToElem :: Node -> Compiler Svg
nodeToElem (Node _ DOCUMENT _) = error "Unexpected DOCUMENT"
nodeToElem (Node _ THEMATIC_BREAK _) = error "Unexpected THEMATIC_BREAK"

nodeToElem (Node _ PARAGRAPH ns) = do
  pathId <- ("path" ++) <$> show <$> getNewPathId
  y <- gets pageY
  width <- gets (pageWidth . config)
  offset <- gets (sideOffset . config)
  space <- gets (lineSpace . config)
  fsize <- gets currFontSize

  let path = mkPath $ do
        forM_ [1..10] $ \i -> do
          m offset $ y + (round $ (fromInteger $ i * fsize) * space)
          h (width - offset)

  appendDef $ S.path ! id_ (fromString pathId) ! d path

  return . (text_ ! fontSize (toAttrValue fsize))
         . (textpath ! xlinkHref (fromString $ "#" ++ pathId))
         =<< sequence_ <$> nodesToElems ns

nodeToElem (Node _ BLOCK_QUOTE _) = return $ return ()
nodeToElem (Node _ (HTML_BLOCK _) _) = return $ return ()
nodeToElem (Node _ (CUSTOM_BLOCK _ _) _) = return $ return ()
nodeToElem (Node _ (CODE_BLOCK _ _) _) = return $ return ()

nodeToElem (Node _ (HEADING i) ns) = do
  changeFontSize (flip div $ toInteger i) $ do
    size <- gets currFontSize
    increaseY size
    px <- gets pageX
    py <- gets pageY

    return . (text_ ! (fontSize $ fromString $ show $ size)
                    ! x (toAttrValue px) ! y (toAttrValue py))
           . sequence_ =<< nodesToElems ns

nodeToElem (Node _ (LIST _) _) = return $ return ()
nodeToElem (Node _ ITEM _) = return $ return ()

nodeToElem (Node _ (TEXT t) _) = do
  return $ toMarkup $ unpack t

nodeToElem (Node _ SOFTBREAK _) = return $ return ()
nodeToElem (Node _ LINEBREAK _) = return $ return ()
nodeToElem (Node _ (HTML_INLINE _) _) = return $ return ()
nodeToElem (Node _ (CUSTOM_INLINE _ _) _) = return $ return ()
nodeToElem (Node _ (CODE _) _) = return $ return ()

nodeToElem (Node _ EMPH ns) = do
  attrs <- gets (emphAttrs . config)
  es <- nodesToElems ns
  return $ g !* attrs $ sequence_ es

nodeToElem n@(Node _ STRONG _) = nodeToElem n

nodeToElem (Node _ (LINK _ _) _) = return $ return ()
nodeToElem (Node _ (IMAGE _ _) _) = return $ return ()


nodesToElems :: [Node] -> Compiler [Svg]
nodesToElems = mapM nodeToElem


getNewPathId :: Compiler Integer
getNewPathId = do
  s <- get
  put $ s { pathId = pathId s + 1 }
  return $ pathId s


appendDef :: Svg -> Compiler ()
appendDef def = modify $ \s -> s { defsElems = def:(defsElems s) }


increaseX :: Pixel -> Compiler ()
increaseX p = modify $ \s -> s { pageX = pageX s + p }


increaseY :: Pixel -> Compiler ()
increaseY p = modify $ \s -> s { pageY = pageY s + p }


textpath :: Svg -> Svg
textpath = Parent "textPath" "<textPath"  "</textPath>"


changeFontSize :: (Pixel -> Pixel) -> Compiler a -> Compiler a
changeFontSize changeSize doSome = do
  size <- gets currFontSize
  modify $ \s -> s { currFontSize = changeSize size }
  y <- doSome
  modify $ \s -> s { currFontSize = size }
  return y


(!*) :: Attributable h => h -> [Attribute] -> h
h !* as = foldl (!) h as


toAttrValue :: Show a => a -> AttributeValue
toAttrValue = fromString . show
