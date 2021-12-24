{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Graphics.Blank.JavaScript where

import           Data.Bits                       (shiftR, (.&.))
import           Data.Char                       (isAscii, isControl, ord)
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Default.Class
import           Data.Ix
import qualified Data.List                       as L
import           Data.String
import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Builder          as B (singleton)
import           Data.Vector.Unboxed             (Vector, toList)
import qualified Data.Vector.Unboxed             as V
import           Data.Word                       (Word8)

import           Graphics.Blank.Parser

import           Prelude.Compat

import           Text.ParserCombinators.ReadP    (choice, skipSpaces)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read                       (Read (..), parens,
                                                  readListPrecDefault)

import           Numeric                         (showHex)
import           TextShow
import           TextShow.Data.Floating          (showbFFloat)
import           TextShow.Data.Integral          (showbHex)
import           TextShow.TH                     (deriveTextShow)

-------------------------------------------------------------

-- | A handle to an offscreen canvas. 'CanvasContext' cannot be destroyed.
data CanvasContext = CanvasContext Int Int Int deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasContext)

-- | A handle to a canvas image. 'CanvasImage's cannot be destroyed.
data CanvasImage = CanvasImage Int Int Int     deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasImage)

-- | A handle to the a canvas gradient. 'CanvasGradient's cannot be destroyed.
newtype CanvasGradient = CanvasGradient Int    deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasGradient)

-- | A handle to a canvas pattern. 'CanvasPattern's cannot be destroyed.
newtype CanvasPattern = CanvasPattern Int      deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasPattern)

-- | A handle to a canvas audio. 'CanvasAudio's cannot be destroyed.
data CanvasAudio = CanvasAudio !Int !Double    deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasAudio)

-------------------------------------------------------------

-- | 'ImageData' is a transliteration of JavaScript's
-- @<https://developer.mozilla.org/en-US/docs/Web/API/ImageData ImageData>@.
-- 'ImageData' consists of two 'Int's and one (unboxed) 'Vector' of 'Word8's.
-- @width@, @height@, and @data@ can be projected from 'ImageData',
-- 'Vector.length' can be used to find the @data@ length.
--
-- Note: 'ImageData' lives on the server, not the client.

data ImageData = ImageData !Int !Int !(Vector Word8) deriving (Eq, Ord, Show)

-- Defined manually to avoid an orphan T.Show (Vector a) instance
instance TextShow ImageData where
    showbPrec p (ImageData w h d) = showbParen (p > 10) $
        "ImageData " <> showbPrec 11 w <> showbSpace
                     <> showbPrec 11 h <> showbSpace
                     <> showbUnaryWith showbPrec "fromList" 11 (toList d)

-------------------------------------------------------------

-- | Class for JavaScript objects that represent images (including the canvas itself).
class Image a where
    jsImage :: a -> Builder
    width  :: Num b => a -> b
    height :: Num b => a -> b

instance Image CanvasImage where
    jsImage = jsCanvasImage
    width  (CanvasImage _ w _) = fromIntegral w
    height (CanvasImage _ _ h) = fromIntegral h

-- The Image of a canvas is the the canvas context, not the DOM entry, so
-- you need to indirect back to the DOM here.
instance Image CanvasContext where
    jsImage = (<> ".canvas") . jsCanvasContext
    width  (CanvasContext _ w _) = fromIntegral w
    height (CanvasContext _ _ h) = fromIntegral h

class Audio a where
    jsAudio  :: a -> Builder
    duration :: Fractional b => a -> b

instance Audio CanvasAudio where
  jsAudio                    = jsCanvasAudio
  duration (CanvasAudio _ d) = realToFrac d

-- instance Element Video  -- Not supported

-----------------------------------------------------------------------------

-- | A data type that can represent a style. That is, something with one or more
-- colors.
class Style a where
    -- | Convert a value into a JavaScript string representing a style value.
    jsStyle :: a -> Builder

instance Style Text                 where { jsStyle = jsText }
instance Style CanvasGradient       where { jsStyle = jsCanvasGradient }
instance Style CanvasPattern        where { jsStyle = jsCanvasPattern }
instance Style (Colour Double)      where { jsStyle = jsColour }
instance Style (AlphaColour Double) where { jsStyle = jsAlphaColour }

-- | A 'Style' containing exactly one color.
class Style a => CanvasColor a

jsCanvasColor :: CanvasColor color => color -> Builder
jsCanvasColor = jsStyle

instance CanvasColor Text
instance CanvasColor (Colour Double)
instance CanvasColor (AlphaColour Double)

-------------------------------------------------------------

-- | The direction in which a 'CanvasPattern' repeats.
data RepeatDirection = Repeat   -- ^ The pattern repeats both horizontally
                                --   and vertically (default).
                     | RepeatX  -- ^ The pattern repeats only horizontally.
                     | RepeatY  -- ^ The pattern repeats only vertically.
                     | NoRepeat -- ^ The pattern displays only once and
                                --   does not repeat.
  deriving (Bounded, Enum, Eq, Ix, Ord)

-- | Shorthand for 'Repeat', with an underscore to distinguish it from 'repeat'.
repeat_ :: RepeatDirection
repeat_ = Repeat

-- | Shorthand for 'RepeatX'.
repeatX :: RepeatDirection
repeatX = RepeatX

-- | Shorthand for 'RepeatY'.
repeatY :: RepeatDirection
repeatY = RepeatY

-- | Shorthand for 'NoRepeat'.
noRepeat :: RepeatDirection
noRepeat = NoRepeat

instance Default RepeatDirection where
  def = Repeat

instance IsString RepeatDirection where
  fromString = read

instance Read RepeatDirection where
    readPrec = parens . lift $ do
        skipSpaces
        choice
            [ Repeat   <$ stringCI "repeat"
            , RepeatX  <$ stringCI "repeat-x"
            , RepeatY  <$ stringCI "repeat-y"
            , NoRepeat <$ stringCI "no-repeat"
            ]
    readListPrec = readListPrecDefault

instance Show RepeatDirection where
    showsPrec p = showsPrec p . FromTextShow

instance TextShow RepeatDirection where
    showb Repeat   = "repeat"
    showb RepeatX  = "repeat-x"
    showb RepeatY  = "repeat-y"
    showb NoRepeat = "no-repeat"

-- | The style of the caps on the endpoints of a line.
data LineEndCap = ButtCap   -- ^ Flat edges (default).
                | RoundCap  -- ^ Semicircular end caps
                | SquareCap -- ^ Square end caps
  deriving (Bounded, Enum, Eq, Ix, Ord)

-- | Shorthand for 'ButtCap'.
butt :: LineEndCap
butt = ButtCap

-- | Shorthand for 'SquareCap'.
square :: LineEndCap
square = SquareCap

instance Default LineEndCap where
    def = ButtCap

instance IsString LineEndCap where
    fromString = read

instance Read LineEndCap where
    readPrec = parens . lift $ do
        skipSpaces
        choice
            [ ButtCap   <$ stringCI "butt"
            , RoundCap  <$ stringCI "round"
            , SquareCap <$ stringCI "square"
            ]
    readListPrec = readListPrecDefault

instance RoundProperty LineEndCap where
    round_ = RoundCap

instance Show LineEndCap where
    showsPrec p = showsPrec p . FromTextShow

instance TextShow LineEndCap where
    showb ButtCap   = "butt"
    showb RoundCap  = "round"
    showb SquareCap = "square"

-- | The style of corner that is created when two lines join.
data LineJoinCorner = BevelCorner -- ^ A filled triangle with a beveled edge
                                  --   connects two lines.
                    | RoundCorner -- ^ A filled arc connects two lines.
                    | MiterCorner -- ^ A filled triangle with a sharp edge
                                  --   connects two lines (default).
  deriving (Bounded, Enum, Eq, Ix, Ord)

-- | Shorthand for 'BevelCorner'.
bevel :: LineJoinCorner
bevel = BevelCorner

-- | Shorthand for 'MiterCorner'.
miter :: LineJoinCorner
miter = MiterCorner

instance Default LineJoinCorner where
    def = MiterCorner

instance IsString LineJoinCorner where
    fromString = read

instance Read LineJoinCorner where
    readPrec = parens . lift $ do
        skipSpaces
        choice
            [ BevelCorner <$ stringCI "bevel"
            , RoundCorner <$ stringCI "round"
            , MiterCorner <$ stringCI "miter"
            ]
    readListPrec = readListPrecDefault

instance RoundProperty LineJoinCorner where
    round_ = RoundCorner

instance Show LineJoinCorner where
    showsPrec p = showsPrec p . FromTextShow

instance TextShow LineJoinCorner where
    showb BevelCorner = "bevel"
    showb RoundCorner = "round"
    showb MiterCorner = "miter"

-- | The anchor point for text in the current 'DeviceContext'.
data TextAnchorAlignment = StartAnchor  -- ^ The text is anchored at either its left edge
                                        --   (if the canvas is left-to-right) or its right
                                        --   edge (if the canvas is right-to-left).
                         | EndAnchor    -- ^ The text is anchored at either its right edge
                                        --   (if the canvas is left-to-right) or its left
                                        --   edge (if the canvas is right-to-left).
                         | CenterAnchor -- ^ The text is anchored in its center.
                         | LeftAnchor   -- ^ The text is anchored at its left edge.
                         | RightAnchor  -- ^ the text is anchored at its right edge.
  deriving (Bounded, Enum, Eq, Ix, Ord)

-- | Shorthand for 'StartAnchor'.
start :: TextAnchorAlignment
start = StartAnchor

-- | Shorthand for 'EndAnchor'.
end :: TextAnchorAlignment
end = EndAnchor

-- | Shorthand for 'CenterAnchor'.
center :: TextAnchorAlignment
center = CenterAnchor

-- | Shorthand for 'LeftAnchor'.
left :: TextAnchorAlignment
left = LeftAnchor

-- | Shorthand for 'RightAnchor'.
right :: TextAnchorAlignment
right = RightAnchor

instance Default TextAnchorAlignment where
    def = StartAnchor

instance IsString TextAnchorAlignment where
    fromString = read

instance Read TextAnchorAlignment where
    readPrec = parens . lift $ do
        skipSpaces
        choice
            [ StartAnchor  <$ stringCI "start"
            , EndAnchor    <$ stringCI "end"
            , CenterAnchor <$ stringCI "center"
            , LeftAnchor   <$ stringCI "left"
            , RightAnchor  <$ stringCI "right"
            ]
    readListPrec = readListPrecDefault

instance Show TextAnchorAlignment where
    showsPrec p = showsPrec p . FromTextShow

instance TextShow TextAnchorAlignment where
    showb StartAnchor  = "start"
    showb EndAnchor    = "end"
    showb CenterAnchor = "center"
    showb LeftAnchor   = "left"
    showb RightAnchor  = "right"

-- | The baseline alignment used when drawing text in the current 'DeviceContext'.
--   The baselines are ordered from highest ('Top') to lowest ('Bottom').
data TextBaselineAlignment = TopBaseline
                           | HangingBaseline
                           | MiddleBaseline
                           | AlphabeticBaseline
                           | IdeographicBaseline
                           | BottomBaseline
  deriving (Bounded, Enum, Eq, Ix, Ord)

-- | Shorthand for 'TopBaseline'.
top :: TextBaselineAlignment
top = TopBaseline

-- | Shorthand for 'HangingBaseline'.
hanging :: TextBaselineAlignment
hanging = HangingBaseline

-- | Shorthand for 'MiddleBaseline'.
middle :: TextBaselineAlignment
middle = MiddleBaseline

-- | Shorthand for 'AlphabeticBaseline'.
alphabetic :: TextBaselineAlignment
alphabetic = AlphabeticBaseline

-- | Shorthand for 'IdeographicBaseline'.
ideographic :: TextBaselineAlignment
ideographic = IdeographicBaseline

-- | Shorthand for 'BottomBaseline'.
bottom :: TextBaselineAlignment
bottom = BottomBaseline

instance Default TextBaselineAlignment where
    def = AlphabeticBaseline

instance IsString TextBaselineAlignment where
    fromString = read

instance Read TextBaselineAlignment where
    readPrec = parens . lift $ do
        skipSpaces
        choice
            [ TopBaseline         <$ stringCI "top"
            , HangingBaseline     <$ stringCI "hanging"
            , MiddleBaseline      <$ stringCI "middle"
            , AlphabeticBaseline  <$ stringCI "alphabetic"
            , IdeographicBaseline <$ stringCI "ideographic"
            , BottomBaseline      <$ stringCI "bottom"
            ]
    readListPrec = readListPrecDefault

instance Show TextBaselineAlignment where
    showsPrec p = showsPrec p . FromTextShow

instance TextShow TextBaselineAlignment where
    showb TopBaseline         = "top"
    showb HangingBaseline     = "hanging"
    showb MiddleBaseline      = "middle"
    showb AlphabeticBaseline  = "alphabetic"
    showb IdeographicBaseline = "ideographic"
    showb BottomBaseline      = "bottom"

-- | Class for @round@ CSS property values.
class RoundProperty a where
    -- | Shorthand for 'RoundCap' or 'RoundCorner', with an underscore to
    -- distinguish it from 'round'.
    round_ :: a

-------------------------------------------------------------

-- | Class for Haskell data types which represent JavaScript data.
class JSArg a where
    -- | Display a value as JavaScript data.
    showbJS :: a -> Builder

instance JSArg (AlphaColour Double) where
  showbJS = jsAlphaColour

jsAlphaColour :: AlphaColour Double -> Builder
jsAlphaColour aCol
    | a >= 1    = jsColour rgbCol
    | a <= 0    = jsLiteralBuilder "rgba(0,0,0,0)"
    | otherwise = jsLiteralBuilder $ "rgba("
        <> showb r    <> B.singleton ','
        <> showb g    <> B.singleton ','
        <> showb b    <> B.singleton ','
        <> jsDouble a <> B.singleton ')'
  where
    a         = alphaChannel aCol
    rgbCol    = darken (recip a) $ aCol `over` black
    RGB r g b = toSRGB24 rgbCol

instance JSArg Bool where
    showbJS = jsBool

jsBool :: Bool -> Builder
jsBool True  = "true"
jsBool False = "false"

instance JSArg CanvasAudio where
  showbJS = jsCanvasAudio

jsCanvasAudio :: CanvasAudio -> Builder
jsCanvasAudio (CanvasAudio n _ ) = "sounds[" <> showb n <> B.singleton ']'

instance JSArg CanvasContext where
    showbJS = jsCanvasContext

jsCanvasContext :: CanvasContext -> Builder
jsCanvasContext (CanvasContext n _ _) = "canvasbuffers[" <> showb n <> B.singleton ']'

instance JSArg CanvasImage where
    showbJS = jsCanvasImage

jsCanvasImage :: CanvasImage -> Builder
jsCanvasImage (CanvasImage n _ _) = "images[" <> showb n <> B.singleton ']'

instance JSArg CanvasGradient where
    showbJS = jsCanvasGradient

jsCanvasGradient :: CanvasGradient -> Builder
jsCanvasGradient (CanvasGradient n) = "gradient_" <> showb n

instance JSArg CanvasPattern where
    showbJS = jsCanvasPattern

jsCanvasPattern :: CanvasPattern -> Builder
jsCanvasPattern (CanvasPattern n) = "pattern_" <> showb n

instance JSArg (Colour Double) where
    showbJS = jsColour

jsColour :: Colour Double -> Builder
jsColour = jsLiteralBuilder . sRGB24showb

-- | Convert a colour in hexadecimal 'Builder' form, e.g. \"#00aaff\"
sRGB24showb :: (Floating b, RealFrac b) => Colour b -> Builder
sRGB24showb c =
    B.singleton '#' <> showbHex2 r' <> showbHex2 g' <> showbHex2 b'
  where
    RGB r' g' b' = toSRGB24 c
    showbHex2 x | x <= 0xf = B.singleton '0' <> showbHex x
                | otherwise = showbHex x

instance JSArg Double where
    showbJS = jsDouble

jsDouble :: Double -> Builder
jsDouble = showbFFloat $ Just 3

instance JSArg ImageData where
    showbJS = jsImageData

jsImageData :: ImageData -> Builder
jsImageData (ImageData w h d) = "ImageData(" <> showb w
    <> B.singleton ',' <> showb h
    <> ",[" <> vs <> "])"
  where
    vs = jsList showb $ V.toList d

instance JSArg Int where
    showbJS = jsInt

jsInt :: Int -> Builder
jsInt = showb

instance JSArg LineEndCap where
    showbJS = jsLineEndCap

jsLineEndCap :: LineEndCap -> Builder
jsLineEndCap = jsLiteralBuilder . showb

instance JSArg LineJoinCorner where
    showbJS = jsLineJoinCorner

jsLineJoinCorner :: LineJoinCorner -> Builder
jsLineJoinCorner = jsLiteralBuilder . showb

jsList :: (a -> Builder) -> [a] -> Builder
jsList js = mconcat . L.intersperse "," . map js

instance JSArg RepeatDirection where
    showbJS = jsRepeatDirection

jsRepeatDirection :: RepeatDirection -> Builder
jsRepeatDirection = jsLiteralBuilder . showb

instance JSArg Text where
    showbJS = jsText

jsText :: Text -> Builder
jsText = jsLiteralBuilder . fromText

instance JSArg TextAnchorAlignment where
    showbJS = jsTextAnchorAlignment

jsTextAnchorAlignment :: TextAnchorAlignment -> Builder
jsTextAnchorAlignment = jsLiteralBuilder . showb

instance JSArg TextBaselineAlignment where
    showbJS = jsTextBaselineAlignment

jsTextBaselineAlignment :: TextBaselineAlignment -> Builder
jsTextBaselineAlignment = jsLiteralBuilder . showb

-- The following was adapted from our Sunroof compiler.
-- -------------------------------------------------------------
-- Builder Conversion Utilities: Haskell -> JS
-- -------------------------------------------------------------

-- | Convert a 'Builder' to a representation as a JS string literal.
jsLiteralBuilder :: Builder -> Builder
jsLiteralBuilder = jsQuoteBuilder . jsEscapeBuilder

-- | Add quotes to a 'Builder'.
jsQuoteBuilder :: Builder -> Builder
jsQuoteBuilder b = B.singleton '"' <> b <> B.singleton '"'

-- | Correctly replace a `Builder'`s characters by the JS escape sequences.
jsEscapeBuilder :: Builder -> Builder
jsEscapeBuilder = fromLazyText . TL.concatMap jsEscapeChar . toLazyText

-- | Correctly replace Haskell characters by the JS escape sequences.
jsEscapeChar :: Char -> TL.Text
jsEscapeChar '\\' = "\\\\"
-- Special control sequences.
jsEscapeChar '\b' = "\\b"
jsEscapeChar '\f' = "\\f"
jsEscapeChar '\n' = "\\n"
jsEscapeChar '\r' = "\\r"
jsEscapeChar '\t' = "\\t"
jsEscapeChar '\v' = "\\v"
jsEscapeChar '\"' = "\\\""
jsEscapeChar '\'' = "\\'"
-- Code borrowed from GHCJS implementation: https://github.com/ghcjs/ghcjs/blob/718b37fc7167269ebca633914c716c3dbe6d0faf/src/Compiler/JMacro/Base.hs#L887-L905
jsEscapeChar '/'  = "\\/"
jsEscapeChar c
    -- Non-control ASCII characters can remain as they are.
    | not (isControl c) && isAscii c = TL.singleton c
    | ord c <= 0xff   = hexxs "\\x" 2 (ord c)
    -- All other non ASCII signs are escaped to unicode.
    | ord c <= 0xffff = hexxs "\\u" 4 (ord c)
    | otherwise      = let cp0 = ord c - 0x10000 -- output surrogate pair
                       in hexxs "\\u" 4 ((cp0 `shiftR` 10) + 0xd800) `mappend`
                          hexxs "\\u" 4 ((cp0 .&. 0x3ff) + 0xdc00)
    where hexxs prefix pad cp =
            let h = showHex cp ""
            in  TL.pack (prefix ++ replicate (pad - length h) '0' ++ h)
