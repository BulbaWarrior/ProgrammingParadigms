{-# LANGUAGE NoImplicitPrelude #-}

module PropertyRoundTrip ( roundTripTests ) where

import Prelude.Compat

import Control.Applicative (Const)
import Data.Aeson.Types
import Data.DList (DList)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy)
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import Data.Tagged (Tagged)
import Data.Time (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import Data.Version (Version)
import Data.Time.Calendar.Compat (CalendarDiffDays, DayOfWeek)
import Data.Time.LocalTime.Compat (CalendarDiffTime)
import Data.Time.Clock.System.Compat (SystemTime)
import Instances ()
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.UUID.Types as UUID
import PropUtils
import PropertyRTFunctors


roundTripTests :: TestTree
roundTripTests =
  testGroup "roundTrip" [
      testProperty "Bool" $ roundTripEq True
    , testProperty "Double" $ roundTripEq (1 :: Approx Double)
    , testProperty "Int" $ roundTripEq (1 :: Int)
    , testProperty "NonEmpty Char" $ roundTripEq (undefined :: NonEmpty Char)
    , testProperty "Integer" $ roundTripEq (1 :: Integer)
    , testProperty "String" $ roundTripEq ("" :: String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Lazy Text" $ roundTripEq LT.empty
    , testProperty "Foo" $ roundTripEq (undefined :: Foo)
    , testProperty "Day" $ roundTripEq (undefined :: Day)
    , testProperty "BCE Day" $ roundTripEq (undefined :: BCEDay)
    , testProperty "DotNetTime" $ roundTripEq (undefined :: Approx DotNetTime)
    , testProperty "LocalTime" $ roundTripEq (undefined :: LocalTime)
    , testProperty "TimeOfDay" $ roundTripEq (undefined :: TimeOfDay)
    , testProperty "UTCTime" $ roundTripEq (undefined :: UTCTime)
    , testProperty "ZonedTime" $ roundTripEq (undefined :: ZonedTime)
    , testProperty "NominalDiffTime" $ roundTripEq (undefined :: NominalDiffTime)
    , testProperty "DiffTime" $ roundTripEq (undefined :: DiffTime)
    , testProperty "DayOfWeek" $ roundTripEq (undefined :: DayOfWeek)
    , testProperty "SystemTime" $ roundTripEq (undefined :: SystemTime)
    , testProperty "CalendarDiffTime" $ roundTripEq (undefined :: CalendarDiffTime)
    , testProperty "CalendarDiffDays" $ roundTripEq (undefined :: CalendarDiffDays)
    , testProperty "Version" $ roundTripEq (undefined :: Version)
    , testProperty "Natural" $ roundTripEq (undefined :: Natural)
    , testProperty "Proxy" $ roundTripEq (undefined :: Proxy Int)
    , testProperty "Tagged" $ roundTripEq (undefined :: Tagged Int Char)
    , testProperty "Const" $ roundTripEq (undefined :: Const Int Char)
    , testProperty "DList" $ roundTripEq (undefined :: DList Int)
    , testProperty "Seq" $ roundTripEq (undefined :: Seq Int)
    , testProperty "Rational" $ roundTripEq (undefined :: Rational)
    , testProperty "Ratio Int" $ roundTripEq (undefined :: Ratio Int)
    , testProperty "UUID" $ roundTripEq UUID.nil
    , roundTripFunctorsTests
    , testGroup "ghcGenerics" [
        testProperty "OneConstructor" $ roundTripEq OneConstructor
      , testProperty "Product2" $ roundTripEq (undefined :: Product2 Int Bool)
      , testProperty "Product6" $ roundTripEq (undefined :: P6)
      , testProperty "Sum4" $ roundTripEq (undefined :: S4)
      ]
    ]
