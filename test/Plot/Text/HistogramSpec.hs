--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Plot.Text.HistogramSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( when )
import Plot.Text.Histogram
    ( Histogram (..), plot )
import Test.Hspec
    ( Spec, SpecWith, describe, expectationFailure, it )

spec :: Spec
spec = describe "Histograms" $ do
    assertPlots
        ( Histogram
            { width  = 80
            , height = 24
            , bins   =
                [ ( "Metallica", 78 )
                , ( "Iron Maiden", 63 )
                , ( "Slayer", 16 )
                , ( "Amon Amarth", 3 )
                , ( "Ghost", 48 )
                ]
            }
        )
        [ "78 ┤                                                                       "
        , "   │┌─────────────┐                                                        "
        , "   ││             │                                                        "
        , "   ││             │                                                        "
        , "   ││             │                                                        "
        , "62 ┤│             │                                                        "
        , "   ││             ├─────────────┐                                          "
        , "   ││             │             │                                          "
        , "   ││             │             │                                          "
        , "   ││             │             │                                          "
        , "47 ┤│             │             │                           ┌─────────────┐"
        , "   ││             │             │                           │             │"
        , "   ││             │             │                           │             │"
        , "   ││             │             │                           │             │"
        , "   ││             │             │                           │             │"
        , "31 ┤│             │             │                           │             │"
        , "   ││             │             │                           │             │"
        , "   ││             │             │                           │             │"
        , "   ││             │             │                           │             │"
        , "   ││             │             │                           │             │"
        , "16 ┤│             │             ├─────────────┐             │             │"
        , "   ││             │             │             │             │             │"
        , "   ││             │             │             │             │             │"
        , "   ││             │             │             │             │             │"
        , "   ││             │             │             ├─────────────┤             │"
        , "       Metallica    Iron Maiden     Slayer      Amon Amarth      Ghost     "
        ]

    assertPlots
        (Histogram
            { width  = 60
            , height = 24
            , bins   =
                [ ("1m₳", 0)
                , ("10m₳", 14)
                , ("100m₳", 32 )
                , ("1₳", 3 )
                , ("10₳", 29 )
                , ("100₳", 7 )
                ]
            }
        )
        [ "32 ┤                ┌───────┐                        "
        , "   │                │       │                        "
        , "   │                │       │       ┌───────┐        "
        , "   │                │       │       │       │        "
        , "27 ┤                │       │       │       │        "
        , "   │                │       │       │       │        "
        , "   │                │       │       │       │        "
        , "   │                │       │       │       │        "
        , "21 ┤                │       │       │       │        "
        , "   │                │       │       │       │        "
        , "   │                │       │       │       │        "
        , "   │                │       │       │       │        "
        , "16 ┤                │       │       │       │        "
        , "   │                │       │       │       │        "
        , "   │        ┌───────┤       │       │       │        "
        , "   │        │       │       │       │       │        "
        , "11 ┤        │       │       │       │       │        "
        , "   │        │       │       │       │       │        "
        , "   │        │       │       │       │       │        "
        , "   │        │       │       │       │       ├───────┐"
        , " 5 ┤        │       │       │       │       │       │"
        , "   │        │       │       │       │       │       │"
        , "   │        │       │       ├───────┤       │       │"
        , "   │ ───────┘       │       │       │       │       │"
        , "       1m₳    10m₳    100m₳    1₳      10₳    100₳   "
        ]

    assertPlots
        (Histogram
             { width  = 60
             , height = 24
             , bins   = []
             }
        )
        []

    assertPlots
        (Histogram
             { width  = 60
             , height = 0
             , bins   =
                [ ("a", 1)
                , ("b", 1)
                , ("c", 1)
                ]
             }
        )
        []

    assertPlots
        (Histogram
             { width  = 0
             , height = 24
             , bins   =
                [ ("a", 1)
                , ("b", 1)
                , ("c", 1)
                ]
             }
        )
        []

assertPlots :: Histogram -> [String] -> SpecWith ()
assertPlots src want = it (show src) $
    when (unlines want /= plot src) $ do
        putStrLn "GOT:  " *> putStrLn (plot src)
        putStrLn "WANT: " *> putStrLn (unlines want)
        expectationFailure "charts don't match"
