--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: MPL-2.0
-- Stability: experimental
--
-- Plot 'Histogram' as unicode strings; for terminal plotting.
-- For example, one could plot answers and their frequencies to the question:
--
-- /"Who's your favorite metal band?"/
--
-- > plot $ Histogram
-- >     { width  = 80
-- >     , height = 24
-- >     , bins   =
-- >         [ ( "Metallica", 78 )
-- >         , ( "Iron Maiden", 61 )
-- >         , ( "Slayer", 16 )
-- >         , ( "Dimmu Borgir", 3 )
-- >         , ( "Ghost", 48 )
-- >         ]
-- >     }
--
-- > 78 ┤┌─────────────┐
-- >    ││             │
-- >    ││             │
-- >    ││             │
-- >    ││             │
-- > 62 ┤│             │
-- >    ││             ├─────────────┐
-- >    ││             │             │
-- >    ││             │             │
-- >    ││             │             │
-- > 47 ┤│             │             │                            ┌─────────────┐
-- >    ││             │             │                            │             │
-- >    ││             │             │                            │             │
-- >    ││             │             │                            │             │
-- >    ││             │             │                            │             │
-- > 31 ┤│             │             │                            │             │
-- >    ││             │             │                            │             │
-- >    ││             │             │                            │             │
-- >    ││             │             │                            │             │
-- >    ││             │             │                            │             │
-- > 16 ┤│             │             ├─────────────┐              │             │
-- >    ││             │             │             │              │             │
-- >    ││             │             │             │              │             │
-- >    ││             │             │             │              │             │
-- >    ││             │             │             ├──────────────┤             │
-- >        Metallica    Iron Maiden     Slayer      Dimmu Borgir      Ghost
module Plot.Text.Histogram
    ( Histogram (..)
    , plot
    ) where

import Control.Applicative
    ( (<|>) )
import Data.List
    ( nub )
import Data.Maybe
    ( fromJust, fromMaybe )

-- | Model a distribution as an [Histogram](https://en.wikipedia.org/wiki/Histogram).
-- This representation works for continuous non-overlapping distributions. Axes
-- are labelled automatically based on the bins.
data Histogram = Histogram
    { width  :: !Int
    , height :: !Int
    , bins   :: ![(String, Int)]
    } deriving (Show, Read)

-- | Convert an 'Histogram' to 'String'
plot :: Histogram -> String
plot Histogram{width,height,bins}
    | height == 0 || width == 0 =
        ""
    | otherwise =
        unlines $ getPlot $ mconcat $ axis widthYAxis ys : (bar . mkBar <$> bins)
  where
    highest = maximum (snd <$> bins)
    nbLbls  = min (length bins) (height `div` 3)

    widthYAxis = maximum ((+3) . length <$> lbls)

    mkBar (label, x) = Bar
        { width  = 1 + (width - 2 - widthYAxis) `div` length bins
        , height = round (double height * double x / double highest)
        , label
        }

    lbls = nub [ show $ round (double highest * double i / double nbLbls)
               | i <- [ 1 .. nbLbls ]
               ]

    ys = Axis (round $ double height / double (length lbls) - 1) <$> lbls

-- | Represent a labelled vertical bar.
data Bar = Bar
    { width  :: Int
    , height :: Int
    , label  :: String
    }

bar :: Bar -> Plot
bar (Bar w h l)
    | h == 0 =
        Plot $ replicate w '─' : lbl

    | otherwise =
        Plot $ [ padMiddle w '─' "┌┐" ]
            ++ ( padMiddle w ' ' "││" .* (h - 1) )
            ++ lbl
  where
    lbl = [ padBoth w ' ' l ]

-- | Represent one mark on the y-axis.
data Axis = Axis
    { height :: Int
    , label  :: String
    }

axis :: Int -> [Axis] -> Plot
axis _ [] = Plot []
axis w es = Plot (concatMap each (reverse es) ++ [ replicate w ' ' ])
  where
    each :: Axis -> [String]
    each (Axis h l) =
        padLeft w ' ' (l <> " ┤ ") : replicate h (padLeft w ' ' "│ ")

--
-- Plot
--

-- | A plot represents a partial plot on the console which is modeled as a 2D
-- grid (a list of list of chars). Plots can be combined horizontally and merged
-- into a bigger plot. Edges are smoothen into more appropriate charaters.
--
-- For any plots of length N & M, the plot (N <> M) is of length (N + M - 1).
newtype Plot = Plot { getPlot :: [String] }

instance Semigroup Plot where
    Plot xs <> Plot ys
        | length xs > length ys =
            let ys' = fill (x - y) ys in Plot $ zipWith merge xs ys'

        | otherwise =
            let xs' = fill (y - x) xs in Plot $ zipWith merge xs' ys
      where
        (x, y) = (length xs, length ys)

        merge [] ys = ys
        merge xs [] = xs
        merge xs ys
            | match "││" = into "│"
            | match "│┌" = into "├"
            | match "│ " = into "│"
            | match "┐│" = into "┤"
            | match "┐┌" = into "┬"
            | match "┐ " = into "┐"
            | match " │" = into "│"
            | match " ┌" = into "┌"
            | match "─│" = into "┘"
            | match "│─" = into "└"
            | match "──" = into "─"
            | match "┬─" = into "┬"
            | match "┐─" = into "─"
            | match "─┌" = into "─"
            | otherwise  = into " "
          where
            match [a,b] = last xs == a && head ys == b
            into s = init xs ++ s ++ tail ys

instance Monoid Plot where
    mempty = Plot []

--
-- Helpers
--

infixl 5 .*

(.*) :: e -> Int -> [e]
(.*) = flip replicate

double :: Real i => i -> Double
double = fromRational . toRational

padRight :: Int -> e -> [e] -> [e]
padRight n e es =
    es ++ replicate (n - length es) e

padLeft :: Int -> e -> [e] -> [e]
padLeft n e es =
    replicate (n - length es) e ++ es

padBoth :: Int -> e -> [e] -> [e]
padBoth n e es
    | len >= n         = es
    | len `mod` 2 == 0 = padBoth n e $ padRight (len + 1) e es
    | otherwise        = padBoth n e $ padLeft  (len + 1) e es
  where
    len = length es

padMiddle :: Int -> e -> [e] -> [e]
padMiddle n e es =
    take half es ++ replicate δ e ++ drop half es
  where
    δ    = n - length es
    half = length es `div` 2

fill :: Int -> [String] -> [String]
fill n []    = replicate n mempty
fill n (h:q) = replicate n (replicate (length h) ' ') ++ (h:q)
