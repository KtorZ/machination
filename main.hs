import Prelude

import Plot.Text.Histogram
    ( Histogram (..), plot )

main :: IO ()
main = putStrLn $ do
    plot $ Histogram
        { width  = 80
        , height = 24
        , bins   =
            [ ( "Metallica", 78 )
            , ( "Iron Maiden", 61 )
            , ( "Slayer", 16 )
            , ( "Dimmu Borgir", 3 )
            , ( "Ghost", 48 )
            ]
        }
