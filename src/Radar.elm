module Radar exposing (Points, radar)

import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)


type alias Points =
    List Point


type alias Point =
    { name : String
    , value : Float
    }


chartSize =
    100


numberOfScales : Float
numberOfScales =
    7


radar : Points -> Svg msg
radar points =
    svg [ viewBox "0 0 100 100", class "radar" ]
        [ spokes
        , poly data
        ]


spokes : Svg msg
spokes =
    g []
        (List.map (toFloat >> radarScale) (List.range 1 (round numberOfScales) |> List.reverse))


radarScale : Float -> Svg msg
radarScale scale =
    g []
        [ circle
            [ cx "50"
            , cy "50"
            , r <| String.fromFloat ((scale / numberOfScales * chartSize) / 2)
            , fill "#FAFAFA"
            , stroke "#999"
            , strokeWidth "0.2"
            ]
            []
        ]
