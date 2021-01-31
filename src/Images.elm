module Images exposing (a, b, c, d, e, f, soundWave, view)

import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)


view : Svg msg -> Svg msg
view img =
    svg [ viewBox "0 0 100 100" ]
        [ img ]


a : Svg msg
a =
    svg [] []


b : Svg msg
b =
    svg [] []


c : Svg msg
c =
    Svg.path [ Attr.d "M60,10 C-10,0 -10,100 60,90" ] []


d : Svg msg
d =
    svg [] []


e : Svg msg
e =
    svg [] []


f : Svg msg
f =
    svg [] []


soundWave : Svg msg
soundWave =
    let
        ( wavelength, amplitude ) =
            ( 25, 25 )

        ( svgWidth, svgHeight ) =
            ( wavelength * 4, amplitude * 4 )

        xs =
            List.range 0 (wavelength + 1) |> List.map toFloat

        sinYs =
            List.map (\x -> (amplitude * 2) + (amplitude - 3) * sin (2 * pi * x / wavelength)) xs

        sinCoords =
            List.map2 Tuple.pair xs sinYs

        sinPath =
            List.foldl
                (\( x, y ) pathAcc -> pathAcc ++ " L" ++ String.fromFloat x ++ "," ++ String.fromFloat y)
                ""
                sinCoords
                |> String.dropLeft 2
                |> (\p -> "M" ++ p)

        cPath =
            "M60,10 C-10,0 -10,100 60,90"

        sinSVG =
            Svg.path
                [ Attr.d sinPath

                -- [ Attr.d cPath
                , Attr.stroke "black"
                , Attr.strokeWidth "2"
                ]
                [ Svg.animate
                    [ Attr.id "letterAnim1"
                    , from cPath
                    , to sinPath
                    , Attr.attributeName "d"
                    , Attr.attributeType "XML"
                    , Attr.dur "3s"
                    , Attr.fill "freeze"
                    ]
                    []
                ]

        animation =
            Svg.animateTransform
                [ Attr.id "anim1"
                , from (String.fromInt wavelength ++ ",0")
                , to "0,0"
                , Attr.attributeName "patternTransform"
                , Attr.type_ "translate"
                , Attr.dur "0.5s"
                , Attr.begin "0s"
                , Attr.fill "freeze"
                , Attr.repeatCount "indefinite"
                ]
                []

        pattern =
            Svg.defs []
                [ Svg.pattern
                    [ id "sinWave"
                    , Attr.x "0"
                    , Attr.y "0"
                    , Attr.height (String.fromInt svgHeight)
                    , Attr.width (String.fromInt <| wavelength)
                    , Attr.patternUnits "userSpaceOnUse"
                    ]
                    [ sinSVG

                    -- , animation
                    ]
                ]

        viewBox =
            "0 0 " ++ String.fromInt svgWidth ++ " " ++ String.fromInt svgHeight
    in
    Svg.svg
        [ Attr.viewBox viewBox ]
        [ -- pattern
          sinSVG
        , Svg.rect
            [ Attr.fill "url(#sinWave)"
            , Attr.width "100%"
            , Attr.height "100%"
            ]
            []
        ]
