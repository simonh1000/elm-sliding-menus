module Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes as A


arrow =
    svg
        [ A.height "24"
        , A.viewBox "0 0 24 24"
        , A.width "24"
        ]
        [ path [ A.d "M8.59 16.34l4.58-4.59-4.58-4.59L10 5.75l6 6-6 6z" ] []
        , path [ A.d "M0-.25h24v24H0z", A.fill "none" ] []
        ]


arrow2 =
    svg
        [ A.fill "#000000"
        , A.height "24"
        , A.viewBox "0 0 24 24"
        , A.width "24"
        , A.transform "rotate(180)"
        ]
        [ path [ A.d "M8.59 16.34l4.58-4.59-4.58-4.59L10 5.75l6 6-6 6z" ] []
        , path [ A.d "M0-.25h24v24H0z", A.fill "none" ] []
        ]
