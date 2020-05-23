module Router exposing (Route(..), urlToRoute)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, s, top)


type Route
    = Top
    | Game


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Top top
        , map Game (s "game")
        ]


urlToRoute : Url -> Maybe Route
urlToRoute url =
    Url.Parser.parse routeParser url
