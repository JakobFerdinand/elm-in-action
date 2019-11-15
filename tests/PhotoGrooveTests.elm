module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (Model, Msg(..), initialModel, update)
import Test exposing (..)


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (unitiled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitled)")


sliders : Test
sliders =
    describe "Slider sets the desird field in the Model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidNoise" SlidNoise .noise
        , testSlider "SlidRipple" SlidRipple .ripple
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount
