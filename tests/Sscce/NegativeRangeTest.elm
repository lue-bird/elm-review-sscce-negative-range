module Sscce.NegativeRangeTest exposing (all)

import Review.Test
import Sscce.NegativeRange exposing (rule)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Sscce.NegativeRange"
        [ test "should replace \"replace\" in \"test replace test\" by \"a\" with non-inverted range" <|
            \() ->
                """module A exposing (..)
a = "test replace-non-inverted test"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "replacing the second word \"replace-non-inverted\" with \"a\""
                            , details = [ "Using Rule.replaceRangeBy with non-inverted start and end." ]
                            , under = "\"test replace-non-inverted test\""
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = "test a test"
"""
                        ]
        , test "try replace \"replace\" in \"test replace test\" by \"a\" with inverted range" <|
            \() ->
                """module A exposing (..)
a = "test replace test"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "negative replacing the second word \"replace\" with \"a\""
                            , details = [ "Using Rule.replaceRangeBy with inverted start and end." ]
                            , under = "\"test replace test\""
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = "test replace test"
"""
                        ]
        , test "try remove \"remove\" in \"test remove test\" with inverted range" <|
            \() ->
                """module A exposing (..)
a = "test remove test"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "negative removing the second word \"remove\""
                            , details = [ "Using Rule.removeRange with inverted start and end." ]
                            , under = "\"test remove test\""
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = "test remove test"
"""
                        ]
        ]
