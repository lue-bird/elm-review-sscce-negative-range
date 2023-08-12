module Sscce.NegativeRange exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location, Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| -}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "Sscce.NegativeRange" initialContext
        |> Rule.providesFixesForModuleRule
        |> Rule.withExpressionEnterVisitor
            (\expressionNode context ->
                ( expressionVisitor expressionNode context
                , context
                )
            )
        |> Rule.fromModuleRuleSchema


type alias Context =
    {}


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\() ->
            {}
        )


expressionVisitor : Node Expression -> Context -> List (Rule.Error {})
expressionVisitor expressionNode context =
    let
        range =
            expressionNode |> Node.range

        rangeOfWordBetweenTest word =
            { start = range.start |> columnAdd 6
            , end = range.start |> columnAdd (6 + (word |> String.length))
            }
    in
    case expressionNode |> Node.value of
        Expression.Literal "test replace-non-inverted test" ->
            [ Rule.errorWithFix
                { message = "replacing the second word \"replace-non-inverted\" with \"a\""
                , details = [ "Using Rule.replaceRangeBy with non-inverted start and end." ]
                }
                range
                [ Fix.replaceRangeBy
                    (rangeOfWordBetweenTest "replace-non-inverted")
                    "a"
                ]
            ]

        Expression.Literal "test replace test" ->
            [ Rule.errorWithFix
                { message = "negative replacing the second word \"replace\" with \"a\""
                , details = [ "Using Rule.replaceRangeBy with inverted start and end." ]
                }
                range
                [ Fix.replaceRangeBy
                    (rangeOfWordBetweenTest "replace" |> rangeInvert)
                    "a"
                ]
            ]

        Expression.Literal "test remove test" ->
            [ Rule.errorWithFix
                { message = "negative removing the second word \"remove\""
                , details = [ "Using Rule.removeRange with inverted start and end." ]
                }
                range
                [ Fix.removeRange
                    (rangeOfWordBetweenTest "remove" |> rangeInvert)
                ]
            ]

        _ ->
            []


columnAdd : Int -> Location -> Location
columnAdd columnIncrease =
    \location ->
        { location | column = location.column + columnIncrease }


rangeInvert : Range -> Range
rangeInvert =
    \range -> { start = range.end, end = range.start }
