module Examples (testAllExamples) where

import qualified Data.Map (fromList)
import Elaine.AST (Value (..))
import Elaine.Exec (Result (..), execRun, pack')
import System.Directory (listDirectory)
import Test.Hspec
  ( Expectation,
    SpecWith,
    describe,
    expectationFailure,
    hspec,
    it,
    runIO,
    shouldBe,
    shouldSatisfy,
  )
import Elaine.Pretty (Pretty(pretty))

expected :: String -> Value -> Bool
expected "abort.elaine" v = pretty v == "Maybe::Nothing()"
expected "basics.elaine" v = v == Int 8
expected "elab_transformed.elaine" v = v == String "1\n2\n1\n2\n"
expected "id.elaine" v = v == Int 5
expected "implicit.elaine" v = v == String "The answer is: 23"
expected "local_reader.elaine" v = v == Int 8
expected "local_reader_implicit.elaine" v = v == Int 8
expected "logic_once.elaine" v = v == String "False, True, True\n"
expected "logic.elaine" v = v == String "False, True, True\nFalse, True, False\n"
expected "match.elaine" v = v == String "5"
expected "safe_division.elaine" v = v == Int 5
expected "square_is_even.elaine" v = v == String "The square of 4 is even"
expected "state.elaine" v = v == Int 6
expected "structured_logging.elaine" v = v == String "main: msg1\nmain:foo: msg2\nmain:bar: msg3\n"
expected "ask.elaine" v = v == Int 32
expected "yield.elaine" v = v == String "2\n4\n"
expected "exception.elaine" v = pretty v == "Result::Ok(13)"
expected "std-while.elaine" v = v == Int 10
expected "std-while-state.elaine" v = v == Int 10
expected "std-repeat-state.elaine" v = v == Int 21
expected "list.elaine" v = v == Int 15
expected "parser_combinator.elaine" v = pretty v == "Maybe::Just(Json::JsonObject(List::Cons((\"key1\", Json::JsonInt(\"123\")), List::Cons((\"key2\", Json::JsonArray(List::Cons(Json::JsonInt(\"1\"), List::Cons(Json::JsonInt(\"2\"), List::Cons(Json::JsonInt(\"3\"), List::Nil()))))), List::Cons((\"key3\", Json::JsonString(\"some string\")), List::Nil())))))"
expected _ _ = error "Example does not have an expected value"

testAllExamples :: SpecWith ()
testAllExamples = describe "Text Examples" $ do
  examples <- runIO $ listDirectory "examples"

  -- We ignore examples starting with an underscore
  let examples' = filter (\s -> head s /= '_') examples

  mapM_ testExample examples'

run :: String -> String -> Result Value
run path = execRun . pack' . (,) path

testExample :: FilePath -> SpecWith ()
testExample p = do
  contents <- runIO $ readFile $ "examples/" ++ p
  it p $ do
    run p contents `shouldSatisfy` \case
      Right v -> expected p v
      _ -> False
