{-# OPTIONS_GHC -Wall #-}
module Main where

import Prelude hiding ((.))
import Control.Category ((.))
import Text.PrettyPrint (render)
import Control.Applicative

import Data.Validation.Rules

data Foo = Foo { fooContent :: Int }
           deriving (Show, Eq)

data Node = Node String Foo [Node]
            deriving (Show, Eq)

nodeVal :: Node -> String
nodeVal (Node s _ _) = s

childNodes :: Node -> [Node]
childNodes (Node _ _ ns) = ns

-- Rules.
getChild :: Int -> Rule Node Node
getChild num = rule ("Get child node " ++ show num) $
               \n -> if (length $ childNodes n) < num + 1
                     then failRule $ "Child " ++ show num ++ " not found"
                     else pure $ childNodes n !! num

children :: Rule Node [Node]
children = rule "Get child nodes" (pure . childNodes)

isIntNode :: Rule Node Int
isIntNode = rule "the node has an integer value" $
            \n -> case reads $ nodeVal n of
                    (v,""):_ -> pure v
                    _ -> failRule $ "Not an integer: " ++ (show $ nodeVal n)

isCharNode :: Rule Node Char
isCharNode = rule "the node has a char value" $
             \n -> if (length $ nodeVal n) == 1
                   then pure $ head $ nodeVal n
                   else failRule $ "Not a character: " ++ (show $ nodeVal n)

isStringNode :: Rule Node String
isStringNode = rule "the node has a string value" (pure . nodeVal)

hasChildren :: Int -> Rule Node ()
hasChildren num = rule ("The node has exactly " ++ show num ++ " children") $
                  \n -> if (length $ childNodes n) == num
                        then pure ()
                        else failRule $ show num ++ " children required"

fooRule :: Rule Foo Int
fooRule = rule "foo has content 5" $
          \foo -> if fooContent foo == 5
                  then pure $ fooContent foo
                  else failRule "fooContent is wrong"

main :: IO ()
main = do
  let t = Node "13" (Foo 1) [ Node "foo bar" (Foo 2) [Node "192" (Foo 3) []]
                            , Node "6" (Foo 4) [ Node "7" (Foo 5) []
                                               ]
                            ]
      getFoo = rule "get the foo value" $
               \(Node _ f _) -> pure f
      r = hasChildren 2 *> ((,,,,,)
                            <$> isIntNode
                            <*> ((fooRule . getFoo) <|> pure 1)
                            <*> isStringNode . getChild 0
                            <*> isIntNode . getChild 1
                            <*> ((nodeVal <$>) <$> children)
                            <*> (foreach children $ foreach children isIntNode)
                           )

  print t

  putStrLn "Rule:"
  putStrLn $ render $ ruleDoc r
  putStrLn ""

  case apply t r of
    Left e -> putStrLn $ "Rule application failed: " ++ e
    Right val -> do
         putStrLn "Data from applying rule:"
         print val