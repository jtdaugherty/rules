{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Main where

import Prelude hiding (id, (.))
import Control.Category
import Data.Either (lefts, rights)
import Data.List (intercalate)
import Text.PrettyPrint
import Control.Applicative hiding (Const)

data Foo = Foo { fooContent :: Int }
           deriving (Show, Eq)

data Node = Node String Foo [Node]
            deriving (Show, Eq)

nodeVal :: Node -> String
nodeVal (Node s _ _) = s

childNodes :: Node -> [Node]
childNodes (Node _ _ ns) = ns

-- Validation rules which yield validated data.
data Rule n a where
    -- Function application inside rules.
    Apply :: Rule n (a -> b) -> Rule n a -> Rule n b

    -- Disjunction with exactly one match.
    OneOf :: [Rule n a] -> Rule n a

    -- User-defined rule with a description.
    Rule :: String -> (n -> Rule n a) -> Rule n a

    -- Embed a constant value in a rule.
    Const :: a -> Rule n a

    -- Signal a rule failure (in custom rules).
    Failed :: String -> Rule n a

    -- Compose rules on structure type.
    Compose :: Rule a b -> Rule n a -> Rule n b

    -- Iteration rule.
    Foreach :: Rule a [b] -> Rule b c -> Rule a [c]

instance Functor (Rule n) where
    fmap f r = Apply (Const f) r

instance Applicative (Rule n) where
    pure = Const
    r2 <*> r1 = Apply r2 r1

instance Alternative (Rule n) where
    empty = Failed "empty"
    (OneOf as) <|> (OneOf bs) = OneOf $ as ++ bs
    (OneOf as) <|> b = OneOf $ as ++ [b]
    a <|> (OneOf bs) = OneOf $ a : bs
    a <|> b = OneOf [a, b]

instance Category Rule where
    id = Rule "the identity rule" (pure . id)
    r2 . r1 = Compose r2 r1

ruleDoc :: Rule n a -> Doc
ruleDoc (Const _) = text "Constant"
ruleDoc (Apply (Const _) r1) = ruleDoc r1
ruleDoc (Apply r2 (Const _)) = ruleDoc r2
ruleDoc (Apply r2 r1) = vcat [ ruleDoc r2
                             , ruleDoc r1
                             ]
ruleDoc (Failed msg) = text $ "Failed: " ++ show msg
ruleDoc (Rule desc _) = text desc
ruleDoc (OneOf rs) = vcat [ text "One of these rules is satisfied:"
                          , vcat $ (nest 2 . ruleDoc) <$> rs
                          ]
ruleDoc (Compose r2 r1) = vcat [ text "compose" <+> (ruleDoc r2)
                               , nest 2 $ text "with" <+> ruleDoc r1
                               ]
ruleDoc (Foreach things r) = vcat [ text "for each of"
                                  , nest 2 $ ruleDoc things
                                  , text "satisfy"
                                  , nest 2 $ ruleDoc r
                                  ]

-- Apply a rule to a node, yielding the value checked and computed by
-- the rule.
apply :: n -> Rule n a -> Either String a
apply _ (Const a) = Right a
apply _ (Failed e) = Left e
apply n (Rule _ f) = apply n (f n)
apply n (Apply r2 r1) = do
  f <- apply n r2
  v <- apply n r1
  return $ f v
apply n (Foreach things r) = do
  values <- apply n things
  mapM (flip apply r) values
apply n (OneOf rs) =
    let results = apply n <$> rs
        successes = rights results
        failures = lefts results
    in if not $ null successes
       then Right $ head successes
       else Left $ "No rules matched: " ++ intercalate ", " failures
apply n (Compose r2 r1) = do
  v <- apply n r1
  apply v r2

-- Rules.
getChild :: Int -> Rule Node Node
getChild num = Rule ("Get child node " ++ show num) $
               \n -> if (length $ childNodes n) < num + 1
                     then Failed $ "Child " ++ show num ++ " not found"
                     else pure $ childNodes n !! num

children :: Rule Node [Node]
children = Rule "Get child nodes" (pure . childNodes)

isIntNode :: Rule Node Int
isIntNode = Rule "the node has an integer value" $
            \n -> case reads $ nodeVal n of
                    (v,""):_ -> pure v
                    _ -> Failed $ "Not an integer: " ++ (show $ nodeVal n)

isCharNode :: Rule Node Char
isCharNode = Rule "the node has a char value" $
             \n -> if (length $ nodeVal n) == 1
                   then pure $ head $ nodeVal n
                   else Failed $ "Not a character: " ++ (show $ nodeVal n)

isStringNode :: Rule Node String
isStringNode = Rule "the node has a string value" (pure . nodeVal)

hasChildren :: Int -> Rule Node ()
hasChildren num = Rule ("The node has exactly " ++ show num ++ " children") $
                  \n -> if (length $ childNodes n) == num
                        then pure ()
                        else Failed $ show num ++ " children required"

fooRule :: Rule Foo Int
fooRule = Rule "foo has content 5" $
          \foo -> if fooContent foo == 5
                  then pure $ fooContent foo
                  else Failed "fooContent is wrong"

main :: IO ()
main = do
  let t = Node "13" (Foo 1) [ Node "foo bar" (Foo 2) [Node "192" (Foo 3) []]
                            , Node "6" (Foo 4) [ Node "7" (Foo 5) []
                                               ]
                            ]
      getFoo = Rule "get the foo value" $
               \(Node _ f _) -> pure f
      rule = hasChildren 2 *> ((,,,,,)
                               <$> isIntNode
                               <*> ((fooRule . getFoo) <|> pure 1)
                               <*> isStringNode . getChild 0
                               <*> isIntNode . getChild 1
                               <*> ((nodeVal <$>) <$> children)
                               <*> (Foreach children $ Foreach children isIntNode)
                              )

  print t

  putStrLn "Rule:"
  putStrLn $ render $ ruleDoc rule
  putStrLn ""

  case apply t rule of
    Left e -> putStrLn $ "Rule application failed: " ++ e
    Right val -> do
         putStrLn "Data from applying rule:"
         print val