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

children :: Node -> [Node]
children (Node _ _ ns) = ns

-- Validation rules which yield validated data.
data Rule n a where
    -- Function application inside rules.
    Apply :: Rule n (a -> b) -> Rule n a -> Rule n b

    -- Disjunction with exactly one match.
    OneOf :: [Rule n a] -> Rule n a

    -- Custom rule with a description.
    Custom :: String -> (n -> Rule n a) -> Rule n a

    -- Embed a constant value in a rule.
    Const :: a -> Rule n a

    -- Signal a rule failure (in custom rules).
    Failed :: String -> Rule n a

    -- Compose rules on structure type.
    Compose :: Rule a b -> Rule n a -> Rule n b

    -- Iteration rule.
    Foreach :: Rule a [b] -> Rule b c -> Rule a [c]

instance Show (Rule n a) where
    show = render . ruleDoc

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
    id = Custom "the identity rule" (pure . id)
    r2 . r1 = Compose r2 r1

ruleDoc :: Rule n a -> Doc
ruleDoc (Const _) = text "Constant"
ruleDoc (Apply (Const _) r1) = ruleDoc r1
ruleDoc (Apply r2 (Const _)) = ruleDoc r2
ruleDoc (Apply r2 r1) = (ruleDoc r2) $$ (ruleDoc r1)
ruleDoc (Failed msg) = text $ "Failed: " ++ show msg
ruleDoc (Custom desc _) = text desc
ruleDoc (OneOf rs) = text "One of these rules is satisfied:" $$ vcat ((nest 2 . ruleDoc) <$> rs)
ruleDoc (Compose r2 r1) = text "compose" <+> (ruleDoc r2) $$ (nest 2 $ text "with" <+> ruleDoc r1)
ruleDoc (Foreach things r) = text "for each of" $$ (nest 2 $ ruleDoc things) $$ text "satisfy" $$ (nest 2 $ ruleDoc r)

-- Some example rules.
intNode :: Rule Node Int
intNode = Custom "the node has an integer value" $
          \n -> case reads $ nodeVal n of
                  (v,""):_ -> pure v
                  _ -> Failed $ "Not an integer: " ++ (show $ nodeVal n)

charNode :: Rule Node Char
charNode = Custom "the node has a char value" $
           \n -> if (length $ nodeVal n) == 1
                 then pure $ head $ nodeVal n
                 else Failed $ "Not a character: " ++ (show $ nodeVal n)

stringNode :: Rule Node String
stringNode = Custom "the node has a string value" (pure . nodeVal)

hasChildren :: Int -> Rule Node ()
hasChildren num = Custom ("The node has exactly " ++ show num ++ " children") $
                  \n -> if (length $ children n) == num
                        then pure ()
                        else Failed $ show num ++ " children required"

fooRule :: Rule Foo Int
fooRule = Custom "foo has content 5" $
          \foo -> if fooContent foo == 5
                  then pure $ fooContent foo
                  else Failed "fooContent is wrong"

-- Apply a rule to a node, yielding the value checked and computed by
-- the rule.
apply :: n -> Rule n a -> Either String a
apply _ (Const a) = Right a
apply _ (Failed e) = Left e
apply n (Custom _ f) = apply n (f n)
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

getChild :: Int -> Rule Node Node
getChild num = Custom ("Get child node " ++ show num) $
               \n -> if (length $ children n) < num + 1
                     then Failed $ "Child " ++ show num ++ " not found"
                     else pure $ children n !! num

getChildren :: Rule Node [Node]
getChildren = Custom "Get child nodes" (pure . children)

main :: IO ()
main = do
  let t = Node "13" (Foo 1) [ Node "foo bar" (Foo 2) [Node "192" (Foo 3) []]
                            , Node "6" (Foo 4) [ Node "7" (Foo 5) []
                                               ]
                            ]
      getFoo = Custom "get the foo value" $
               \(Node _ f _) -> Const f
      rule = hasChildren 2 *> ((,,,,,)
                               <$> intNode
                               <*> ((fooRule . getFoo) <|> Const 1)
                               <*> stringNode . getChild 0
                               <*> intNode . getChild 1
                               <*> ((nodeVal <$>) <$> getChildren)
                               <*> (Foreach getChildren $ Foreach getChildren intNode)
                              )

  print t

  putStrLn "Rule:"
  putStrLn $ show rule
  putStrLn ""

  case apply t rule of
    Left e -> putStrLn $ "Rule application failed: " ++ e
    Right val -> do
         putStrLn "Data from applying rule:"
         print val