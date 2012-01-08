{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.List (intercalate)
import Text.PrettyPrint
import Control.Applicative hiding (Const)

data Node = Node String [Node]
            deriving (Show, Eq)

nodeVal :: Node -> String
nodeVal (Node s _) = s

children :: Node -> [Node]
children (Node _ ns) = ns

-- Validation rules where rules can yield values of type 'a'.
data Rule a where
    -- Check nodes whose values can be encoded in the specified types.
    IntNode :: Rule Int
    CharNode :: Rule Char
    StringNode :: Rule String

    -- Check that the number of children is as expected.
    HasChildren :: Int -> Rule ()

    -- Require that all children of a node satisfy the specified rule.
    AllChildren :: Rule a -> Rule [a]

    -- Apply a rule to the specified child.
    Child :: Int -> Rule a -> Rule a

    -- Function application inside rules.
    Apply :: Rule (a -> b) -> Rule a -> Rule b

    -- Custom rule with a description.
    Custom :: String -> (Node -> Rule a) -> Rule a

    -- Embed a constant value in a rule.
    Const :: a -> Rule a

    -- Signal a rule failure (in custom rules).
    Failed :: String -> Rule a

    -- Disjunction with exactly one match.
    OneOf :: [Rule a] -> Rule a

    -- Node identity.
    Id :: Rule Node

instance Show (Rule a) where
    show = render . ruleDoc

instance Functor Rule where
    fmap f r = Apply (Const f) r

instance Applicative Rule where
    pure = Const
    r2 <*> r1 = Apply r2 r1

instance Alternative Rule where
    empty = Failed "empty"
    (OneOf as) <|> (OneOf bs) = OneOf $ as ++ bs
    (OneOf as) <|> b = OneOf $ as ++ [b]
    a <|> (OneOf bs) = OneOf $ a : bs
    a <|> b = OneOf [a, b]

ruleDoc :: Rule a -> Doc
ruleDoc Id = text "the identity rule"
ruleDoc IntNode = text "The node has an integer value"
ruleDoc CharNode = text "The node has a char value"
ruleDoc StringNode = text "The node has a string value"
ruleDoc (Const _) = text "Constant"
ruleDoc (HasChildren n) = text $ "The node has " ++ show n ++ " children"
ruleDoc (AllChildren r) = text "For each child:" $$ (nest 2 $ ruleDoc r)
ruleDoc (Apply (Const _) r1) = ruleDoc r1
ruleDoc (Apply r2 (Const _)) = ruleDoc r2
ruleDoc (Apply r2 r1) = (ruleDoc r2) $$ (ruleDoc r1)
ruleDoc (Child n r) = (text $ "Child " ++ show n ++ " satisfies:") $$ (nest 2 $ ruleDoc r)
ruleDoc (Failed msg) = text $ "Failed: " ++ show msg
ruleDoc (Custom desc _) = text desc
ruleDoc (OneOf rs) = text "One of these rules is satisfied:" $$ vcat ((nest 2 . ruleDoc) <$> rs)

-- Apply a rule to a node, yielding the value checked and computed by
-- the rule.
apply :: Node -> Rule a -> Either String a
apply n Id = Right n
apply n IntNode = case reads $ nodeVal n of
                    (v,""):_ -> Right v
                    _ -> Left $ "Not an integer: " ++ (show $ nodeVal n)
apply n CharNode = if (length $ nodeVal n) == 1
                   then Right $ head $ nodeVal n
                   else Left $ "Not a character: " ++ (show $ nodeVal n)
apply n StringNode = Right $ nodeVal n
apply _ (Const a) = Right a
apply n (HasChildren num) = if (length $ children n) == num
                            then Right ()
                            else Left $ show num ++ " children required"
apply n (AllChildren r) = go [] (children n)
    where
      go vals [] = Right vals
      go vals (c:cs) = case apply c r of
                         Left e -> Left e
                         Right val -> go (vals ++ [val]) cs
apply n (Child num r) =
    if ((length $ children n) - 1) < num
    then Left $ "Child " ++ show num ++ " not found"
    else let child = children n !! num
         in apply child r
apply n (Apply r2 r1) =
    case apply n r2 of
      Left e -> Left e
      Right f -> case apply n r1 of
                   Left e' -> Left e'
                   Right v -> Right $ f v
apply n (Custom _ f) = apply n (f n)
apply _ (Failed e) = Left e
apply n (OneOf rs) = go [] rs
    where
      go es [] = Left $ "No rules matched: " ++ intercalate ", " es
      go es (r:rs') = case apply n r of
                       Left e -> go (es++[e]) rs'
                       Right v -> Right v

main :: IO ()
main = do
  let t = Node "13" [ Node "foo bar" [Node "flurb" []]
                    , Node "6" [ Node "7" []
                               ]
                    ]
      rule = (,,)
             <$> (HasChildren 2 *> IntNode)
             <*> (AllChildren (Child 0 StringNode))
             <*> (AllChildren (Child 0 StringNode))

  print t

  putStrLn "Rule:"
  putStrLn $ show rule
  putStrLn ""

  case apply t rule of
    Left e -> putStrLn $ "Rule application failed: " ++ e
    Right val -> do
         putStrLn "Data from applying rule:"
         print val