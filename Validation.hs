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
ruleDoc (Const _) = text "Constant"
ruleDoc (AllChildren r) = text "For each child:" $$ (nest 2 $ ruleDoc r)
ruleDoc (Apply (Const _) r1) = ruleDoc r1
ruleDoc (Apply r2 (Const _)) = ruleDoc r2
ruleDoc (Apply r2 r1) = (ruleDoc r2) $$ (ruleDoc r1)
ruleDoc (Child n r) = (text $ "Child " ++ show n ++ " satisfies:") $$ (nest 2 $ ruleDoc r)
ruleDoc (Failed msg) = text $ "Failed: " ++ show msg
ruleDoc (Custom desc _) = text desc
ruleDoc (OneOf rs) = text "One of these rules is satisfied:" $$ vcat ((nest 2 . ruleDoc) <$> rs)

-- Some example rules.
intNode :: Rule Int
intNode = Custom "the node has an integer value" $
          \n -> case reads $ nodeVal n of
                  (v,""):_ -> pure v
                  _ -> Failed $ "Not an integer: " ++ (show $ nodeVal n)

charNode :: Rule Char
charNode = Custom "the node has a char value" $
           \n -> if (length $ nodeVal n) == 1
                 then pure $ head $ nodeVal n
                 else Failed $ "Not a character: " ++ (show $ nodeVal n)

stringNode :: Rule String
stringNode = Custom "the node has a string value" (pure . nodeVal)

identity :: Rule Node
identity = Custom "the identity rule" (pure . id)

hasChildren :: Int -> Rule ()
hasChildren num = Custom ("The node has " ++ show num ++ " children") $
                  \n -> if (length $ children n) == num
                        then pure ()
                        else Failed $ show num ++ " children required"

getChild :: Node -> Int -> Either String Node
getChild n num
    | (length $ children n) < num + 1 =
        Left $ "Child " ++ show num ++ " not found"
    | otherwise = Right $ children n !! num

-- Apply a rule to a node, yielding the value checked and computed by
-- the rule.
apply :: Node -> Rule a -> Either String a
apply _ (Const a) = Right a
apply _ (Failed e) = Left e
apply n (Custom _ f) = apply n (f n)
apply n (Child num r) = do
  child <- getChild n num
  apply child r
apply n (Apply r2 r1) = do
  f <- apply n r2
  v <- apply n r1
  return $ f v
apply n (AllChildren r) = go [] (children n)
    where
      go vals [] = return vals
      go vals (c:cs) = do
        -- Fail on children for which the rule is not satisfied.
        val <- apply c r
        go (vals ++ [val]) cs
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
      rule = (,)
             <$> (hasChildren 2 *> intNode)
             <*> (AllChildren (Child 0 stringNode))

  print t

  putStrLn "Rule:"
  putStrLn $ show rule
  putStrLn ""

  case apply t rule of
    Left e -> putStrLn $ "Rule application failed: " ++ e
    Right val -> do
         putStrLn "Data from applying rule:"
         print val