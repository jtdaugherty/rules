{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
module Rules
    ( Rule(..)
    , apply
    , ruleDoc
    )
where

import Prelude hiding (id, (.))
import Control.Category
import Data.Either (lefts, rights)
import Data.List (intercalate)
import Text.PrettyPrint
import Control.Applicative

-- Validation rules which yield validated data.
data Rule n a where
    -- Function application inside rules.
    Apply :: Rule n (a -> b) -> Rule n a -> Rule n b

    -- Disjunction with exactly one match.
    OneOf :: [Rule n a] -> Rule n a

    -- User-defined rule with a description.
    Rule :: String -> (n -> Rule n a) -> Rule n a

    -- Embed a constant value in a rule.
    Pure :: a -> Rule n a

    -- Signal a rule failure (in custom rules).
    Failed :: String -> Rule n a

    -- Compose rules on structure type.
    Compose :: Rule a b -> Rule n a -> Rule n b

    -- Iteration rule.
    Foreach :: Rule a [b] -> Rule b c -> Rule a [c]

instance Functor (Rule n) where
    fmap f r = Apply (Pure f) r

instance Applicative (Rule n) where
    pure = Pure
    (<*>) = Apply

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
ruleDoc (Pure _) = text "Constant"
ruleDoc (Apply (Pure _) r1) = ruleDoc r1
ruleDoc (Apply r2 (Pure _)) = ruleDoc r2
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
apply _ (Pure a) = Right a
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
