{-# LANGUAGE GADTs #-}
-- |Validation of structured data typically entails traversal of a
-- structure to produce either a failure value indicating the specific
-- failure or a ''validated'' value which can be used for subsequent
-- computations.  This module provides a 'Rule' type for expressing
-- validation rules in such a way that the rules can be used to
-- generate documentation about validation requirements, while also
-- decoupling the rules from the structure of the validated data.
--
-- For example, a rule which checks that a string represents an
-- integer value would do the check and, if successful, would return
-- the integer value in question.  More complex rules can be built up
-- from simple rules by using the type classes instanced by 'Rule'.
module Data.Validation.Rules
    ( Rule
    -- * Creating Rules
    , rule
    , foreach
    , failRule
    -- * Applying Rules
    , apply
    -- * Pretty-Printing Rules
    , ruleDoc
    )
where

import Prelude hiding (id, (.))
import Control.Category
import Data.Either (lefts, rights)
import Data.Monoid (Monoid, mempty, mconcat)
import Text.PrettyPrint
import Control.Applicative

-- |Validation rules which yield validated data.  The idea is that a
-- rule encapsulates a validation process and also returns the data
-- which was validated.  'Rule's are parameterized on the type of
-- validation errors (@e@), the type of data structure type under
-- validation (@n@), and the rule result type (@a@).
data Rule e n a where
    -- Function application inside rules.
    Apply :: Rule e n (a -> b) -> Rule e n a -> Rule e n b

    -- Disjunction with exactly one match.
    OneOf :: [Rule e n a] -> Rule e n a

    -- User-defined rule with a description.
    Rule :: String -> (n -> Rule e n a) -> Rule e n a

    -- Embed a constant value in a rule.
    Pure :: a -> Rule e n a

    -- Signal a rule failure (in custom rules).
    Failed :: e -> Rule e n a

    -- Compose rules on structure type.
    Compose :: Rule e a b -> Rule e n a -> Rule e n b

    -- Iteration rule.
    Foreach :: Rule e a [b] -> Rule e b c -> Rule e a [c]

instance Functor (Rule e n) where
    fmap f r = Apply (Pure f) r

instance Applicative (Rule e n) where
    pure = Pure
    (<*>) = Apply

instance (Monoid e) => Alternative (Rule e n) where
    empty = Failed mempty
    (OneOf as) <|> (OneOf bs) = OneOf $ as ++ bs
    (OneOf as) <|> b = OneOf $ as ++ [b]
    a <|> (OneOf bs) = OneOf $ a : bs
    a <|> b = OneOf [a, b]

instance Category (Rule e) where
    id = Rule "the identity rule" (pure . id)
    r2 . r1 = Compose r2 r1

-- |For each element yielded by a rule, apply another rule.
foreach :: Rule e a [b] -> Rule e b c -> Rule e a [c]
foreach = Foreach

-- |A rule indicating failure with the given message.
failRule :: e -> Rule e n a
failRule = Failed

-- |Create a new rule with the given description and implementation.
-- /NOTE:/ Bear in mind that the behavior of the implementation should
-- precisely match the description so that the printed version of this
-- rule is sufficiently descriptive to the user of the data structure
-- under validation.
rule :: String -> (n -> Rule e n a) -> Rule e n a
rule = Rule

-- |Pretty-print a rule.  Note that the quality of the printed rule
-- depends largely on the quality of the descriptions of the rules
-- used within.
ruleDoc :: (Show e) => Rule e n a -> Doc
ruleDoc (Pure _) = text "Constant"
ruleDoc (Apply (Pure _) r1) = ruleDoc r1
ruleDoc (Apply r2 (Pure _)) = ruleDoc r2
ruleDoc (Apply r2 r1) = vcat [ ruleDoc r2
                             , ruleDoc r1
                             ]
ruleDoc (Failed e) = text $ "Failed: " ++ show e
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

-- |Apply a rule to an input value, yielding the value checked and
-- computed by the rule.
apply :: (Monoid e) => n -> Rule e n a -> Either e a
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
       else Left $ mconcat failures
apply n (Compose r2 r1) = do
  v <- apply n r1
  apply v r2
