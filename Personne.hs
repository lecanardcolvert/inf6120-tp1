{- |
Module      :  Personne.hs
Description :  Module pour la gestion des personnes utilisant Smail.
Copyright   :  (c) Alexandre H. Bourdeau, HAMA12128907
License     :  GPL-3

Maintainer  :  alexandre.bourdeau@mailbox.org
Stability   :  experimental
Portability :  portable

Ce module offre les fonctionalités permettant de manipuler des personnes utilisant Smail. 
-}

module Personne where 

import Data.Char
import Data.List

type Nom        = String
type Prenom     = String
type Courriel   = String
type Signature  = (Prenom, Nom)
data Personne   = Personne 
                    Courriel 
                    Signature 
                    deriving (Show, Read) -- Courriel unique
instance Eq Personne where
    (==) (Personne cour1 _ ) (Personne cour2 _ ) = cour1 == cour2

-- | Retourne le courriel d'une personne
courriel :: Personne -> String
courriel (Personne c _)= c

-- | Vérifie si le courriel passé en paramètre est conforme. 
-- Un courriel smail conforme est de la forme <xxxx@smail.ca>
-- La partie xxxx ne peut contenir que des lettres en minuscules, des chiffres, des points ou des tirets '_' et '-' uniquement.
--
-- >>> courrielValide "tato.ange@samail.ca"
-- False
-- >>> courrielValide "ange-@tato@smail.ca"
-- False
-- >>> courrielValide "ang+-@tato@smail.ca"
-- False
-- >>> map courrielValide ["tatoooange@smail.ca", "ange.tato@smail.ca", "ange_tato@smail.ca", "Tato@smail.ca"]
-- [True,True,True,False]
courrielValide :: [Char] -> Bool
courrielValide [] = False
courrielValide x  = (length x >= 10 
                     && courrielPrefixeValide (take (length x - 9) x)
                     && courrielSuffixeValide (drop (length x - 9) x))

courrielPrefixeValide :: [Char] -> Bool
courrielPrefixeValide []        = True
courrielPrefixeValide (x:xs)    = (elem x ['a' .. 'z'] 
                                    || isDigit x 
                                    || elem x ['.', '-', '_'])
                                   && courrielPrefixeValide xs

courrielSuffixeValide :: [Char] -> Bool
courrielSuffixeValide [] = False
courrielSuffixeValide s = isSuffixOf "@smail.ca" s