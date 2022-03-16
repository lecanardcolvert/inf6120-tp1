{- |
Module      :  CompteSmail.hs
Description :  Module pour la gestion des comptes de la messagerie SmartMail.
Copyright   :  (c) Ange Tato, adapté par Alexandre H. Bourdeau, HAMA12128907
License     :  GPL-3

Maintainer  :  nyamen_tato.ange_adrienne@uqam.ca, alexandre.bourdeau@mailbox.org
Stability   :  experimental
Portability :  portable

Ce module offre les fonctionalités permettant de manipuler les comptes SmartMail. 
-}

module CompteSmail where 

import qualified Data.List as List
import Personne
import Trame

type Contact        = (Personne, Etat)
type Explications   = String 
data Etat           = Noir | Blanc deriving (Show, Eq, Read) -- Noir = contact dans liste noire (bloqué), Blanc contact non bloqué
type Reception      = [Trame] -- Boîte de reception
type Envoi          = [Trame] -- Boîte d'envoi
type Spams          = [(Trame, Explications)] -- Boîte des spams
type Preferences    = [Trame -> Bool]
data CompteSmail    = CompteSmail
                        Personne
                        Reception
                        Envoi
                        Spams
                        Preferences
                        [Contact] -- Compte smail

instance Show CompteSmail where
    show (CompteSmail p r e s pr c) = ligne1 ++ ligne2 ++ ligne3 ++ ligne4 ++ ligne5
        where
            ligne1 = "CompteSmail " ++ show (courriel p) ++ ":"
            ligne2 = "\nRecus = " ++ show r ++ ","
            ligne3 = "\nEnvois = " ++ show e ++ ","
            ligne4 = "\nSpams = " ++ show s ++ ","
            ligne5 = "\nContacts = " ++ show (listeContacts (c))

listeContacts [] = []
listeContacts ((p, e):xs) = (courriel p, e):listeContacts xs


-- | Retourne la personne à qui appartient le compte Smail
personne :: CompteSmail -> Personne
personne (CompteSmail p _ _ _ _ _) = p


-- | Retourne la liste des messages de la boîte  de reception d'un compte Smail
reception :: CompteSmail -> Reception 
reception (CompteSmail _ r _ _ _ _) = r


-- | Retourne la liste des messages de la boîte d'envoi d'un compte Smail
envoi :: CompteSmail -> Envoi
envoi (CompteSmail _ _ e _ _ _) = e  


-- | Retourne la liste des messages spams d'un compte Smail
spams :: CompteSmail -> Spams
spams (CompteSmail _ _ _ s _ _) = s


-- | Retourne la liste des préférences d'un compte Smail
--
-- filtres ou contraintes imposés par le titulaire d'un compte smail 
-- exemple: je ne veux aucun message dont le courriel de l'expéditeur se termine pas ".zz"
--          si la préférence n'est pas satisfaite, le message est automatiquement redirigé dans la boîte des spams
preferences :: CompteSmail  -> Preferences
preferences (CompteSmail _ _ _ _ pr _) = pr


-- | Retourne la liste de tous les contacts d'un compte Smail
contacts :: CompteSmail  -> [Contact]
contacts (CompteSmail _ _ _ _ _ c) = c 


-------------------------------------------------------------------
--------------------------NE PAS MODIFIER--------------------------
-------------------------------------------------------------------

-- Quelques données utilisées dans les tests
pers0 = Personne "equipesmartmail@smail.ca" ("equipe","smail")
pers1 = Personne "tato.ange@smail.ca" ("ange","tato")
pers2 = Personne "nkambou.roger@smail.ca" ("roger","nkambou")
pers3 = Personne "robert.julien@smail.ca" ("julien","robert")
pers4 = Personne "noel.alice@smail.ca" ("alice","noel")
pers5 = Personne "bourassa.alex@smail.ca" ("alex","bourassa")
pers6 = Personne "ariane.carotte@techno.co" ("arianne","carotte")
pers7 = Personne "pablo.adamo@blob.es" ("olivier","adam")
pers8 = Personne "michel.desrosiers@blob.ca" ("michel","desrosiers")
pers9 = Personne "mimi.lafleur@smail.ca" ("mimi","lafleur")
pers10 = Personne "adam.ronelle@smail.ca" ("Adam", "Ronelle")
pers11 = Personne "gabrielle.joyce@smail.ca" ("Gabrielle", "Joyce")
pers12 = Personne "marsu.pilami@smail.ca" ("Marsu", "Pilami")
pers13 = Personne "satan.peticoeur@smail.ca" ("Satan", "Peticoeur")

-- Exemples de trame de message
trameBienvenue1 = (Trame (Entete (Date 2021 02 10) "Bienvenue" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trameBienvenue2 = (Trame (Entete (Date 2021 02 10) "Bienvenue" pers0 [pers2] [] []) "Bienvenue dans votre boite smartMail !")
trame1 = (Trame (Entete (Date 2021 01 18) "AB CD EF" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trame2 = (Trame (Entete (Date 2020 12 21) "Bi!en! venue!" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trame3 = (Trame (Entete (Date 2021 01 01) "?Bien venue?" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !")  
trame4 = (Trame (Entete (Date 2019 10 05) "" pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trame5 = (Trame (Entete (Date 2018 05 07) "Bienvenue $ " pers0 [pers1] [] []) "Bienvenue dans votre boite smartMail !") 
trame6 = (Trame (Entete (Date 2019 05 09) "Bienvenue" pers1 [pers3] [] []) "Allo Robert") 
trame7 = (Trame (Entete (Date 2020 18 10) "un message" pers0 [pers1] [] []) "Bienvenue dans sexe du viagra chaud nu") 
trame8 = (Trame (Entete (Date 2021 01 15) "un message" pers0 [pers1] [] []) "offre de Bienvenue dans  publicite gratuit pour voyage special") 
trame9 = (Trame (Entete (Date 2021 10 10) "un message" pers0 [pers1] [] []) "0de 3Bienvenue dans 1 et 9 | pour tp1et2") 
trame10 = (Trame (Entete (Date 2021 02 01) "un message" pers0 [pers1] [] []) "bien1venue") 
trame11 = (Trame (Entete (Date 2021 10 18) "un message" pers0 [pers1] [] []) "allo|allo") 
trame12 = (Trame (Entete (Date 2020 11 17) "Salut ange" pers3 [pers1] [] []) "Salut Ange tu vas bien ?") 
trame13 = (Trame (Entete (Date 2021 01 18) "Salut roger" pers3 [pers2] [] []) "special voyage demain, viens vite" ) 
trame14 = (Trame (Entete (Date 2021 03 18) "Hola mimi" pers7 [pers9] [] []) "como estas ?" ) 
trame15 = (Trame (Entete (Date 2021 02 09) "Bingo" pers6 [pers9] [] []) "J'ai trouve ce que tu cherchais hier" ) 
trame16 = (Trame (Entete (Date 2021 01 07) "Par rapport a Ivan" pers8 [pers9] [] []) "Ivan ne viendra pas demain ?" ) 
trame17 = (Trame (Entete (Date 2022 02 14) "Je suis le prince de Namek" pers13 [pers1] [pers10] [pers11, pers12]) "Je suis satan petit coeur et je viens de la planete Namek." ) 

tramet = (Trame (Entete (Date 2021 01 13) "Cool" pers4 [pers5] [] []) "Allo Alex") 
tramett = (Trame (Entete (Date 2021 01 25) "Nouvelles" pers2 [pers5] [] []) "Tu vas bien ?") 

-- Exemples de compte smail
csmail0 = CompteSmail pers0 [] [trameBienvenue1, trameBienvenue2] [] [] []
csmail1 = CompteSmail pers1 [trameBienvenue1] [] [] [] [(pers2,Blanc),(pers4,Noir)]
csmail2 = CompteSmail pers2 [trameBienvenue2] [] [] [] []
csmail3 = CompteSmail pers3  [] [] [] [] []
csmail4 = CompteSmail pers4  [] [] [] [] [(pers1,Blanc)] 
csmail5 = CompteSmail pers5  [] [] [] [] [(pers1,Blanc)]
csmail6 = CompteSmail pers9  [] [] [] [\(Trame (Entete d ob p1 _ _ _) c) -> (tail $ dropWhile (/='.') (dropWhile (/='@') (courriel p1))) /= "es" ] []

csmail21 = CompteSmail pers10  [] [] [] [] [(pers11,Blanc), (pers13,Blanc)] 
csmail22 = CompteSmail pers11  [] [] [] [] [(pers10,Blanc),(pers12,Noir),(pers13,Noir)] 
csmail23 = CompteSmail pers12  [] [] [] [] [] 
csmail24 = CompteSmail pers13  [] [] [] [] [] 

-------------------------------------------------------------------
-----------------FIN DE LA ZONE À NE PAS MODIFIER------------------
-------------------------------------------------------------------


-- | Ajouter un contact
--
-- Les paramètres sont : les informations du contact à ajouter, et le compte à modifier
-- Note1: De base, un contact est ajouté avec l'état = Blanc et en entête de la liste de contacts. 
-- Note2: Pas besoin de vérifier si le courriel est bon ou pas car le courriel passé en paramètre sera vérifié (pas par vous) avant d'être envoyé à cette fonction.
-- Note3: Vous devez vous assurez que le contact n'existe pas déjà
--
-- >>> csmail4' = ajouterContact "robert.julien@smail.ca" "julien" "robert" csmail4
-- >>> csmail4'
-- CompteSmail "noel.alice@smail.ca":
-- Recus = [],
-- Envois = [],
-- Spams = [],
-- Contacts = [("robert.julien@smail.ca",Blanc),("tato.ange@smail.ca",Blanc)]
ajouterContact :: Courriel -> Prenom -> Nom -> CompteSmail -> CompteSmail 
ajouterContact cc cp cn (CompteSmail csp csr cse css cspr csc) = (CompteSmail csp csr cse css cspr csc')
    where
        p = Personne cc (cp, cn)
        e = Blanc
        c = (p, e)
        exis = filter (\(x, _) -> courriel x == cc) csc
        csc' = case exis of 
            []  ->  [c] ++ csc
            _   ->  csc


-- | Bloquer un contact
--
-- Les paramètres sont : le compte à modifier, le contact à bloquer, le compte modifié
--
-- >>> csmail4'' = ajouterContact "robert.julien@smail.ca" "julien" "robert" $ ajouterContact "bourassa.alex@smail.ca" "alex" "bourassa" csmail4
-- >>> bloquerContact csmail4'' pers5  
-- CompteSmail "noel.alice@smail.ca":
-- Recus = [],
-- Envois = [],
-- Spams = [],
-- Contacts = [("robert.julien@smail.ca",Blanc),("bourassa.alex@smail.ca",Noir),("tato.ange@smail.ca",Blanc)]
bloquerContact :: CompteSmail -> Personne -> CompteSmail 
bloquerContact (CompteSmail csp csr cse css cspr csc) p = (CompteSmail csp csr cse css cspr csc')
    where
        leng = length csc
        indc = List.findIndex (\(x, _) -> x == p) csc
        cont = (p, Noir)
        csc' = case indc of
            Just indx   ->  (take indx csc) ++ [cont] ++ (drop (leng - indx) csc)
            Nothing     ->  csc


-- | Supprimer messages de la boîte de reception, d'envoi ou de spams d'un compte en fonction d'un filtre.
--
-- Tous les messages passant le filtre doivent être supprimés de la boîte spécifié.
-- Les paramètres : Le compte à vider, le type de la boîte : Spams, Envoi ou Reception, un filtre, le compte modifié
--
-- >>> csmail4_1 = CompteSmail (Personne "noel.alice@smail.ca" ("alice","noel")) [trame3,trame4] [trame5,trame6] [(trame1,"majuscules"),(trame2,"points d'exclamation")] [] [(Personne "bourassa.alex@smail.ca" ("alex","bourassa"),Blanc),(Personne "robert.julien@smail.ca" ("julien","robert"),Blanc)]
-- >>> reception $ supprimerMessagesAvecFiltre csmail4_1 "Reception" (\x -> elem '?' $ objet x)
-- [Trame (Entete (Date 2019 10 5) "" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !"]
-- >>> envoi $ supprimerMessagesAvecFiltre csmail4_1 "Envoi" (\x -> annee (date x) == 2018)
-- [Trame (Entete (Date 2019 5 9) "Bienvenue" (Personne "tato.ange@smail.ca" ("ange","tato")) [Personne "robert.julien@smail.ca" ("julien","robert")] [] []) "Allo Robert"]
-- >>> supprimerMessagesAvecFiltre csmail4_1 "Spams" (\x -> all isUpper (filter isAlpha $ objet x))
-- CompteSmail "noel.alice@smail.ca":
-- Recus = [Trame (Entete (Date 2021 1 1) "?Bien venue?" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !",Trame (Entete (Date 2019 10 5) "" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !"],
-- Envois = [Trame (Entete (Date 2018 5 7) "Bienvenue $ " (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !",Trame (Entete (Date 2019 5 9) "Bienvenue" (Personne "tato.ange@smail.ca" ("ange","tato")) [Personne "robert.julien@smail.ca" ("julien","robert")] [] []) "Allo Robert"],
-- Spams = [(Trame (Entete (Date 2020 12 21) "Bi!en! venue!" (Personne "equipesmartmail@smail.ca" ("equipe","smail")) [Personne "tato.ange@smail.ca" ("ange","tato")] [] []) "Bienvenue dans votre boite smartMail !","points d'exclamation")],
-- Contacts = [("bourassa.alex@smail.ca",Blanc),("robert.julien@smail.ca",Blanc)]
supprimerMessagesAvecFiltre :: CompteSmail -> String -> (Trame -> Bool) -> CompteSmail 
supprimerMessagesAvecFiltre (CompteSmail csp csr cse css cspr csc) ty fi
    |   ty == "Reception"   = (CompteSmail csp csr' cse css cspr csc)
    |   ty == "Envoi"       = (CompteSmail csp csr cse' css cspr csc)
    |   ty == "Spams"       = (CompteSmail csp csr cse css' cspr csc)
    |   otherwise           = (CompteSmail csp csr cse css cspr csc)
    where
        csr' = filter (not . fi) csr
        cse' = filter (not . fi) cse
        css' = filter (not . fi . fst) css


-- | Ajouter un courriel dans la boîte de réception
ajouterReception :: Trame -> CompteSmail -> CompteSmail 
ajouterReception t (CompteSmail csp csr cse css cspr csc) = CompteSmail csp csr' cse css cspr csc
    where
        csr' = [t] ++ csr


-- | Ajouter un courriel dans la boîte d'envoi
ajouterEnvoi :: Trame -> CompteSmail -> CompteSmail 
ajouterEnvoi t (CompteSmail csp csr cse css cspr csc) = CompteSmail csp csr cse' css cspr csc
    where
        cse' = [t] ++ cse


-- | Ajouter un courriel dans la boîte de spam
ajouterSpam :: (Trame, Explications) -> CompteSmail -> CompteSmail 
ajouterSpam (t, e) (CompteSmail csp csr cse css cspr csc) = CompteSmail csp csr cse css' cspr csc
    where
        css' = [(t, e)] ++ css


-- | Conserver les n premières trames de la boîte de réception
conserverTrReception :: Int -> CompteSmail -> CompteSmail
conserverTrReception 0 sm = sm
conserverTrReception x (CompteSmail csp csr cse css cspr csc) = (CompteSmail csp csr' cse css cspr csc)
    where
        csr' = take x csr


-- | Conserver les n premières trames de la boîte d'envoi
conserverTrEnvoi :: Int -> CompteSmail -> CompteSmail
conserverTrEnvoi 0 sm = sm
conserverTrEnvoi x (CompteSmail csp csr cse css cspr csc) = (CompteSmail csp csr cse' css cspr csc)
    where
        cse' = take x cse


-- | Conserver les n premières trames de la boîte de spam
conserverTrSpam :: Int -> CompteSmail -> CompteSmail
conserverTrSpam 0 sm = sm
conserverTrSpam x (CompteSmail csp csr cse css cspr csc) = (CompteSmail csp csr cse css' cspr csc)
    where
        css' = take x css


-- | Supprimer les trames de la boîte de réception qui ont une date strictement antérieure à celle spécifiée
supprimerAncienTrReception :: Date -> CompteSmail -> CompteSmail
supprimerAncienTrReception d (CompteSmail csp csr cse css cspr csc) = (CompteSmail csp csr' cse css cspr csc)
    where
        csr' = filter (\x -> date x < d) csr


-- | Supprimer les trames de la boîte d'envoi qui ont une date strictement antérieure à celle spécifiée
supprimerAncienTrEnvoi :: Date -> CompteSmail -> CompteSmail
supprimerAncienTrEnvoi d (CompteSmail csp csr cse css cspr csc) = (CompteSmail csp csr cse' css cspr csc)
    where
        cse' = filter (\x -> date x < d) cse


-- | Supprimer les trames de la boîte de spam qui ont une date strictement antérieure à celle spécifiée
supprimerAncienTrSpam :: Date -> CompteSmail -> CompteSmail
supprimerAncienTrSpam d (CompteSmail csp csr cse css cspr csc) = (CompteSmail csp csr cse css' cspr csc)
    where
        css' = filter (\(x, y) -> date x < d) css