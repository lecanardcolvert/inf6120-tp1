# Travail pratique 1 - Messagerie SmartMail 2.0


## Description

SmartMail 2.0 permet à un utilisateur inscrit de disposer d'un service de messagerie électronique intelligent offrant trois boîtes (envoi, réception, spam). Les messages suspects sont automatiquement envoyés dans la boîte spam. Ils sont filtrés selon un ensemble de principes ou d'heuristiques spécifiques. L'utilisateur peut aussi entretenir un ensemble de contacts et éventuellement de préférences. L'état d'un contact suspect passera de 'blanc' à 'noir' indiquant ainsi son inscription dans la liste noire et provoquant une redirection automatique des messages reçus de ce contact dans la boîte spam. Un service d'administration de SmartMail 2.0 est offert pour la gestion des messages et la production de quelques statistiques d'intérêt. 

## Auteur

Alexandre H. Bourdeau (HAMA12128907)


## Fonctionnement

Pour faire fonctionner le projet, vous devez premièrement télécharger les fichiers du projet.

Puis, installez le compilateur [GHC](https://www.haskell.org/downloads/) s'il n'est pas installé sur votre ordinateur.

Ensuite, vous devez vous assurer que vous avez installé les dépendances listées ci-dessous.

Finalement, lancez le programme à l'aide de la commande `ghci SmartMail.hs`.


## Contenu du projet

Les fichiers du projet sont :

* CompteSmail.hs pour gérer les comptes SmartMail
* Personne.hs pour gérer les personnes
* SmartMail.hs pour gérer la base de données SmartMail
* Trame.hs pour gérer le contenu des courriels


## Dépendances

* Data.Char [(site officiel)](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html)
* Data.List [(site officiel)](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html)
* Data.Map [(site officiel)](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html)
* Data.Maybe [(site officiel)](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Maybe.html)
* Data.Time.Calendar [(site officiel)](https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Calendar.html)
* Data.Time.Clock [(site officiel)](https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Clock.html)
* System.IO.Unsafe [(site officiel)](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO-Unsafe.html)


## Références

* [Learn you Haskell](http://learnyouhaskell.com/)


## Statut

Le projet est présentement en cours de développement.