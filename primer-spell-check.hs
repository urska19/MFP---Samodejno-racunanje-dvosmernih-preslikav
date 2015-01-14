{-|
Description : spellchecker
Copyright   : (c) Urska, 2015;
		          Melanija, 2015
License     : GPL-3

Stability   : experimental

-- A simple example of slovenian spellchecker. It only corrects the mistake in which the last letter of the word is missing.
-}

import Bff
import Data.List.Split 


slovar = ["bo", "dan", "danes", "je", "jutri", "lep", "tudi"]
stavek = "dane j le da" -- vsem besedam manjka zadnja crka
--stavek = "danes je le da" -- zadnjim dvem besedam manjka zadnja crka
--stavek = "jutri bo tudi lep dan"

besede = splitOn " " stavek


--Prejme besedo in ji odstrani zadnjo crko.
get :: [a] -> [a] 
get beseda = take ((length beseda) - 1) beseda

--Prejme besedo iz stavka in vrne prvo podobno besedo iz slovarja.
najdi_pravo_besedo :: Eq a => [a] -> [[a]] -> [a]
najdi_pravo_besedo beseda [] = beseda
najdi_pravo_besedo beseda (x:xs) =  if (take ((length x) - 1) x) == beseda
									then x
									else najdi_pravo_besedo beseda xs
									
--Crkovalnik
popravi_stavek :: [[Char]] -> [Char] -> [Char]
popravi_stavek [] acc = take ((length acc) -1) acc
popravi_stavek (x:xs) acc = let najdena_beseda = najdi_pravo_besedo x slovar
							in if x == najdena_beseda
							   then popravi_stavek xs (acc ++ x ++ " ")
							   else popravi_stavek xs (acc ++ (bff_Eq get (najdi_pravo_besedo x slovar) x) ++ " ")

--Klic iz konzole
--popravi_stavek besede ""


