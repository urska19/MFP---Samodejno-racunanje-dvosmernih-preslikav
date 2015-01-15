{-|
Description : spellchecker
Copyright   : (c) Urska, 2015;
		          Melanija, 2015
License     : GPL-3

Stability   : experimental

-- A simple example of slovenian spellchecker. 
-}

import Bff
import Data.List.Split 
import Data.Char


slovar = ["bo", "dan", "danes", "je", "jutri", "lep", "tudi"]

stavek1 = "dane j le da" -- vsem besedam manjka zadnja crka, ni velike zacetnice in locila
stavek2 = "dane j le da!" -- vsem besedam manjka zadnja crka, ni velike zacetnice, na koncu stavka je locilo
stavek3 = "danes je le da" -- zadnjim dvem besedam manjka zadnja crka, ni velike zacetnice in locila
stavek4 = "Jutri b tud le da" -- je velika zacetnica, na koncu stavka ni locila


-- | Prejme besedo in ji odstrani zadnjo crko.
get :: [a] -> [a] 
get beseda = take ((length beseda) - 1) beseda

-- | Prejme besedo iz stavka in vrne prvo podobno besedo iz slovarja.
najdiPravoBesedo :: Eq a => [a] -> [[a]] -> [a]
najdiPravoBesedo beseda [] = beseda
najdiPravoBesedo beseda (x:xs) =  if (take ((length x) - 1) x) == beseda
									then x
									else najdiPravoBesedo beseda xs
									
-- | Crkovalnik
popraviStavek :: [Char] -> [Char]
popraviStavek stavek = 	popraviStavek2 (splitOn " " stavek) "" 1

popraviStavek2 :: [[Char]] -> [Char] -> Int -> [Char]
popraviStavek2 [x] acc _ = if isLetter (head (drop ((length x) - 1) x))
						   then let najdenaBeseda = najdiPravoBesedo x slovar
								in if x == najdenaBeseda
								then acc ++ x ++ "."
								else acc ++ (bff_Eq get najdenaBeseda x) ++ "."
						   else let najdenaBeseda = najdiPravoBesedo (take ((length x) -1) x) slovar
								in if (take ((length x) -1) x) == najdenaBeseda
								then acc ++ x 
								else acc ++ (bff_Eq get najdenaBeseda (take ((length x) -1) x)) ++ (drop ((length x) - 1) x)
popraviStavek2 (x:xs) acc 1 = if isUpper (head (take 1 x))
							  then 	let 
										beseda = [(toLower (head (take 1 x)))] ++ (drop 1 x)
										najdenaBeseda = najdiPravoBesedo beseda slovar
									in 
										if beseda == najdenaBeseda
										then popraviStavek2 xs (acc ++ x ++ " ") 2
										else popraviStavek2 xs (acc ++ (bff_Eq get najdenaBeseda x) ++ " ") 2
							  else 	let 
										najdenaBeseda = najdiPravoBesedo x slovar
										besedaVelikaZacetnica = [(toUpper (head (take 1 x)))] ++ (drop 1 x)
									in  
										if x == najdenaBeseda
										then popraviStavek2 xs (acc ++ besedaVelikaZacetnica ++ " ") 2
										else popraviStavek2 xs (acc ++ (bff_Eq get najdenaBeseda besedaVelikaZacetnica) ++ " ") 2							 
popraviStavek2 (x:xs) acc n = 	let 
									najdenaBeseda = najdiPravoBesedo x slovar
								in 
									if x == najdenaBeseda
									then popraviStavek2 xs (acc ++ x ++ " ") (n + 1)
									else popraviStavek2 xs (acc ++ (bff_Eq get najdenaBeseda x) ++ " ") (n+1)

--Klic iz konzole
--popraviStavek besede1


