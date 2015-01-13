import Bff
import Data.List.Split 


slovar = ["dan", "danes", "je", "lep"]
stavek = "dane je le da"

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


