MFP---Samodejno-racunanje-dvosmernih-preslikav
==============================================
Ekipa UM (Melanija in Urška)

Repositorij je nastal v sklopu predmeta Matematika s funkcijskim programiranjem 2014/2015.
___

**Dvosmerne preslikave (Bidirectionalization)**

Dvosmerne preslikave so sestavljene iz funkcije *get*, ki vzame izvor (source)
in vrne sliko (view) ter funkcije *put*, ki posodobljeno sliko in originalen
izvor preslika v posodobljen izvor, z določenimi pravili povezanimi z 
obema funkcijama.

Poglejmo si naslednji primer
```
halve :: [a] -> [a]
halve x = take (length `div` 2) x
```
in predpostavimo, da smo sliko na nek način posodobili in jo radi razširili v 
v originalni vhodni seznam.
To lahko naredimo z naslednjo funkcijo:
```
put_1 :: [a] -> [a] -> [a]
put_1 x x' | length x' == n
	  = x' ++ drop n x 
	  where n = length x `div` 2
```

Ker pisanje *put* funkcij ni ravno zabavno, se je porodila misel o avtomatizaciji.
Ideja je napisati funkcijo *bff*, ki vzame *get* funckijo kot vhod in vrne 
primerno *put* funckijo (npr. ```bff halve``` = ```put_1```).

```bff halve``` ne bo imela sinaktično enake definicije kot na roke napisana 
```put_1```. Imela bo zgolj enako fukcionalno vrednost, ki je semantično 
ekvivalentna, kar je z aplikacijskega vidika več kot dovolj.

Dobro definiran par *get/put* funkcij mora zadoščati naslednjima pogojema:
```put s (get s)``` = ```s```
```get (put s v)``` = ```v```


___
**Primerjava**
...

___
**Vir**

[Članek o avtomatskem računanju dvosmernih preslikav] (http://www.janis-voigtlaender.eu/papers/BidirectionalizationForFree.pdf)

___
**Zahteva**

Haskel

___
**Kontakt**

https://github.com/urska19



