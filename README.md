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
polovicka :: [a] -> [a]
polovicka x = take (length `div` 2) x
```
in predpostavimo, da smo sliko na nek način posodobili in bi jo radi razširili v 
v originalni vhodni seznam.
To lahko naredimo z naslednjo funkcijo:
```
put_1 :: [a] -> [a] -> [a]
put_1 x x' | length x' == n
	  = x' ++ drop n x 
	  where n = length x `div` 2
```

Ker pisanje *put* funkcij ni ravno zabavno, se je porodila misel o njihovi avtomatizaciji.
Ideja je napisati funkcijo *bff*, ki vzame kot *get* funckijo kot vhod in vrne 
primerno *put* funckijo (npr. ```bff polovicka``` ~ ```put_1```).

```bff polovicka``` ne bo imela sintaktično enake definicije kot na roke napisana 
```put_1```. Imela bo zgolj enako fukcionalno vrednost, ki je semantično 
ekvivalentna, kar je z aplikacijskega vidika več kot dovolj.

Dobro definiran par *get/put* funkcij mora zadoščati naslednjima pogojema:

- ```put s (get s)``` = ```s```

- ```get (put s v)``` = ```v```


**Delovanje *bff***

Kako se lahko *bff* funkcija nauči kaj uporabnega iz vhodne *get* funkcije in 
izkoriti pridobljene informacije za sproduciranje dobre *put* funkcije?

Najprej omenimo, da mora biti *get* funkcija naj bo polimorfna, tako je njeno obnašanje odvisno le od 
pozicije elementov in ne od njihove konkretne vrednosti.


V grobem *bff* deluje na naslednji način
(*bff* dobi za vhod get funkcijo, originalen izvor *s* in posodobljeno sliko *v*):

- Vzame *s*, naredi predlogo *s' = [0..n]*, če *n = length(s)*, in asociacijo *g*, med 
ustreznimi vrednostmi iz *s'* in *s*.

- Zažene *get* na *s'*, dobi predlogo slike *v'* in sproducira asociacijo *h* med *v'* in *v*.

- Združi asociaciji *g* in *h* v *h'*, kjer ima *h* prednost, ko je index predlog najden 
v *h* in *g*. S tem zagotovimo, da se bomo zatekli k vrednosti iz *s*, ko neka 
pozicija ne bo prišla v sliko in zato ni možnosti, da bi bila vrednost na tej poziciji spremenjena 
s posodobitvijo.

- Na koncu naredi posodobljen izvor z zapolnitvijo vseh pozicij na *[0..n]*
z ustreznimi vrednostmi glede na *h'*.




___
**Primerjava**

Za par enostavnih *get* funkcij smo primerjali računsko zahtevnost avtomatsko zgeneriranih in na roke zapisanih *put* funkcij. Rezultate smo predstavili v obliki naslednjih grafov.

Prvi graf prikazuje računsko zahtevnost avtomatsko zapisane *put* funkcije (*bff polovička*) in na roke zapisano *put* (*put_1*) za *get* funkcijo *polovička*. 

![alt tag](https://raw.githubusercontent.com/urska19/MFP---Samodejno-racunanje-dvosmernih-preslikav/master/halve1.jpg)

Drugi graf prikazuje računsko zahtevnost avtomatsko zapisane *put* funkcije (*bff zbrisiDvojnike*) in na roke zapisano *put* za get funkcijo *zbrisiDvojnike*, ki iz seznama pobriše ponavljajoče se elemente.
V tem primeru originalen izvor ni imel ponavljajočih se elementov.

![alt tag](https://raw.githubusercontent.com/urska19/MFP---Samodejno-racunanje-dvosmernih-preslikav/master/rmdups_all_different_elts1.jpg)

Tretji graf prikazuje računsko zahtevnost avtomatsko zapisane *put* funkcije (*bff zbrisiDvojnike*) in na roke zapisano *put* za get funkcijo *zbrisiDvojnike*, ki iz seznama pobriše ponavljajoče se elemente.
V tem primeru je imel originalen izvor vsak element ponovljen dvakrat.

![alt tag](https://raw.githubusercontent.com/urska19/MFP---Samodejno-racunanje-dvosmernih-preslikav/master/rmdups_every_elt_is_duplicated1.jpg)

V vseh primerih vidimo, da je časovna zahtevnost avtomatske *put* funkcije višja od *put* funkcije, ki smo jo napisali na roke. Lahko rečemo, da je cena avtomatizacije dvosmernih preslikav višja računska zahtevnost.

___
**Vir**

[Članek o avtomatskem računanju dvosmernih preslikav] (http://www.janis-voigtlaender.eu/papers/BidirectionalizationForFree.pdf)

___
**Zahteva**

Haskell

___
**Kontakt**

https://github.com/urska19



