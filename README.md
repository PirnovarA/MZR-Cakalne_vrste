# MZR-Cakalne vrste

## Zagon

V mapi _Shiny_ so datoteke potrebne za pogon aplikacije. Vse knjiznjice so 
nastete na zacetku datoteke _ui.R_ v tej mapi. Aplikacijo se lahko zazene
preko R Studia, ali pa direktno v R-u z ukazom 
```R
shiny::runApp("Shiny")
```

## 1. Osnovna teorija cakalnih vrst

Teorija cakalnih vrst je podzvrst operacijskih raziskav. V praksi je uporabna v 
trgovinah, spletnih trgovinah, fulfilment centrih, zdravstvu, racunalnistvu itd.   
Splosna oznaka za cakalno vrsto s **k** strezniki in **n** cakalnimi mesti, kjer
so casi strezbe pozabljivi, prav tako pa so pozabljivi tudi casi med zaporednimi
prihodi, je **M/M/k/n**. Predvidevamo eno cakalno vrsto, torej ne vec zaporednih,
in princip FIFO (first-in first-out). 
Da lahko govorimo o Markovski verigi in da zadostimo zgornjim lastnostim, 
zahtevamo, da so medprihodni casi porazdeljeni eksponentno z intenziteto 
_lambda_, casi strezbe pa z intenziteto _mu_.  Za tako cakalno vrsto imamo 
razvito verjetnostno teorijo, konvergence, ipd. 
Zaradi zgornjih lastnosti prihodi sledijo homogenemu Poissonovem procesu z 
intenziteto lambda (mu). 
Predpostavka Poissonovega procesa za prihode je smiselna in potrjena v praksi,
kadar gre za nakljucne prihode (in ne npr. cetrtke, ko je vec upokojencev, ki 
cakajo pred vrati na odprtje zaradi popustov). Kjer pa predpostavke niso realne
pa je pri modeliranju casov strezenja. V realnosti strezniki skoraj nikoli 
nimajo iste porazdelitve strezenja (razen mogoce serverji), ta porazdelitev
pa tudi ni eksponentna. V tem modelu tudi ne moremo prikazati dodatnih zapletov
in lastnosti vrst ter cakalnic. Zato preidemo na podrocje simulacij, kjer
zgradimo model poteka vrste, ki krsi zgornje pogoje, hkrati pa doda nove 
lastnosti poteku vrste.  

## 2. Razsirjen model cakalnih vrst

Model, s katerim lahko bolj podrobno simuliramo cakalne vrste. Lastnosti oz.
opcije modela so naslednje:  

* poljubno stevilo streznih mest
* poljubna velikost cakalnice
* porazdelitev prihodov po poissonovem procesu
* prihodi do dolocenega casa ali stevila prihodov
* nestrpne osebe
* prednostne (vip) osebe
* moznost preddolocanja streznikov osebam
* razlicne porazdelitve za vsak streznik, ne nujno eksponentne

Vrsta poteka tako, da ljudje prihaja, in ce so prosta mesta, so postrezeni 
(razen, ce imajo preddolocene streznike in je tisti zaseden). Ce ne morejo biti
postrezene, gredo v cakalnico, kjer cakajo da osebe, ki so prisle pred njimi 
odpravijo. Posebnost pri tem so vip osebe, ki imajo v cakalnici vedno prednost.
Ce jih je vec, se tudi pri njih uposteva hitrost prihoda. Nestrpne osebe po
nekaj casa (dolocenega) cakanja oddidejo iz cakalnice, brez da bi bile 
postrezene. Ko oseba neha biti strezena, oddide. Ce je cakalnica zapolnjena
ob prihodu nove osebe, se ta obrne in oddide, brez strezbe.

### 2.1 Neucakanost oseb

Neucakanost se podeli nakljucno, parameter je delez ljudi, ki so neucakani.
Dodaten parameter je, koliko casa oseba ostane v cakalnici, preden jo zapusti;
za vse je ta parameter enak.

### 2.2 VIP status

Podeljuje se nakljucno, parameter je delez ljudi, ki ga imajo. Lahko se ga 
doloci tocno osebam, ki so neucakane. Med seboj so vip osebe v cakalnice se
vedno razporejene po principu FIFO, torej prvi pride, pri na vrsti.

### 2.3 Porazdelitev streznikov

Strezniki imajo lahko med seboj razlicne porazdelitve. Predpostavka je se vedno,
da so strezni casi pri enem strezniku med seboj neodvisni, kar ni nujno res v
realnosti. Razlicne porazdelitve pa omogocajo simuliranje razlicno hitrih
streznikov, kar je realno predpostavljati.

### 2.4 Preddolocanje streznikov

Osebam se lahko preddolocijo strezniki, ki jih morajo obiskati. To si lahko 
predstavljamo kot upravno enoto, kjer nekdo caka na potni list, nekdo na 
dovoljenje ipd, ali pa kot zdravstveni dom z razlicnimi zdravniki a isto 
cakalnico (i.e. urgenca). Omejitev je, da vsaki osebi predpisemo le en mozen
streznik, hkrati pa preddolocene streznike dolocujemo zaporedoma. To bi lahko
ustrezalo principu, da vsakemu ko pride dolocimo drug streznik, ker hocemo 
lastnorocno prerazporediti ljudi.

# 3. Metoda

Problem pri simuliranju čakalnih vrst je, da se zaporedje dogodkov spreminja
glede na dogodke, ki so se zgodili pred njimi, saj lahko, da oseba mora cakati,
da pride oseba s prednostjo itd. Ker so dogodki diskretni, uporabimo 
**Discrete event simulation (DES)** metodo za simuliranje. Za R obstaja nekaj
paketov ki se ukvarjajo s takimi simulacijami, a za moj namen sem sam spisal
kodo za izvajanje teh simulacij, ki vključuje vse zgoraj nastete lastnosti.

# 4. Cilj

Cilj projekta je simulacija in vizualizacija cakalnih vrst z razlicnimi 
parametri. Na podlagi tega se lahko analizira obnasanje le teh in preizkusa 
mozne izboljsave oz. alokacije (npr vec streznikov, ki so pocasnejsi ipd.).

