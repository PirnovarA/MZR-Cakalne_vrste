source("lib/libraries.R")
source("main/functions.R")

# PRIHODI ####################################################################
simulacija.prihodov <- function(lambda, maxCas = FALSE, maxPrihodi = FALSE) {
  # lambda = intenziteta prihodov
  # maxCas = cas, kolikor so strezna mesta odprta
  # maxPrihodi = koliko prihodov imamo
  # Funkcija simulira prihode, katerih medprihodni cas je porazdeljen eksponentno
  # Ali cas ali prihod morata biti razlicna od FALSE, vsaj en mora biti FALSE
  # Ce maxCas != F: Toliko prihodov, dokler skupni cas prihodov manjsi od cas
  # Ce maxPrihodi != F: 'prihodi' prihodov
  # Vrne: data frame s stolpcema medsebojnih casov prihodov in skupnim casom
  if (maxCas == F & maxPrihodi == F) {
    errorCondition("Cas ali prihodi morata biti razlicna od FALSE.")
  } else if (maxCas != F & maxPrihodi != F) {
    errorCondition("Ali cas ali prihodi morata biti FALSE (ne oba).")
  }
  if (maxPrihodi != FALSE) {
    prihodi <- data.frame(medprihodni.casi = rexp(maxPrihodi, lambda)) %>% 
      mutate(cas.prihoda = cumsum(medprihodni.casi))
  } else {
    EValPrihodov <- lambda*maxCas   # zaradi HPP
    prihodi <- data.frame(medprihodni.casi = rexp(EValPrihodov, lambda)) %>% 
      mutate(cas.prihoda = cumsum(medprihodni.casi))
    while (prihodi[nrow(prihodi), "cas.prihoda"] < maxCas) {
      # sqrt(EValPrihodov) ker je to sd poissonove porazdelitve
      prihodiDodatni <-
        data.frame(medprihodni.casi = rexp(sqrt(EValPrihodov), lambda),
                   cas.prihoda = 0)
      prihodi <- prihodi %>% bind_rows(prihodiDodatni) %>% 
        mutate(cas.prihoda = cumsum(medprihodni.casi))
    }
    prihodi <- prihodi %>% filter(cas.prihoda <= maxCas)
  }
  return(prihodi)
}

dodaj.skupine <- function(prihodi, stSkupin = 0, VIP = 0, ...) {
  # Funkcija, ki prihodnim casom doda skupine, ki ponazarjajo h katerim
  # streznim mestom pasejo. Predpostavka: Ena skupina, eno strezno mesto.
  # prihodi = data.frame prihodov, katerim pripnemo vektor skupin
  # stSkupin = stevilo skupin oz streznih mest (trenutno isto, razen ce 0,
  #             potem ni skupin)
  # VIP = delez; vip osebe, ki pridejo takoj na vrsto, teh je delez v pop.
  # ... = dodatni parametri glede na prejsnje
  browser()
  stOseb <- nrow(prihodi)
  # Osnovno bodo skupine porazdeljene zaporedoma
  if (stSkupin == 0) {
    prihodi$skupine <- rep(0, stOseb)
  } else {
    prihodi$skupine <- rep_len(1:stSkupin, stOseb)
  }
  # VIP osebe, 0 ponazarja ne, 1 ja
  prihodi$VIP <- base::sample(c(0,1), stOseb, replace = T, prob = c(1-VIP, VIP))
  
  return(prihodi)
}
# STREZBE ####################################################################
simulacija.strezb <- function(prihodi, tip = "exp", ...) {
  # Funkcija ki simulira strezne case
  # prihodi = ce st. potem za toliko oseb rabimo zgenerirati. Ce prihodi
  #   data.frame, potem:
  #       - ce ni stolpca skupina: zgeneriramo po isti porazdelitvi za vse
  #       - ce stolpec skupina: skupine od 1:g, vsaka ima svoje parametre,
  #                             v vrstnem redu kot skupine (1:g)
  # tip = kako simuliramo case
  # ... = ostali parametri
  # Vrne vektor casov strezbe v istem redu kot input
  param <- list(...)
  if (!(tip %in% c("exp"))) {
    # TODO dodaj dodatne spremenljivke
    stop("Tip porazdelitve ni pravilen!")
  }
  if ((is.data.frame(prihodi) | length(prihodi) > 1) & 
      any(names(prihodi) == "skupina")) {
    # generiramo po skupinah, ker jih imamo
    if (tip == "exp") {
      # exponentna porazdelitev
      if (length(param$mu) != length(unique(prihodi$skupina))) {
        stop("Stevilo parametrov mora bit enako kot stevilo skupin!")
      }
      casiStrezb <- prihodi %>% group_by(skupina) %>% 
        mutate(casiStrezb = rexp(n(), param$mu[skupina[1]])) %>% 
        pull(casiStrezb)
    }
  } else {
    # Vsi imajo enako porazdelitev
    if (is.data.frame(prihodi)) {
      stOseb <- nrow(prihodi)
    } else if (length(prihodi) > 1) {
      stOseb <- length(prihodi)
    } else {
      stOseb <- prihodi
    }
    if (tip == "exp") {
      # Exponentna porazdelitev
      casiStrezb <- rexp(stOseb, param$mu)
    }
  }
  return(casiStrezb)
}

# FUNKCIJA POTEKA VRSTE ######################################################
simulacija.poteka.vrste <- function(mu, k, n, prihodi, strezniCasi) {
  # Simuliramo case strezbe pri cemer predpostavljamo, da so vsi
  # strezniki enaki
  # mu = intenziteta strezbe
  # k = st. streznikov
  # n = st. cakalnih mest
  # prihodi = data frame prihodov
  # Vrne seznam statistik vrste:
  #   id osebe
  #   cas prihoda
  #   zacetek strezbe
  #   ali je oseba cakala ali ne
  #   cas odhoda
  stOseb <- nrow(prihodi)
  prihodi <-  prihodi %>% mutate(id = 1:nrow(.))
  
  # Statistike ####
  stOdhodov <- 0  # St oseb, ki ni imelo prostora v cakalnici
  strezeneOsebe <- numeric(stOseb)  # Vse osebe, ki so bile strezene, 
                                          # imajo 1
  cakalniCasi <- numeric(stOseb) # Casi v cakalnici
  dogodkiOseb <- data.frame(id = 1:stOseb,  # id
                            prihod = rep(NA, stOseb), # cas
                            zacetekStrezbe = rep(NA, stOseb), # cas
                            cakanje = rep(0, stOseb), #0/1
                            odhod = rep(NA, stOseb)) # cas
  # Potrebno za delovanje ####
  stCak <- 0  # St. v cakalnici
  cakalnica <- rep(NA, n) # Vektor casov prihoda, kjer ime ponazarja osebo
  stZas <- 0  # St. trenutno strezenih
  strezniki <- numeric(k) # ID-ji strezenih
  
  osebe <- prihodi$id # ID-ji oseb, isti vrstni red kot dogodki. Ce -oseba,
                      # potem to pomeni, da je trenutno strezena
  dogodki <- prihodi$cas.prihoda    # Casi dogodkov
  stanje <- numeric(stOseb)  # ce 0, potem oseba pride, ce 1, potem je 
                                    # ze prisla in je cakala
  while (length(dogodki) > 0) {
    cas <- dogodki[1]
    dogodki <- dogodki[-1]
    oseba <- osebe[1]
    osebe <- osebe[-1]
    if (oseba > 0) {
      # Oseba je prisla
      dogodkiOseb$prihod[oseba] <- cas
      if (stZas < k) {
        # Prosto strezno mesto
        casStrezbe <- strezniCasi[oseba]  # Dobimo strezni cas za to osebo
        point <- getPosition(dogodki, cas + casStrezbe)  # lokacija dogodka
        dogodki <- insPosition(dogodki, cas + casStrezbe, point)  #dodamo dogodek
        osebe <- insPosition(osebe, -oseba, point)  # Po vrsti kdaj oseba zapusti
        stZas <- stZas + 1  # Mesto se zasede
        strezniki[which(strezniki == 0)[1]] <- oseba  # Oseba je pri strezniku
        strezeneOsebe[oseba] <- 1 # Oseba je bila postrezena
        dogodkiOseb$zacetekStrezbe[oseba] <- cas
      } else if (stCak < n) {
        # Strezno mesto zasedeno, cakalnica prosta
        prostoMesto <- which(is.na(cakalnica))[1]  # Kje je prosto mesto
        cakalnica[prostoMesto] <- cas  # Oseba v cakalnici
        names(cakalnica)[prostoMesto] <- oseba
        stCak <- stCak + 1  # Eno cakalno mesto vec zasedeno
        dogodkiOseb$cakanje[oseba] <- 1
      } else {
        # Strezno mesto zasedeno, cakalnica zasedena
        stOdhodov <- stOdhodov + 1
        dogodkiOseb$odhod[oseba] <- cas
      }
        
    } else {
      # Oseba je opravila in bo odsla.
      strezniki[which(strezniki == -oseba)] <- 0
      stZas <- stZas - 1
      dogodkiOseb$odhod[-oseba] <- cas
      if (stCak > 0) {
        # Ce kdo v cakalnici, bo na vrsti
        naVrstiInd <- which(cakalnica == min(cakalnica, na.rm = T))[1]
        casPrihoda <- cakalnica[naVrstiInd]
        osebaNaVrsti <- as.integer(names(casPrihoda))  # Oseba iz cakalnice
        cakalnica[naVrstiInd] <- NA
        stCak <- stCak - 1 # Zapusti cakalnico
        
        cakalniCasi[osebaNaVrsti] <- cas - casPrihoda  # Cas cakanja
        
        casStrezbe <- strezniCasi[osebaNaVrsti] # Dobimo strezni cas za to osebo
        point <- getPosition(dogodki, cas + casStrezbe)  # lokacija dogodka
        dogodki <- insPosition(dogodki, cas + casStrezbe, point) # dodamo dogodek
        osebe <- insPosition(osebe, -osebaNaVrsti, point)  # Po vrsti kdaj 
                                                           # oseba zapusti
        stZas <- stZas + 1  # Mesto se zasede
        strezniki[which(strezniki == 0)[1]] <- osebaNaVrsti # Oseba je pri strezniku
        strezeneOsebe[osebaNaVrsti] <- 1 # Oseba je bila postrezena
        dogodkiOseb$zacetekStrezbe[osebaNaVrsti] <- cas  # Oseba iz cakanja je na vrsti
      }
    }
  }
  
  # queuecomputer::queue_step(prihodi$cas.prihoda, strezbeVse, k)
  rezultati <- list()
  rezultati$postrezeni <- which(strezeneOsebe == 1) # Postrezene stranke
  rezultati$stOdhodov <- stOdhodov  # St ljudi, ki je je odslo ker ni bilo prostora
  rezultati$cakalniCasi <- cakalniCasi  # cakalni casi ljudi (ki so cakali)
  rezultati$dogodkiOseb <- dogodkiOseb # za simulacijo
  
  return(rezultati)
}

# SIMULACIJA VRSTE ###########################################################
simulacija.vrste <- function(lambda, mu, k, n, cas) {
  # lambda = intenziteta prihodov
  # mu = intenziteta strezbe
  # k = stevilo streznih mest
  # n = stevilo cakalnih mest
  # cas = cas, kolikor so strezna mesta odprta
  
  
}

# TESTIRANJE ################################################################

prihodi <- simulacija.prihodov(100, maxCas = 10)
dodaj.skupine(prihodi)

strezniCasi <- simulacija.strezb(prihodi = prihodi, tip = "exp", mu = 5)

rez <- simulacija.poteka.vrste(20, 5, 10, prihodi, strezniCasi)

# TODO simulacija, ce osebe nepotrpezljive, ce 2 vrsti streznikov
# ce preddoloceno, kam se morajo uvrstit osebe, druga porazdelitev strezbe,
# priority

# TODO MC simulacija, za nekatere vrednosti, grafi statistik








