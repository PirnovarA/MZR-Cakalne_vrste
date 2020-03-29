source("lib/libraries.R")
source("main/functions.R")

# PRIHODI ####################################################################
simulacija.prihodov <- function(lambda, maxCas = NULL, maxPrihodi = NULL) {
  # lambda = intenziteta prihodov
  # maxCas = cas, kolikor so strezna mesta odprta
  # maxPrihodi = koliko prihodov imamo
  # Funkcija simulira prihode, katerih medprihodni cas je porazdeljen eksponentno
  # Ali cas ali prihod morata biti podana, a le en
  # Ce podan maxCas: Toliko prihodov, dokler skupni cas prihodov manjsi od cas
  # Ce podan maxPrihodi: 'prihodi' prihodov
  # Vrne: data frame s stolpcema medsebojnih casov prihodov in skupnim casom
  if (missing(maxCas) & missing(maxPrihodi)) {
    errorCondition("Cas ali prihodi morata biti razlicna od FALSE.")
  } else if (xor(!missing(maxCas), !missing(maxPrihodi))) {
    errorCondition("Podan je lahko le cas ali le prihodi (ne oba).")
  }
  if (!is.null(maxPrihodi)) {
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

dodaj.nepotrpezljive <- function(prihodi, delez, cas) {
  # Doda indikator, ce je oseba neportpezljiva ali ne
  # prihodi = data.frame prihodov
  # delez = delez nopotrpezljivih oseb
  # cas = cas, po katerem oseba zapusti cakalnico
  # nepotrpezljive osebe, 0 ponazarja ne, 1 ja (imp = impatient)
  # impTime je cas, po katerem oseba zapusti cakalnico
  stOseb <- nrow(prihodi)
  prihodi$imp <- base::sample(c(0,1), stOseb, replace = T, 
                              prob = c(1-delez, delez))
  prihodi$impTime <- cas
  return(prihodi)
}

dodaj.skupine <- function(prihodi, stSkupin = 0, VIP = 0, VIP.imp = FALSE) {
  # Funkcija, ki prihodnim casom doda skupine, ki ponazarjajo h katerim
  # streznim mestom pasejo. Predpostavka: Ena skupina, eno strezno mesto.
  # prihodi = data.frame prihodov, katerim pripnemo vektor skupin
  # stSkupin = stevilo skupin oz streznih mest (trenutno isto, razen ce 0,
  #             potem ni skupin)
  # VIP = delez; vip osebe, ki pridejo takoj na vrsto, teh je delez v pop.
  # ... = dodatni parametri glede na prejsnje
  stOseb <- nrow(prihodi)
  # Osnovno bodo skupine porazdeljene zaporedoma
  if (stSkupin == 0) {
    prihodi$skupina <- rep(0, stOseb)
  } else {
    prihodi$skupina <- rep_len(1:stSkupin, stOseb)
  }
  # VIP osebe, 0 ponazarja ne, 1 ja
  if (VIP.imp) {
    # "poznamo" nepotrpezljive osebe, damo jim prednost
    prihodi$VIP <- prihodi$imp
  } else {
    prihodi$VIP <- base::sample(c(0,1), stOseb, replace = T, prob = c(1-VIP, VIP))
  }
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
  #                             Razen ce skupina 0, potem ista porazd. za vse
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
    skupine <- unique(prihodi$skupina)
    # generiramo po skupinah, ker jih imamo
    if (tip == "exp") {
      # exponentna porazdelitev
      if (length(param$mu) != length(skupine)) {
        stop("Stevilo parametrov mora bit enako kot stevilo skupin!")
      } else if (all(skupine == 0)) {
        # Vsi strezniki enako porazdeljeni, jih je neomejeno
        casiStrezb <- rexp(nrow(prihodi), param$mu)
      } else {
        casiStrezb <- prihodi %>% group_by(skupina) %>% 
          mutate(casiStrezb = rexp(n(), param$mu[skupina[1]])) %>% 
          pull(casiStrezb)
      }
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

simulacija.strezb.razlicni <- function(prihodi, stSkupin, tip, ...) {
  # Funkcija ki simulira strezne case
  # prihodi = ce st. potem za toliko oseb rabimo zgenerirati. Ce prihodi
  #   data.frame, potem toliko casov kot je st. vrstic
  # stSkupin = stevilo skupin, za katere rabimo generirat case
  # tip = vektor dolzine kolikor je stevilo skupin za tipe porazdelitev
  # posameznih skupin
  # Vrne data.frame casov strezb po vrsti, kot so podani njihovi tipi in parametri
  param <- list(...)
  simuliraj.cas <- function(i, tip, stOseb, ...) {
    # Simulira case strezbe glede na tip in stOseb. ... vsebuje parametre.
    # i zaradi izbire parametrov
    if (tip[i] == "exp") {
      # Exponentna porazdelitev, parameter mu
      sim <- rexp(stOseb, param$mu[i])
    }
    return(sim)
  }
  if (is.data.frame(prihodi)) {
    stOseb <- nrow(prihodi)
  } else if (length(prihodi) > 1) {
    stOseb <- length(prihodi)
  } else {
    stOseb <- prihodi
  }
  casiStrezb <- lapply(1:stSkupin, simuliraj.cas, tip = tip, 
                       stOseb = stOseb, ...) %>% bind_cols() %>% 
    `names<-`(paste0("skupina", rep(1:stSkupin)))
  
  return(casiStrezb)
}
# FUNKCIJA POTEKA VRSTE ######################################################
simulacija.poteka.vrste <- function(k, n, prihodi, strezniCasi, porazd = NULL) {
  # Simuliramo case strezbe pri cemer predpostavljamo, da so strezniki
  # razdeljeni v skupine
  # k = st. streznikov
  # n = st. cakalnih mest
  # prihodi = data frame prihodov v formatu, opisanem zgoraj TODO
  # strezniCasi = data frame streznih casov po skupinah, ali pa vektor,
  #               ce vsi strezni casi porazdeljeni enako
  # porazd = vektor dolzine k, ki pove, iz katere porazdelitve so strezniki
  #           na pripadajocem mestu v vektorju. Ce NULL, potem je samo en  
  #           podatek o streznikih (lahko se uporabijo skupine)
  # Vrne data frame z dogodki oseb:
  #   id: id osebe
  #   prihod: cas, kdaj je oseba prispela
  #   zacetekStrezbe: kdaj se je strezba za osebo zacela, NA ce se ni
  #   odhod: kdaj je oseba odsla (koncala s strezbo/obupala/polna cakalnica)
  #   cakanje: 0/1, glede na to, ali je oseba cakala ali ne
  #   zasedeno: 0/1, glede na to, ali je bila cakalnica zasedena ali ne
  #   odhodImp: 0/1, glede na to, ali je neucakana oseba odsla zaradi neucakanosti
  #   streznik: stevilka streznika pri katerem je bila oseba
  
  stOseb <- nrow(prihodi)
  prihodi <-  prihodi %>% mutate(id = 1:nrow(.))
  if (!is.null(porazd)) {
    prihodi$skupina <- 0
  } else {
    strezniCasi <- data.frame(strezniCasi)
    porazd <- rep(1, k)
  }
  skupine <- unique(prihodi$skupina)
  if (is.null(skupine)) {skupine <- 0}
  if (!all(0 == skupine) & length(skupine) != k) {
    # Ali je toliko skupin kolikor je streznih mest? Ce ne, napaka
    stop("Stevilo skupin mora biti enako stevilu streznih mest!")
  }
  # Statistike ####
  dogodkiOseb <- data.frame(id = 1:stOseb,  # id
                            prihod = as.numeric(rep(NA, stOseb)), # cas
                            zacetekStrezbe = as.numeric(rep(NA, stOseb)), # cas
                            odhod = as.numeric(rep(NA, stOseb)), # cas
                            cakanje = rep(0, stOseb), #0/1
                            zasedeno = rep(0, stOseb), #zasedeno
                            odhodImp = rep(0, stOseb),  # nepotrpezljiv odhod
                            streznik = rep(NA, stOseb), # kateri streznik
                            skupina = prihodi$skupina, # kateri skupini pripada
                            imp = prihodi$imp, # ali je oseba nepotrpezljiva
                            VIP = prihodi$VIP)  # ali je vip
  # Potrebno za delovanje ####
  stCak <- 0  # St. v cakalnici
  cakalnica <- rep(NA, n) # Vektor casov prihoda, kjer ime ponazarja osebo
  strezniki <- numeric(k) # ID-ji strezenih
  
  osebe <- prihodi$id # ID-ji oseb, isti vrstni red kot dogodki. Ce -oseba,
  # potem to pomeni, da je trenutno strezena
  strezeneOsebe <- numeric(stOseb)  # Vse osebe, ki so bile strezene, 
  # imajo 1
  dogodki <- prihodi$cas.prihoda    # Casi dogodkov
  stanje <- numeric(stOseb)  # ce 0, potem oseba pride, ce 1, potem je 
  odhodImp <- numeric(stOseb) # Ce je oseba nestrpna, potem se da sem cas,
  #                             ko bo zapustila cakalnico
  # ze prisla in je cakala
  skupineCakajocih <- numeric(n)  # skupine cakajocih ljudi, lazje iskanje
  vipCakajoci <- numeric(n) # Ce 0 potem ni VIP, drugace je
  
  if (any(skupine == 0)) {
    # Ce ne razporedimo osebe na skupine (torej skupine vse 0)
    skupineBool <- FALSE # Ali imamo skupine ali ne
    skupinaOseba <- 1:k
  } else {
    skupineBool <- TRUE
  }
  while (length(dogodki) > 0) {
    cas <- dogodki[1]
    dogodki <- dogodki[-1]
    oseba <- osebe[1]
    osebe <- osebe[-1]
    if (skupineBool) {
      skupinaOseba <- prihodi$skupina[abs(oseba)]
    }
    if (oseba > 0) {
      # Oseba je prisla
      dogodkiOseb$prihod[oseba] <- cas
      if (any(skupinaOseba %in% which(strezniki == 0))) {
        # Prosto strezno mesto
        prostiStreznik <- intersect(which(strezniki == 0), skupinaOseba)[1]
        # Dobimo strezni cas za to osebo, glede na porazdelitev streznika, h 
        # katerem bo oseba sla
        casStrezbe <- strezniCasi[oseba, porazd[prostiStreznik]] %>% as.numeric()  
        point <- getPosition(dogodki, cas + casStrezbe)  # lokacija dogodka
        dogodki <- insPosition(dogodki, cas + casStrezbe, point)  #dodamo dogodek
        osebe <- insPosition(osebe, -oseba, point)  # Po vrsti kdaj oseba zapusti
        # Oseba je pri strezniku
        strezniki[prostiStreznik] <- oseba
        strezeneOsebe[oseba] <- 1 # Oseba je bila postrezena
        dogodkiOseb$zacetekStrezbe[oseba] <- cas
        dogodkiOseb$streznik[oseba] <- prostiStreznik # kateri streznik
      } else if (stCak < n) {
        # Strezno mesto zasedeno, cakalnica prosta
        prostoMesto <- which(is.na(cakalnica))[1]  # Kje je prosto mesto
        vipCakajoci[prostoMesto] <- prihodi$VIP[oseba]  # ali oseba VIP
        cakalnica[prostoMesto] <- cas  # Oseba v cakalnici
        names(cakalnica)[prostoMesto] <- oseba  # Ime cakajoce osebe
        stCak <- stCak + 1  # Eno cakalno mesto vec zasedeno
        dogodkiOseb$cakanje[oseba] <- 1
        # Dodamo skupino cakajoce osebe
        if (!skupineBool) {
          skupineCakajocih[prostoMesto] <- 0
        } else {
          skupineCakajocih[prostoMesto] <- skupinaOseba
        }
        
        if (prihodi$imp[oseba] == 1) {
          # Preverimo se, ce je oseba neucakana Ce je, dodamo njen odhod v vrsto
          casOdhodaImp <- cas + prihodi$impTime[oseba]
          point <- getPosition(dogodki, casOdhodaImp) 
          dogodki <- insPosition(dogodki, casOdhodaImp, point)  #dodamo dogodek
          osebe <- insPosition(osebe, -oseba, point)  # Po vrsti kdaj oseba zapusti
          odhodImp[oseba] <- casOdhodaImp
        }
      } else {
        # Strezno mesto zasedeno, cakalnica zasedena
        dogodkiOseb$odhod[oseba] <- cas
        dogodkiOseb$zasedeno[oseba] <- 1
      }
    } else if (oseba < 0 & odhodImp[-oseba] == cas & 
               strezeneOsebe[-oseba] == 0) {
      # Neucakana oseba je postala prevec neucakana, zapusti cakalnico
      cakalnicaImpOs <- which(names(cakalnica) == -oseba)
      cakalnica[cakalnicaImpOs] <- NA
      dogodkiOseb$odhod[-oseba] <- cas
      dogodkiOseb$odhodImp[-oseba] <- 1
      vipCakajoci[cakalnicaImpOs] <- 0
      stCak <- stCak - 1
    } else if (oseba < 0 & is.na(dogodkiOseb$odhod[-oseba])){
      # Oseba je opravila in bo odsla, ni ze opravila (npr nestrpni)
      prostiStreznik <- which(strezniki == -oseba)
      strezniki[prostiStreznik] <- 0
      dogodkiOseb$odhod[-oseba] <- cas
      if (stCak > 0) {
        # Ce kdo v cakalnici, bo na vrsti, ce ustreza pogoju skupine
        if (!skupineBool) {
          # Nimamo skupin, iz "prave skupine torej vsi, ki cakajo
          izPraveSkupine <- 1:n
          prisotenVip <- 1 %in% vipCakajoci[izPraveSkupine]
        } else {
          # Katere cakajoce osebe pripadajo pravi skupini
          izPraveSkupine <- which(skupineCakajocih == skupinaOseba)
          prisotenVip <- 1 %in% vipCakajoci[izPraveSkupine]
          if (prisotenVip) {
            # Ce imamo vsaj eno VIP osebo
            izPraveSkupine <- intersect(izPraveSkupine, which(vipCakajoci == 1))
          }
        }
        if (length(izPraveSkupine) != 0) {
          # Imamo ljudi, ko so lahko na vrsti
          naVrstiInd <- cakalnica[izPraveSkupine] %>% min(na.rm = T) %>% 
            equals(cakalnica) %>% which %>% extract(1)
          if (prisotenVip) {
            # ce oseba VIP, jo pobrisemo
            vipCakajoci[naVrstiInd] <- 0
          }
          casPrihoda <- cakalnica[naVrstiInd]
          osebaNaVrsti <- as.integer(names(casPrihoda))  # Oseba iz cakalnice
          cakalnica[naVrstiInd] <- NA
          skupineCakajocih[naVrstiInd] <- 0
          stCak <- stCak - 1 # Zapusti cakalnico
          
          # Dobimo strezni cas za to osebo glede na porazdelitev
          # streznika, h kateremu bo oseba sla
          casStrezbe <- strezniCasi[osebaNaVrsti, porazd[prostiStreznik]] %>%
            as.numeric() 
          point <- getPosition(dogodki, cas + casStrezbe)  # lokacija dogodka
          dogodki <- insPosition(dogodki, cas + casStrezbe, point) # dodamo dogodek
          osebe <- insPosition(osebe, -osebaNaVrsti, point)  # Po vrsti kdaj 
          #                                                 oseba zapusti
          
          # Oseba je pri strezniku (skupinaOseba ostane ista, ker nadomesti
          # oseba iz iste skupine)
          strezniki[intersect(which(strezniki == 0), skupinaOseba)[1]] <- 
            osebaNaVrsti # Oseba je pri strezniku
          strezeneOsebe[osebaNaVrsti] <- 1 # Oseba je bila postrezena
          dogodkiOseb$zacetekStrezbe[osebaNaVrsti] <- cas  # Oseba iz cakanja 
          #                                                 je na vrsti
          dogodkiOseb$streznik[osebaNaVrsti] <- prostiStreznik # kateri streznik
          
        }
        # Ce nic od zgornjega je oseba ze bila postrezena, to je le ostanek, 
        # ker je bila nestrpna.
      }
    }
  }
  rezultati <- dogodkiOseb # za simulacijo
  
  return(rezultati)
}

# SIMULACIJA VRSTE ###########################################################
simulacija.vrste <- function(k, n, lambda, maxCas = NULL, maxPrihodi = NULL,
                             delezImp = 0, maxCakanje = 1/4, skupine = FALSE, 
                             VIP = 0, VIP.imp = FALSE, tipStr = "exp", 
                             stRazStr = 1, tipStrSkup = rep("exp", stRazStr),
                             porazd = rep(1, k), ...) {
  # k = stevilo streznih mest
  # n = stevilo cakalnih mest
  # lambda = intenziteta prihodov
  # maxCas, maxPrihodi = parametri za simulacija vrste
  # delezImp = delez nepotrpezljivih
  # maxCakanje = maximalen cas cakanja preden nepotrpezljivi oddide
  # skupine = ali imajo osebe preddolocene streznike (katerih je k)
  # VIP = delez vip oseb
  # VIP.imp = ce TRUE, potem vsi nepotrpezljivi pomembni, VIP zanemarjen
  # tipStr = tip porazdelitve v primeru ko je skupine == TRUE
  # stRazStr = koliko razlicnih skupin streznikov je, v primeru skupine == FALSE
  # tipStrSkup = tip porazdelitve skupin streznikov, podan v vektorju (mesto v
  #               vektorju odgovarja eni tisti skupini), ko skupine == FALSE
  # porazd = vektor, ki pove, kateri skupini pripada posamezen streznik, 
  #         ce skupine == FALSE, drugace se ne uposteva
  # ... = dodatni parametri za funkciji simulacija.strezb in 
  #       simulacija.strezb.razlicni
  # Simulacija prihodov
  if (is.null(maxCas) & is.null(maxPrihodi)) {
    errorCondition("Podan mora biti maxCas ali maxPrihodi!")
  } else if (xor(is.null(maxCas), is.null(maxPrihodi))) {
    prihodi <- simulacija.prihodov(lambda, maxCas, maxPrihodi)
  } else {
    errorCondition("Podan je lahko le maxCas ali maxPrihodi!")
  }
  # Simulacija nepotrpezljivih
  prihodi <- dodaj.nepotrpezljive(prihodi, delez = delezImp, cas = maxCakanje)
  # Dodamo preddolocene skupine, h katerim streznikom gredo katere osebe
  if (skupine) {
    stSkupin <- k
  } else {
    stSkupin <- 0
  }
  prihodi <- dodaj.skupine(prihodi, stSkupin, VIP, VIP.imp)
  # Funkcija za strezne case
  if (skupine) {
    strezniCasi <- simulacija.strezb(prihodi, tip = tipStr, ...)
  } else {
    strezniCasi <- simulacija.strezb.razlicni(prihodi, stSkupin = stRazStr,
                                              tip = tipStrSkup, ...)
  }
  # Simulacija strezb
  if (skupine) {
    porazd <- NULL
  }
  rez <- simulacija.poteka.vrste(k, n, prihodi, strezniCasi, porazd)
}

# TESTIRANJE ################################################################
rez <- simulacija.vrste(k = 4,
                        n = 20,
                        lambda = 30,
                        maxCas = 8,
                        maxPrihodi = NULL,
                        delezImp = 0.2,
                        maxCakanje = 1/4,
                        skupine = FALSE,
                        VIP = 0.1,
                        VIP.imp = FALSE,
                        tipStr = "exp",
                        stRazStr = 3,
                        tipStrSkup = rep("exp", 3),
                        porazd = c(1, 2, 2, 3),
                        mu = c(10, 8, 5)
)

# TODO MC simulacija, za nekatere vrednosti, grafi statistik





