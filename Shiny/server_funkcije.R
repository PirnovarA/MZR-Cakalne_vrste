preveri.parametre <- function(porazd, stParam = 1, ...) {
  # Preveri ali parametri ustrezajo porazdelitvi, vrne TRUE ce vse OK, FALSE
  # drugace. Parametri so med seboj loceni z ';'
  param <- list(...)
  if (porazd == "exp") {
    mu <- strsplit(toString(param$mu), split = ";") %>% unlist() %>% 
      as.numeric()
    if (length(mu) == stParam) {
      if (!any(is.na(mu))) {
        if (all(mu >= 0)) {
          if (any(sapply(mu, function(x) !identical(x%%1, 0)))){
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }
}

preberi.inpute <- function(k,
                           n,
                           lambda,
                           checkbox_maxPrihodi,
                           maxCakanje,
                           maxPrihodi,
                           checkbox_imp,
                           impDelez,
                           impCas,
                           checkbox_vip,
                           checkbox_vipImp,
                           vipDelez,
                           radio_porazdStr,
                           enakoDist,
                           enakoMu,
                           skupineDist,
                           skupineMu,
                           stRazlicnihStr,
                           razlicniDistIsti,
                           razlicniMu,
                           dolocitevSkupinStr) {
  # Vrne seznam, kjer je na mestu 'status' koda statusa:
  # Koda:
  #   1: Vse okey
  #   2: - k, lambda manjsa ali enaka 0 
  #      - n manjsi od 0
  #      - k ali n nista celi stevili
  #   3: - Ali maxPrihodi ali maxCakanje je 0
  #      -  Ali maxPrihodi ni celo stevilo
  #   4: Delez nepotrpezljivih mora biti med 0 in 1
  #   5: Delez vip mora biti med 0 in 1
  #   6: Parametri porazdelitev so napacni
  #   7: Pri podajanju skupin streznikov je prislo do napake
  rezultati <- list()
  if (any(c(k, lambda) <= 0) | !identical(k%%1, 0)) {
    rezultati$status <- 2
    return(rezultati)
  }
  if (n < 0 | !identical(n%%1, 0)) {
    rezultati$status <- 2
    return(rezultati)
  }
  if ((checkbox_maxPrihodi & 0 %in% c(maxPrihodi)) |
      (checkbox_maxPrihodi & !identical(maxPrihodi%%1, 0)) |
      (!checkbox_maxPrihodi & 0 %in% c(maxCakanje))) {
    rezultati$status <- 3
    return(rezultati)
  }
  if (checkbox_imp) {
    if (impDelez < 0 | impDelez > 1) {
      rezultati$status <- 4
      return(rezultati)
    }
  }
  if (checkbox_vip) {
    if (!identical(checkbox_vipImp, T)) {
      checkbox_vipImp <- FALSE
      if (vipDelez < 0 | vipDelez > 1) {
        rezultati$status <- 5
        return(rezultati)
      }
    }
  }
  if (radio_porazdStr == "enako") {
    if (enakoDist == "exp") {
      if (!preveri.parametre(enakoDist, stParam = 1, mu = enakoMu)) {
        rezultati$status <- 6
        return(rezultati)
      }
    }
  } else if (radio_porazdStr == "skupine") {
    if (skupineDist == "exp") {
      if (!preveri.parametre(skupineDist, stParam = k, mu = skupineMu)) {
        rezultati$status <- 6
        return(rezultati)
      }
    }
  } else {
    if (razlicniDistIsti == "exp") {
      if (!preveri.parametre(razlicniDistIsti, stParam = stRazlicnihStr,
                             mu = razlicniMu)) {
        rezultati$status <- 6
        return(rezultati)
      }
    }
    # Skupine streznikov
    skupineStreznikov <- unlist(strsplit(dolocitevSkupinStr, ";")) %>% 
      as.numeric()
    if (length(skupineStreznikov) == k) {
      if (!any(is.na(skupineStreznikov))) {
        if (!all(skupineStreznikov %in% 1:stRazlicnihStr)) {
          rezultati$status <- 7
          return(rezultati)
        }
      } else {
        rezultati$status <- 7
        return(rezultati)
      }
    } else {
      rezultati$status <- 7
      return(rezultati)
    }
  }
  # Ce do sedaj ni bilo problema, je status 1
  rezultati$status <- 1
  # Priprava za uporabo v funkciji za generiranje
  if (checkbox_maxPrihodi) {
    maxCakanje <- NULL
  } else {
    maxPrihodi <- NULL
  }
  if (!checkbox_imp) {
    impDelez <- 0
    impCas <- 0
  }
  if (!checkbox_vip) {
    checkbox_vipImp <- FALSE
    vipDelez <- 0
  }
  if (radio_porazdStr == "enako"){
    skupine <- FALSE
    tipStr <- NULL
    tipStrSkup <- enakoDist
    stRazStr <- 1
    porazd <- rep(1, k)
    mu <- enakoMu
  } else if (radio_porazdStr == "skupine") {
    skupine <- TRUE
    tipStr <- skupineDist
    tipStrSkup <- NULL
    stRazStr <- k
    porazd <- NULL
    mu <- unlist(strsplit(toString(skupineMu), ";")) %>% as.numeric()
  } else {
    skupine <- FALSE
    tipStr <- NULL
    tipStrSkup <- rep(razlicniDistIsti, stRazlicnihStr)
    stRazStr <- as.numeric(stRazlicnihStr)
    porazd <- unlist(strsplit(toString(dolocitevSkupinStr), ";")) %>% as.numeric()
    mu <- unlist(strsplit(toString(razlicniMu), ";")) %>% as.numeric()
  }
  
  rez <- simulacija.vrste(k = k,
                          n = n,
                          lambda = lambda,
                          maxCas = maxCakanje,
                          maxPrihodi = maxPrihodi,
                          delezImp = impDelez,
                          maxCakanje = impCas,
                          skupine = skupine,
                          VIP = vipDelez,
                          VIP.imp = checkbox_vipImp,
                          tipStr = tipStr,
                          stRazStr = stRazStr,
                          tipStrSkup = tipStrSkup,
                          porazd = porazd,
                          mu = mu)
  rezultati$rez <- rez
  rezultati$zaGraf <- preoblikuj.vrsto(rez)
  return(rezultati)
}


