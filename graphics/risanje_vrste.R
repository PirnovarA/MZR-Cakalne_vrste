preoblikuj.vrsto <- function(rez) {
  # Funkcija preoblikuje vrsto, zgenerirano s simulacija.vrste in vrne 
  # data.frame dogodkov oz stanj
  # Input
  # rez = data.frame rezultatov poteka vrste
  # Output
  # Data.frame v long obliki z dodanimi stolpci:
  #   dogodek: long oblika; prihod/odhod/zacetekStrezbe
  #   cas.dogodka: cas dogodka
  #   streznik.dogodek: 1/-1/0
  #   cakalnica.dogodek: 1/-1/0
  #   stanje.streznika: 1/0
  #   stanje.cakalnice: 0/1/.../n
  #   prihodCum: 0/1/...
  #   postrezenCum: 0/1/...
  rez.long <- rez %>%
    pivot_longer(
      cols = c(prihod, zacetekStrezbe, odhod),
      names_to = "dogodek",
      values_to = "cas.dogodka",
      values_drop_na = TRUE
    ) %>%
    arrange(cas.dogodka,
            match(dogodek, c("prihod", "odhod", "zacetekStrezbe"))) %>%
    mutate(
      streznik.dogodek = ifelse(
        dogodek == "zacetekStrezbe",
        1,
        ifelse(dogodek == "odhod" & odhodImp == 0 & zasedeno == 0, -1, 0)
      ),
      cakalnica.dogodek = ifelse(
        dogodek == "prihod" & cakanje == 1,
        1,
        ifelse(
          dogodek == "zacetekStrezbe" & cakanje == 1,
          -1,
          ifelse(dogodek == "odhod" & odhodImp == 1,
                 -1,
                 0)
        )
      )
    ) %>%
    group_by(streznik) %>% 
    mutate(stanje.streznika = cumsum(streznik.dogodek)) %>% ungroup() %>% 
    mutate(stanje.cakalnice = cumsum(cakalnica.dogodek)) %>% 
    mutate(prihod.temp = ifelse(dogodek == "prihod", 1, 0),
           postrezen.temp = ifelse(dogodek == "zacetekStrezbe", 1, 0),
           prihodCum = cumsum(prihod.temp), 
           postrezenCum = cumsum(postrezen.temp)) %>% 
    select(-prihod.temp, -postrezen.temp)
  return(rez.long)
}

temaGraf1 <- function() {
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.text = element_text(color = "grey"),
        axis.title = element_text(color = "grey"),
        title = element_text(color = "grey"),
        plot.title = element_text(hjust = 0.5),
        legend.direction = "horizontal",
        legend.title = element_blank(), 
        legend.text = element_text(color = "grey"),
        legend.position = "top",
        axis.line = element_line(color = "grey"),
        axis.ticks = element_line(color = "grey"),
        legend.background = element_rect(fill = "transparent"))
}

narisi.vrsto.cum <- function(rez.long) {
  maxCas <- rez.long %$% max(cas.dogodka)
  zaGraf <- rez.long %>% mutate(grp1 = cas.dogodka) %>% group_by(grp1) %>% 
    mutate(casZaGraf = cas.dogodka + rnorm(1,0, maxCas/1000000)) %>% 
    ungroup() %>% select(-grp1) %>% 
    pivot_longer(cols = c(prihodCum, postrezenCum), names_to = "polnilo",
                 values_to = "value") %>% 
    mutate(polnilo = factor(polnilo, labels = c("Strezbe", "Prihodi")),
           polnilo = factor(polnilo, levels = c("Prihodi", "Strezbe"))) %>% 
    select(polnilo, casZaGraf, value) %>% group_by(polnilo, casZaGraf) %>% 
    summarise(Stevilo = max(value)) %>% rename("Cas" = "casZaGraf")
  
  graf <- ggplot(data = zaGraf, aes(x = Cas)) +
    geom_area(aes(y = Stevilo, fill = polnilo, alpha = polnilo), 
              position = "identity") + 
    labs(x = "Cas", y = "Stevilo", title = "Kumulativni prihodi in strezbe") +
    scale_fill_manual(values=c("red","#30d189")) +
    scale_alpha_manual(values = c(1, 1)) + 
    temaGraf1()
  
  return(graf)
}

narisi.vrsto.cum.plotly <- function(rez.long) {
  graf <- narisi.vrsto.cum(rez.long)
  
  graf <- graf + theme(legend.direction = "vertical", 
               legend.position = "right") 
  graf %>%  ggplotly(tooltip = c("x", "y")) %>% layout(hovermode = "compare",
                                  legend = list(orientation = "h")) %>% 
    config(displayModeBar = F)
}

ugotovi.potek.vrste <- function(rez.long, k, n) {
  # Funkcija pripravi podatke za animacijo poteka vrste. Vrne seznam, v katerem
  # so casi, za katere so stanja vrste in seznami za vsako stanje. Imamo
  # snapshote stanj v casu za naslednje:
  #   casi: vsi casi za katere imamo podatke
  #   strezniki: seznam vektorjev ki prikazujejo id-je strezenih ob casu
  #   tipiStrezenih: seznam vektorjev ki prikazujejo tipe strezenih ob casu
  #   cakalnica: seznam vektorjev ki prikazujejo id-je cakajocih ob casu
  #   tipiCakajocih: seznam vektorjev ki prikazujejo tipe cakajocih ob casu
  
  vsiCasi <- c(0, rez.long$cas.dogodka %>% unique())
  stCasov <- length(vsiCasi)
  rez.long <- rez.long %>%
    mutate(tipOsebe = ifelse(VIP == 1 & imp == 1, "impVIP",
                             ifelse(
                               VIP == 1, "VIP",
                               ifelse(imp == 1, "imp",
                                      "basic")
                             ))) 

  sezStrezniki <- vector("list", stCasov) %>% lapply(function(x) numeric(k))
  sezCakalnica <- vector("list", stCasov) %>% lapply(function(x) numeric(n))
  sezTipiStrezniki <- vector("list", stCasov) %>% 
    lapply(function(x) character(k))
  sezTipiCakalnica <- vector("list", stCasov) %>% 
    lapply(function(x) character(n))
  
  # Zaporedoma lahko delamo tako, ker je rez.long urejen ne le po casih vendar
  # tudi po dogodkih, saj "prihod" < "odhod" < "zacetekStrezbe". Ker so casi 
  # zvezni dolocimo dogodku da oseba oddide in nova pride na isti cas verjetnost
  # 0
  kateriCasPrejsnji <- 1
  for (i in 1:nrow(rez.long)) {
    vrstica <- rez.long[i, ]
    cas <- vrstica$cas.dogodka
    kateriCas <- which(cas == vsiCasi)
    # Prvo vzamemo prejsnje stanje cakalnice/streznikov v zdejsnje
    sezStrezniki[[kateriCas]] <- sezStrezniki[[kateriCasPrejsnji]]
    sezTipiStrezniki[[kateriCas]] <- sezTipiStrezniki[[kateriCasPrejsnji]]
    sezCakalnica[[kateriCas]] <- sezCakalnica[[kateriCasPrejsnji]]
    sezTipiCakalnica[[kateriCas]] <- sezTipiCakalnica[[kateriCasPrejsnji]]
    if (vrstica$dogodek == "prihod") {
      # Oseba je prisla
      if (vrstica$cakanje == 1) {
        # Strezniki bili zasedeni, oseba gre cakat
        prostCak <- min(which(sezCakalnica[[kateriCas]] == 0))
        sezCakalnica[[kateriCas]][prostCak] <- vrstica$id
        sezTipiCakalnica[[kateriCas]][prostCak] <- vrstica$tipOsebe
      }
      
    } else if (vrstica$dogodek == "odhod") {
      # Oseba je odsla
      if (vrstica$zasedeno == 1) {
        # Strezna mesta in cakalnica so bila zasedena
      } else if (vrstica$odhodImp == 1) {
        # Nepotrpezljiva oseba je odsla
        cakMest <- which(sezCakalnica[[kateriCas]] == vrstica$id)
        sezCakalnica[[kateriCas]][cakMest] <- 0
        sezTipiCakalnica[[kateriCas]][cakMest] <- ""
      } else {
        # Oseba je koncala s strezbo in odsla
        sezStrezniki[[kateriCas]][vrstica$streznik] <- 0
        sezTipiStrezniki[[kateriCas]][vrstica$streznik] <- ""
      }
      
    } else if (vrstica$dogodek == "zacetekStrezbe") {
      # Oseba je zacela s strezbo
      if (vrstica$cakanje == 1) {
        # Oseba je cakala, gre iz cakalnice v strezbo
        cakMest <- which(sezCakalnica[[kateriCas]] == vrstica$id)
        sezCakalnica[[kateriCas]][cakMest] <- 0
        sezTipiCakalnica[[kateriCas]][cakMest] <- ""
        sezStrezniki[[kateriCas]][vrstica$streznik] <- vrstica$id
        sezTipiStrezniki[[kateriCas]][vrstica$streznik] <- vrstica$tipOsebe
      } else {
        # Oseba ni cakala, gre direktno v strezbo
        sezStrezniki[[kateriCas]][vrstica$streznik] <- vrstica$id
        sezTipiStrezniki[[kateriCas]][vrstica$streznik] <- vrstica$tipOsebe
      }
    }
    kateriCasPrejsnji <- kateriCas
  }
  
  rezultati <- list(
    casi = vsiCasi,
    strezniki = sezStrezniki,
    tipiStrezenih = sezTipiStrezniki,
    cakalnica = sezCakalnica,
    tipiCakajocih = sezTipiCakalnica
  )
}

