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
  browser()
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
    mutate(stanje.cakalnice = cumsum(cakalnica.dogodek))
  # TODO kumulativa postrzenih in prislih
  return(rez.long)
}

rez.long <- preoblikuj.vrsto(rez)

