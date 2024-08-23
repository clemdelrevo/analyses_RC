
write_quarto <- function(bilan_station, data_pit_may) {

  #targets::tar_load(bilan_station)
  #targets::tar_load(data_pit_may)
  
  file.create("rc_report.qmd")
  
  site <- c(
    "bandrele", "boa", "boueni", "dzoumogne", "longoni", "majikavo", "mbouzi",       
    "mtsangamouji", "ngouja", "pes.deux", "pes.onze", "sakouli", "saziley",      
    "tanaraki", "tzoundzou"
    )
  
  type <- c(
    "Frangeant", "Interne", "Barrière", "Frangeant", "Interne", "Frangeant", "Frangeant",
    "Frangeant", "Frangeant", "Barrière", "Barrière", "Frangeant", "Frangeant",
    "Frangeant", "Frangeant"
  )
  
  pression <- c(
    "Moyenne", "Forte", "Faible", "Faible", "Moyenne", "Moyenne", "Très forte",
    "Moyenne", "Faible", "Faible", "Faible", "Faible", "Très Faible", 
    "Faible", "Faible"
  )
  
  correct_name <- stringr::str_to_upper(site)
  correct_name <- stringr::str_replace(correct_name, "PES.DEUX", "PASSE EN S BOUÉE 2")
  correct_name <- stringr::str_replace(correct_name, "PES.ONZE", "PASSE EN S BOUÉE 11")
  correct_name <- stringr::str_replace(correct_name, "MBOUZI", "M'BOUZI")
  correct_name <- stringr::str_replace(correct_name, "MTSANGAMOUJI", "M'TSANGAMOUJI")
  correct_name <- stringr::str_replace(correct_name, "BANDRELE", "BANDRÉLÉ")
  correct_name <- stringr::str_replace(correct_name, "NGOUJA", "N'GOUJA")
  
  names(correct_name) <- site
  names(type)         <- site
  names(pression)     <- site
  
  
  yaml <- glue::glue(
  "---
  title: 'Bilan des stations Reef Check'
  subtitle: 'Mayotte 2023-2024'
  format:
    pdf: default
  echo: FALSE
  geometry: 
    - top=16mm
    - left=15mm
    - right=15mm
    - bottom=20mm
  ---

  # [Auteurs]{.underline}
  
  Clément Delamare: Service de Plongée Scientifique  
  Sebastien Quaglietti: Service de Plongée Scientifique
  
  # [Contributeurs]{.underline}
  
  Flavien Foncin, Francois-Elie Paute, Léa Bernagou, Anna Roger, Charles Le Bozec, Annabelle Lapostolle, Dimitri Theuerkauff, Clémentine Cardon.
  
  # [Financeur]{.underline}
  
  **Direction de l’Environnement, de l’Aménagement, du Logement et de la Mer de Mayotte (DEALM)**  
  Terre-plein de M'tsapéré  
  97600 Mamoudzou  
  <deal-mayotte@developpement-durable.gouv.fr>
  
  # [Accès des données]{.underline}
  
  Le code reproduisant les analyses est disponible à l'adresse suivante: <https://github.com/clemdelrevo/analyses_RC.git>. PDF généré avec [Quarto](https://quarto.org).
  
  ::: {layout-ncol='3'}
  ![](rc_mayotte.jpeg){width='100' fig-align='left'}
  
  ![](rc_france.jpeg){width='200' fig-align='center'}
  
  ![](deal.png){width='100' fig-align='right'}
  :::
  
  {{< pagebreak >}}

  ",
  .open = "<<", .close = ">>"
  )
  
  final_bilan <- glue::glue_collapse(unlist(lapply(names(correct_name), function(i) {
  
    #i = "bandrele"
    dates <- unique(data_pit_may$standard_date[data_pit_may$site == i])
    last_date   <- dates[length(dates)]
    before_date <- dates[length(dates) - 1]
    cn <- correct_name[names(correct_name) %in% i]
    t  <- type[names(type) %in% i]
    p  <- pression[names(pression) %in% i]
    bilan <- bilan_station[[i]]
    
    survey_sheet <- (
    "
  
    # <<cn>>
    
    :::{layout-ncol=2}
  
    #
  
    - Date du suivi: <<last_date>>
    - Suivi précédant: <<before_date>>
  
    # 
  
    - Type de récif: <<t>>
    - Pressions issues du bassin versant: <<p>>
    
    :::
    
    ## Composition du substrat en 2024 et évolution du recouvrement en corail dur
    
    :::: {layout='[30, -2, 30]' fig-pos='H'}
    ::: {#first-column}
    <<bilan$cc>>
    :::
    
    ::: {#fig-sub-<<i>>}
    ![](outputs/mayotte/substrat/<<i>>_substrat_survey.png){width=90%}
    
    Composition du substrat (% moyen)
    :::
    ::::
    
    \
    
    ::: {#fig-cc-<<i>> fig-pos='H'}
    ![](outputs/mayotte/coral_cover/<<i>>_coral_cover.png){width=80%}
    
    Évolution du recouvrement en corail dur (%). Les points représentent le % de recouvrement en corail dur de chaque transect.
    :::

    {{< pagebreak >}}
    
    :::: {layout='[[10, -1, 20], [10, -1, 20]]' fig-pos='H'}
    
    ## Communautés ichtyologiques en 2024 et évolution
    
    ::: {#first-column}
    <<bilan$fish>>
    :::
    
    ::: {#fig-fish-<<i>>}
    ![](outputs/mayotte/fish/<<i>>_fish_survey.png)
    
    Évolution de la densité de poissons indicateurs. Barres d'erreurs = erreur standard.
    :::

    ## Communautés benthiques en 2024 et évolution
    ::: {#first-column}
    <<bilan$invert>>
    :::
    
    ::: {#fig-invert-<<i>>}
    ![](outputs/mayotte/invert/<<i>>_invert_survey.png)
    
    Évolution de la densité d'invertébrés indicateurs. Barres d'erreurs = erreur standard.
    :::
    ::::
    
    {{< pagebreak >}}
    
    "
    )
  
    glue_survey_sheet <- glue::glue(survey_sheet, .open = "<<", .close = ">>")
    
  })))
  
  final_sheet <- glue::glue_collapse(c(yaml, final_bilan))
  
  writeLines(final_sheet, "rc_report.qmd")
  
}

