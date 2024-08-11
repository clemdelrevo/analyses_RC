
write_bilan <- function(export) {

  bilan_station <- list(
    
    bandrele = list(
      cc = glue::glue("Le recouvrement benthique sur la station de Bandrélé est principalement 
      **biotique** et largement représenté par le **corail dur** (@fig-sub-bandrele). 
      L’évolution du recouvrement corallien montre une importante mortalité corallienne en 2016, 
      certainement causé par la vague de chaleur du phénomène El Nino (@fig-cc-bandrele). 
      Le retour à un taux de recouvrement semblable à celui de 2012 observé en 2022 montre 
      une bonne résilience des peuplements coralliens sur cette station. La couverture corallienne moyenne 
      ne semble pas diminuer en 2024 sur la station mais est plus hétérogène entre les différents transects 
      que lors du suivi précédant."),
      fish = glue::glue("La principale famille ichtyologique représenté en 2024 sur la station
      de Bandrélé est celle des poissons papillons et la densité de ces poissons 
      reste stable par rapport aux précédentes années, probablement en lien avec 
      l’important taux de recouvrement en corail dur sur cette station (@fig-fish-bandrele). Les densités 
      de poissons perroquets (herbivores) sont très faibles dès 2012 et ne 
      montrent pas de variation entre 2012 et 2024. Aucune espèce piscivore n’a été 
      observée depuis la reprise des suivis en 2022 sur cette station."),
      invert = glue::glue("Les principaux taxons benthiques suivis par le réseau Reef Check 
      sont très peu représentés sur cette station en 2024 (@fig-invert-bandrele). 
      Une réduction importante de la densité d’oursins diadèmes est observé entre 2012 et 2022.")
      ),
    
    boa = list(
      cc = glue::glue("Le recouvrement benthique sur la station de Boa est principalement **abiotique**, 
      composé de **roche, de débris et de sable** (@fig-sub-boa). Après une progression du taux de recouvrement 
      en corail dur entre 2011 et 2015, celui ci entame un déclin continue entre 2016 et 2020 (@fig-cc-boa). 
      Aucune augmentation 'significative' du recouvrement en corail dur est observé depuis
      la reprise des suivis par le SPS. Le suivi régulier sur cette station montre une résilience 
      médiocre des peuplements coralliens sur ce site de récif interne, probablement en lien avec les 
      fortes pressions issues du bassin versant^[Ifrecor (2020) État de santé des récifs coralliens et écosystèmes associés des outre-mer français.]"),
      fish = glue::glue("Les **papillons et perroquets** représentent les seuls familles observées
      en 2024, avec très peu d'individus (@fig-fish-boa). L'évolution de la densité
      des papillons montre un **impact négatif marqué** de la perte en recouvrement corallien
      à partir de 2016 tandis que la trajectoire des perroquets semble décroitre de 
      manière plus linéaire. Si les poissons piscivores étaient déjà rarement observés, ils sont **absents**
      depuis 2020."),
      invert = glue::glue("Les invertébrés benthiques sont faiblement représentés sur 
      l'entièreté de la série temporelle et aucune tendance d'évolution ne se démarque clairement (@fig-invert-boa).")
      ),
    
    boueni = list(
      cc = glue::glue("Le substrat sur la station de Boueni est partagé entre le recouvrement par 
      le **corail dur** et la **roche** (@fig-sub-boueni). Les peuplements coralliens ont montré
      une **résistance médiocre** mais une capacité de **récupération importante** face à l'évènement thermique de 2016 sur 
      cette station profonde de récif barrière, éloignée des apports térrigènes de la côte (@fig-cc-boueni). Cependant,
      l'absence de suivi avant la reprise du réseau par le SPS ne permet pas de mettre en avant la vitesse de cette récupération."),
      fish = glue::glue("Les **papillons et perroquets** représentent les seuls familles observées
      en 2024 (@fig-fish-boueni). Ces familles semblent moins abondante à partir de 2021-2022 qu'en 2012.
      Les densités de **poissons piscivores** étaient les plus importantes en 2012 et sont les plus basses observées
      en 2024."),
      invert = glue::glue("Les **bénitiers**, en augmentation, sont les seuls invertébrés observés 
      sur la série temporelle hormis l'observation d'une seule holothurie en 2012 (@fig-invert-boueni).")
      ),
    
    dzoumogne = list(
      cc = glue::glue("Le recouvrement benthique sur la station de Dzoumogne est principalement **abiotique**,
      représenté majoritairemeent par du **sable et de la roche**, tandis que la **couverture corallienne** ne représente 
      qu'**un tiers** de la station (@fig-sub-dzoumogne). Le peu de données temporelles sur la trajectoire
      des peuplements coralliens de cette station ne permet pas d'émettre d'hypothèses sur leur capacité de résilience.
      Néanmoins, au regard de la période de suivi de cette station par rapport au phénomène de blachissement global intervenu courant 2024,
      la dégradation du taux de recouvrement corallien suggère plutôt un **impact anthropique local** sur ce récif frangeant, 
      certainement en relation avec les activités portuaires à proximité (@fig-cc-dzoumogne)."),
      fish = glue::glue("Les **papillons** représentent la majorité des observations de poissons indicateurs 
      sur la série temporelle, couplés à quelques observations de **perroquets** (@fig-fish-dzoumogne). Les **poissons piscivores**, très rarement comptés
      en 2012 ou en 2021-2022 sont totalement absent en 2023-2024."),
      invert = glue::glue("Les invertébrés benthiques indicateurs recensés sont principalement des **oursins diadèmes** et sont en
      **forte augmentation** depuis la reprise des suivis par rapport à 2012 (@fig-invert-dzoumogne). Les **bénitiers et les holothuries** sont également
      observés très ocasionnellement.")
      ),
    
    longoni = list(
      cc = glue::glue("Le recouvrement benthique sur la station de Longoni est principalement **abiotique**, composé principalement
      de **roche et de débris** (@fig-sub-longoni). Le **recouvrement corallien** représente environ **un tiers** de la station tandis que les macroalgues couvrent
      une part substancielle du substrat. L'évolution du taux de recouvrement montre 
      une **stabilisation du recouvrement** en corail dur depuis 2020, après que les peuplements coralliens aient 
      progressivement récupéré d'une mortalité intense entre 2015 et 2016 (@fig-cc-longoni). Le taux de recouvrement est revenu à son niveau 'historique' 
      pré-perturbation, qui semble plafonner entre **30 et 40%**, temoignant probablement d'un **stress chronique** sur cette station à proximité du seul port
      commercial de Mayotte."),
      fish = glue::glue("Les poissons indicateurs sont exclusivement représentés par les **papillons** en 2024 (@fig-fish-longoni). Cette famille semble en progression, après un déclin
      jusqu'en 2018, probablement en lien avec la mortalité corallienne et décalée dans le temps. Les **perroquets** sont rarement observés depuis
      2014. Les **piscivores**, déjà rarement observé sur toute la série temporelle, sont absent depuis 3 campagnes de suivis."),
      invert = glue::glue("Seuls **4 oursins diadèmes** ont été observés en 2024 sur la station (@fig-invert-longoni). Malgré tout, ce taxon semble en progression depuis 2016.
      Si l'observation de macroalgues n'est pas avéré précédemment, la mortalité corallienne à probablement permis la colonisation des squelettes 
      de carbonate de calcium par du turf algual, ce qui expliquerait l'augmentation des oursins herbivores à partir de 2016. La présence d'oursins
      remplissant la fonction d'herbivorie en l'abscence des poissons montre une certaine capacité de résilience de l'écosystème sur cette station.
      La présence de macroalgues en 2024 sur cette station couplé à la forte diminution des oursins, ainsi que la forte dégradation de 
      la station proche de Dzoumogne suggère un épisode de stress important dans la zone.")
      ),
    
    majikavo = list(
      cc = glue::glue("Le recouvrement benthique sur la station de Majikavo est principalement **biotique**, majoritairement du **corail dur**
      tandis que les **macroalgues** couvrent une part substancielle du substrat (@fig-sub-majikavo). Le recouvrement en corail dur est en augmentation (@fig-cc-majikavo)."),
      fish = glue::glue("Les poissons **papillons** représentent la quasi exclusivité des poissons sur cette station hormis l'observation 
      d'un gaterin lors du dernier suivi (@fig-fish-majikavo)."),
      invert = glue::glue("L'observation d'****un bénitier représente la seule observation d'invertébré indicateur cette année, en baisse par rapport
      à la dernière campagne (@fig-invert-majikavo).")
      ),
    
    mbouzi = list(
      cc = glue::glue("Le recouvrement benthique sur la station de M'bouzi est principalement **abiotique**, composé principalement de 
      **roche, débris et sable** (@fig-sub-mbouzi). Le recouvrement **biotique** est principalement partagé entre le **corail dur et le corail mou**.
      Le recouvrement en corail dur varie peu sur cette station de récif frangeant (@fig-cc-mbouzi) fortement exposé aux pressions provenant de la rivière Kwalé, induisant une 
      forte sédimentation et un envasement du substrat favorisant probablement la présence d'hétérotrophe comme les coraux mous."),
      fish = glue::glue("Les **papillons** sont les seuls poissons observés en 2024 et dont la densité semble diminuer depuis 2013. L'observation de poissons perroquets
      parait plus aléatoire sur la série temporelle (@fig-fish-mbouzi). Les poissons **piscivores** marquent par leur abscence depuis la reprise des suivis."),
      invert = glue::glue("Les densités d'invertébrés indicateurs sont globalement faibles sur l'ensemble de la série temporelle, hormis pour les 
      **oursins diadèmes** dont l'abondance est divisé par 5 entre 2012 et 2013 (@fig-invert-mbouzi).")
      ),
    
    mtsangamouji = list(
      cc = glue::glue("Le recouvrement benthique sur la station de M'tsangamouji est principalement **biotique**, composé de **corail dur**
      tandis que les **macroalgues** couvrent une part substancielle du substrat (@fig-sub-mtsangamouji). La couverture corallienne est légèrement plus importante qu'en
      2012 mais en baisse depuis la dernière campagne de suivi (@fig-cc-mtsangamouji)."),
      fish = glue::glue("Les **papillons** représentent la principale famille de poisson observé en 2024 (@fig-fish-mtsangamouji). Cette famille est en augmentation depuis 2012,
      peut être en lien avec l'augmentation du recouvrement en corail dur. Les poissons **piscivores** restent rares et les poissons **perroquets** n'ont jamais
      été observés."),
      invert = glue::glue("**Un bénitier et une holothurie** constitue les seules observations d'invertébrés benthiques en 2024 (@fig-invert-mtsangamouji). Les **oursins diadèmes**, très abondants en 
      2012, ont vu leur abondance divisé par 7 depuis la reprise des suivis par le SPS.")
      ),
    
    ngouja = list(
      cc = glue::glue("Le recouvrement benthique sur la station de N'gouja est principalement **biotique**, composé majoritairement de **corail dur**
      tandis que les **macroalgues** occupent une part substancielle du substrat (@fig-sub-ngouja). La trajectoire à la baisse continue du recouvrement en corail dur sur 
      l'ensemble de la série temporelle suggère une **mauvaise résilience** des peuplements coralliens sur cette station de récif frangeant à l'afflux touristique important (@fig-cc-ngouja)."),
      fish = glue::glue("Les **papillons** représentent la principale famille de poisson observée en 2024 avec seulement 4 individus (@fig-fish-ngouja). La diminution de la densité de poissons de cette famille
      est certainement à mettre en lien avec la diminution constante en corail dur. **Perroquets et piscivores** sont absents depuis 2 campagnes de suivis et restaient très occasionnellement observés
      les années précedantes."),
      invert = glue::glue("La présence de **deux holothuries** et d'**un oursin diadème** constitue les seuls observations d'invertébrés benthiques en 2024 (@fig-invert-ngouja). Si les **holothuries**
      sont très rarement observées sur l'ensemble de la série temporelle, les densités d'**oursins diadèmes et de bénitiers** pouvaient être importante en 2012 et 2014
      mais tendent à la baisse depuis.")
    ),
    
    pes.deux = list(
      cc = glue::glue("Le recouvrement benthique sur la station de la bouée 2 de la passe en S est principalement **abiotique**, composé principalement
      de **sable et de débris**, conditions plutôt normales sur cette station localisé en bordure de passe à épandage détritique (@fig-sub-pes.deux). Le recouvrement en corail dur ne représente 
      qu'un tiers de la station. L'évolution du recouvrement corallien montre l'importante mortalité suite au stress thermique de 2016 sur cette station 
      de récif barrière, dont les peuplements sont à la base principalement constitués d'espèces branchus et tabulaires (@fig-cc-pes.deux).
      L'amélioration mitigé de la part du recouvrement corallien suggère une **résilience médiocre** de ces peuplements thermiquement sensibles."),
      fish = glue::glue("Les peuplements de poissons sont **faiblement représentés** en 2024 et les densité des principaux groupes fonctionnels **tendent à la baisse** sur l'ensemble
      de la série temporelle (@fig-fish-pes.deux). Si la trajectoire des **papillons** peut être lié à la diminution du recouvrement en corail dur, celle des **perroquets et piscivores**
      suggère d'autres types de pressions tel que la pêche sur cette station classé en réserve de pêche."),
      invert = glue::glue("Les invértébrés sont **faiblement représentés** sur cette station en 2024 (@fig-invert-pes.deux). En particulier, on note une tandance à la baisse des **holothuries**, espèces d'interêts
      écologiques et sensibles aux pressions d'origines anthropiques. Aucun commerce d'holothurie n'est pourtant recensé sur Mayotte.")
      ),
    
    pes.onze = list(
      cc = glue::glue("Le recouvrement benthique sur la station de la bouée 11 de la passe en S est principalement **biotique**, composé majoritairement de **corail dur** (@fig-sub-pes.onze).
      L'évolution du recouvrement corallien montre une **dégradation continue** entre 2016 et 2020, compensé en deux ans par une **remarquable amélioration**, probablement au profit
      d'espèces digités à fort taux de croissance sur cette station de bordure de passe à peuplement d'acropores digités et tabulaires (@fig-cc-pes.onze)."),
      fish = glue::glue("L'évolution de la densité des peuplements de poissons est fluctuante et à relativiser sur cette station à très faible profondeur (@fig-fish-pes.onze). On note malgré tout
      une tendance à la baisse des **perroquets** et la rareté des **piscivores** dans une zone classé réserve de pêche."),
      invert = glue::glue("Les principaux taxons benthiques sont **très peu représentés** sur cette station en 2024 et constitués principalement de **bénitiers et
      d'holothuries** (@fig-invert-pes.onze). L'évolution de la densité de **bénitier** tend à la baisse sur l'ensemble de la série temporelle tandis que celle des **holothuries** est plus aléatoire.")
      ),
    
    sakouli = list(
      cc = glue::glue("Le recouvrement benthique sur la station de Sakouli est principalement **biotique**, en grande partie occupé par du **corail dur** (@fig-sub-sakouli).
      L'évolution du recouvrement corallien montre une **bonne résilience** des peuplements coralliens sur Sakouli, avec un impact modéré du stress
      thermique de 2016 sur le recouvrement et un retour proche du stade probablement 'climax' constaté depuis la reprise de suivis pas le SPS (@fig-cc-sakouli)."),
      fish = glue::glue("Les peuplements de poissons sont principalements constitués de **papillons** et en moindre mesure de **perroquets** en 2024 (@fig-fish-sakouli). Les 
      poissons **piscivores** restent absents si l'on occulte la présence du seul mérou observé en 2021-2022."),
      invert = glue::glue("La présence de **deux bénitiers** constitue les seuls observations d'invertébrés benthiques en 2024 (@fig-invert-sakouli). Les peuplements 
      d'invertébrés benthiques deumeurent **faibles** depuis la reprise des suivis, notamment pour les **oursins diadèmes** dont l'abondance semblait importante 
      en 2012.")
      ),
    
    saziley = list(
      cc = glue::glue("Le recouvrement benthique sur la station de Saziley est principalement **abiotique** et largement représenté par les **débris et la roche** (@fig-sub-saziley).
      Le **corail dur** n'occupe qu'un tiers de la station. L'évolution du recouvrement en corail dur montre une **amélioration entre 2016 et 2019** mais 
      une **importante dégradation** d'environ 40% depuis la reprise des suivis (@fig-cc-saziley)."),
      fish = glue::glue("Les peuplements de poissons sont principalements constitués de **papillons** et en moindre mesure de **perroquets** en 2024 (@fig-fish-saziley).
      Les papillons semblent en augmentation depuis 2018 tandis que la trajectoire des perroquets ne montre pas de variation claires. Les **piscivores**
      restent **très rares** sur cette station."),
      invert = glue::glue("Les peuplements d'invertébrés sont principalements constitués de **bénitiers** en 2024, famille dont l'abondance semble
      **en hausse** depuis 2018 (@fig-invert-saziley). Les **oursins diadèmes**, bien que peu abondants sur l'ensemble de la série temporelle sont absents en 2024.")
      ),
    
    tanaraki = list(
      cc = glue::glue("Le recouvrement benthique sur la station de Tanaraki est principalement **biotique**, pour les trois-quart occupé
      par du **corail dur** (@fig-sub-tanaraki). La récupération du recouvrement corallien après l'importante mortalité de 2016 suggère une **bonne résilience** des peuplements coralliens
      sur cette station dominée par les acropores digités (@fig-cc-tanaraki)."),
      fish = glue::glue("Les peuplements de poissons sont **faiblement représentés** sur cette station, 
      principalement constitués de **papillons** et en moindre mesure de **perroquets** en 2024 (@fig-fish-tanaraki).
      La densité de papillons **tend à la hausse** malgré une année 2020 marqué par leur abscence. Les autres groupes fonctionnels restent **rares**
      sur l'ensemble de la série temporelle."),
      invert = glue::glue("Les peuplements d'invertébrés sont **très faiblement représentés** en 2024 et majoritaiement par les **oursins diadèmes** sur l'ensemble de
      la série temporelle (@fig-invert-tanaraki). En particulier, on observe une **importante diminution** du nombre d'oursins diadèmes depuis 2012.")
      ),
    
    tzoundzou = list(
      cc = glue::glue("Le recouvrement benthique sur la station de Tzoundzou est principalement **abiotique**, majoritairement de la **roche** (@fig-sub-tzoundzou).
      Le recouvrement corallien représente environ un tiers de la station sur l'ensemble de la série temporelle et est en augmentation
      depuis la dernière campagne de suivi (@fig-cc-tzoundzou)."),
      fish = glue::glue("Les **papillons** représentent la seule famille de poissons en 2024 et sur l'ensemble des suivis, hormis l'observation
      de quelques **perroquets** en 2012 (@fig-fish-tzoundzou). Les **piscivores**, à l'instar de l'ensemble des stations Reef Check manquent sur cette station."),
      invert = glue::glue("L'observation d'**un bénitier** constitue la seule observation d'invertébrés indicateurs sur cette station, qui restent rares
      malgré le comptage de **quelques oursins diadèmes** en 2021-2022 (@fig-invert-tzoundzou).")
      )
    
  )
  
  return(bilan_station)
  
}
