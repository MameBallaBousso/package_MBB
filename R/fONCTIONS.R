
Auto_traite1 <- function(base_cons_ehcvm, Table_de_conversion_phase_2) {
  library(haven)
  library(dplyr)
  library(readxl)
  library(tidyverse)
  library(data.table)
  library(ggplot2)


  #Renommer
  colnames(base_cons_ehcvm)[3] <- c("Produit_id")
  colnames(base_cons_ehcvm)[4:14] <- c("AutresProd","Qtty_cons",
                                       "Unite_cons","Taille_cons",
                                       "AutoCons","AutresProv",
                                       "DernierAchat","Qtty_achat",
                                       "Unite_achat","Taille_achat",
                                       "Value_achat")

  #Apurement

  #Suppression des ménages qui ne consomment pas
  base_cons_ehcvm <- na.omit(base_cons_ehcvm, "Qtty_cons")
  #Vérifier s'il ya des doublons
  doublons <- any(duplicated(base_cons_ehcvm))

  # 2. Calculs :
  #---------La quantité consommée en kilogramme---------


  return(base_cons_ehcvm)
}


Auto_traite2 <- function(base_cons_ehcvm, Table_de_conversion_phase_2) {

  library(dplyr)
  tableconves<- Table_de_conversion_phase_2 %>%
    select(-c(8,9)) %>%
    mutate(produit=factor(produitID,
                          levels=produitID,
                          labels=produitNom
    ),
    unite_cons=factor(uniteID,levels=uniteID,
                      labels=uniteNom),
    taille_cons=factor(tailleID,levels=tailleID,
                       labels=tailleNom))


  base_cons_ehcvm1 <- merge(base_cons_ehcvm,tableconves,
                            by.x = c("Produit_id","Unite_cons","Taille_cons"),
                            by.y = c("produitID","uniteID","tailleID"),
                            all.x = T)

  ## Poids
  base_cons_ehcvm1 <- data.table(base_cons_ehcvm1)
  setnames(base_cons_ehcvm1,"poids","poids_cons")


  ## NA dans poids
  anyNA(base_cons_ehcvm1$poids_cons)
  sum(is.na(base_cons_ehcvm1$poids_cons))

  # Vérifier si poids_cons est numérique
  is.numeric(base_cons_ehcvm1$poids_cons)
  # on convertit poids en numeric
  base_cons_ehcvm1[,poids_cons:=as.numeric(poids_cons)]

  ## Quantity conso en unite standard (kg)
  base_cons_ehcvm1 [, qtty_cons_kg:= poids_cons*Qtty_cons/1000]

  glimpse(base_cons_ehcvm1)

  return(base_cons_ehcvm1)
}




# Il faut mettre le lien pour ouvrir la base ehcvm_conso concernée sous le nom
# base_cons_ehcvm

#base_cons_ehcvm <- lien_vers cette base


Auto_traite3 <- function(base_cons_ehcvm, Table_de_conversion_phase_2) {
  library(dplyr)
  tableconves<- Table_de_conversion_phase_2 %>%
    select(-c(8,9)) %>%
    mutate(produit=factor(produitID,
                          levels=produitID,
                          labels=produitNom
    ),
    unite_cons=factor(uniteID,levels=uniteID,
                      labels=uniteNom),
    taille_cons=factor(tailleID,levels=tailleID,
                       labels=tailleNom))


  output_qtté_achat <- merge(base_cons_ehcvm,tableconves,
                             by.x = c("Produit_id","Unite_achat","Taille_achat"),
                             by.y = c("produitID","uniteID","tailleID"),
                             all.x = T)

  output_qtté_achat <- na.omit(output_qtté_achat, "poids")
  setnames(output_qtté_achat,"poids","poids_achat")
  sum(is.na(viandes2$poids_achat))
  output_qtté_achat <- data.table(output_qtté_achat)

  output_qtté_achat[,poids_achat:=as.numeric(poids_achat)]

  ## Quantity consommée en unite standard (kg)
  output_qtté_achat [, qtty_achat_kg:= poids_achat*Qtty_achat/1000]


  output_qtté_achat[, pu:= Value_achat/qtty_achat_kg]
  #viandes2[Unite_achat==100, summary(pu)]

  ### Extraire les Prix
  prixunitaire <- subset(output_qtté_achat, !is.na(pu),
                         select =c("Produit_id", "Unite_achat", "Taille_achat", "pu"))
  glimpse(prixunitaire)

  #Supression de prix de la base viandes1
  output_qtté_achat <- output_qtté_achat %>%
    select(-pu)



  #Corriger les valeurs abérantes du prix pu (au niveau de la base prixunitaire)

  for (i in unique(prixunitaire$Produit_id)) {
    indices <- which(prixunitaire$Produit_id == i)
    for (j in indices) {
      if (!is.na(prixunitaire$pu[j]) && prixunitaire$pu[j] > quantile(prixunitaire$pu[indices], 0.75, na.rm = TRUE)) {
        prixunitaire$pu[j] <- quantile(prixunitaire$pu[indices], 0.75, na.rm = TRUE)
      }
    }
  }

  #Calculer les prix moyens et médiane pour chaque type de viande
  prixunitaire2<- prixunitaire %>%
    group_by(Produit_id,Unite_achat,Taille_achat) %>%
    mutate(pu_mean = mean(pu, na.rm = T), pu_median=median(pu,na.rm=T))
  glimpse(prixunitaire2)


  Val_cons_kg <- merge(base_cons_ehcvm1, prixunitaire2,
                       by.x=c("Produit_id","Unite_cons","Taille_cons"),
                       by.y = c("Produit_id", "Unite_achat", "Taille_achat"),
                       all.x = T, allow.cartesian=TRUE)
  glimpse(Val_cons_kg)

  # Evaluer le taux de matching

  anyNA(Val_cons_kg$pu_mean)
  Val_cons_kg <- Val_cons_kg %>%
    mutate(Ntrue = ifelse(is.na(pu_mean)==F,1,0))

  table(Val_cons_kg$Ntrue)
  tauxMatching <- 100*sum(Val_cons_kg$Ntrue)/(dim(Val_cons_kg)[1])
  tauxMatching

  setnames(Val_cons_kg,"pu","prix_FCFA")

  Val_cons_kg [, Valeur_Cons_FCFA:= prix_FCFA*qtty_cons_kg]

  glimpse(Val_cons_kg)

  return(Val_cons_kg)
}


Auto_traite4 <- function(Val_cons_kg, EhcvmMod) {

  #Génération d’une variable taille du ménage pour chaque ménage

  library(haven)
  colnames(EhcvmMod)[2:3]<- c("Région", "Milieu")
  glimpse(EhcvmMod)

  #Récupérer les labels dans les colonnes Région et Milieu

  levprod <- unique(EhcvmMod$Région)
  levprod1 <- unique(EhcvmMod$Milieu)

  #edit(levprod)
  #edit(levprod1)


  levprodN <- names(attr(EhcvmMod$Région,"labels"))
  levprodL <- unname(attr(EhcvmMod$Région,"labels"))

  levprodN1 <- names(attr(EhcvmMod$Milieu,"labels"))
  levprodL1 <- unname(attr(EhcvmMod$Milieu,"labels"))

  EhcvmMod$Région <- as.factor(EhcvmMod$Région)
  EhcvmMod$Milieu <- as.factor(EhcvmMod$Milieu)

  glimpse(EhcvmMod)

  table(EhcvmMod$Région)

  EhcvmMod$Région <- factor(EhcvmMod$Région,
                            levels = levprodL,
                            labels = levprodN )
  EhcvmMod$Milieu <- factor(EhcvmMod$Milieu,
                            levels = levprodL1,
                            labels = levprodN1)

  glimpse(EhcvmMod)

  #Création d'une nouvelle colonne donnant la taille du ménage
  #J'ai pas importé la base contenant le tableux ANSD, mais les données sont conformes
  #à celles données lors du dernier recensement

  EhcvmMod <- EhcvmMod %>%
    mutate(Taille_mean_men = case_when(
      Région == "DAKAR" ~ 6,
      Région == "ZIGUINCHOR" ~ 7,
      Région == "DIOURBEL" ~ 11,
      Région == "SAINT-LOUIS" ~ 9,
      Région == "TAMBACOUNDA" ~ 12,
      Région == "KAOLACK" ~ 11,
      Région == "THIES" ~ 9,
      Région == "LOUGA" ~ 9,
      Région == "FATICK" ~ 10,
      Région == "KOLDA" ~ 11,
      Région == "MATAM" ~ 12,
      Région == "KAFFRINE" ~ 12,
      Région == "KEDOUGOU" ~ 8,
      Région == "SEDHIOU" ~ 12,
      TRUE ~ NA_real_  # Pour gérer les régions non spécifiées
    ))

  #Consommation par tête
  #Fusion avec la base Val_cons_kg
  Ehcvm_cons_finale <- merge(base_cons_ehcvm1, EhcvmMod,
                             by.x=c("interview__key"),
                             by.y=c("interview__key"), all.y = T)


  Ehcvm_cons_finale <- na.omit (Ehcvm_cons_finale, "qtty_cons_kg")

  glimpse(Ehcvm_cons_finale)

  Ehcvm_cons_finale$conso_par_tete <- Ehcvm_cons_finale$qtty_cons_kg / Ehcvm_cons_finale$Taille_mean_men

  library(dplyr)

  consommation_moyenne_Région <- Ehcvm_cons_finale %>%
    group_by(Région) %>%
    summarise(conso_mean_par_tete = mean(conso_par_tete))
  View(consommation_moyenne_Région)

  glimpse(Ehcvm_cons_finale)
  #Consommation moyenne par milieu-région
  Ehcvm_cons_finale<- Ehcvm_cons_finale %>%
    group_by(Région) %>%
    mutate(cons_mean_région = mean(qtty_cons_kg, na.rm = T))

  #Par milieu
  Ehcvm_cons_finale<- Ehcvm_cons_finale %>%
    group_by(Milieu) %>%
    mutate(cons_mean_milieu = mean(qtty_cons_kg, na.rm = T))

  return(Ehcvm_cons_finale)
}
