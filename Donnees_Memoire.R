######################################################################################
#####                                                            #####################
##### MEMOIRE M1: Vaccination infantile en Afrique Subsaharienne #####################
#####  enfant de 12-23 mois                                      #####################
###################################################################################### 

# Chemin d'accèes aux fichiers
setwd("C:/Users/nosty/Desktop/MEMOIRE/DocMemoire/Données/RCI/CIKR81DT")


#===== Import des librairies nécessaires====


library(haven)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(sf)
library(rnaturalearth)
library(ggplot2)

#====IMPORTER LES BASES DE DONNEES====
#** I- IMPORT DES BASES DE DONNES DES DIFFERENTS PAYS**

#** COTE D'IVOIRE**
#*
CIK <- read_dta("CIKR81FL.DTA")

# Menage
CIKM<-read_dta("C:/Users/nosty/Desktop/MEMOIRE/DocMemoire/Données/MENAGE/CIHR81DT/CIHR81FL.DTA")

#** MADAGASCAR**

MDKR <- read_dta("C:/Users/nosty/Desktop/MEMOIRE/DocMemoire/Données/MADAGASCAR/MDKR81DT/MDKR81FL.DTA")

#Ménage
MDKRM<- read_dta("C:/Users/nosty/Desktop/MEMOIRE/DocMemoire/Données/MENAGE/MDHR81DT/MDHR81FL.DTA")


#** GABON**
GAB<- read_dta("C:/Users/nosty/Desktop/MEMOIRE/DocMemoire/Données/GABON/GAKR71DT/GAKR71FL.DTA")

#*Ménage
GABM<- read_dta("C:/Users/nosty/Desktop/MEMOIRE/DocMemoire/Données/MENAGE/GAHR71DT/GAHR71FL.DTA")  

#** TANZANIE**

TANZ <- read_dta("C:/Users/nosty/Desktop/MEMOIRE/DocMemoire/Données/TANZANI/TZKR82DT/TZKR82FL.DTA")

# Menage 
TANZM <- read_dta("C:/Users/nosty/Desktop/MEMOIRE/DocMemoire/Données/MENAGE/TZHR82DT/TZHR82FL.DTA")


#==== CONSTRUCTION DES VARIABLES D4INTERETS====

menage<- c('hv001','hv002', 'hv025', 'hv219', 'hv270a', 'hv207', 'hv208', 'hv243a')

mere_enfant <- c(
   'bidx','v000', 'v001', 'v002', 'v012', 'm14', 'm15','v467b', 'v467d', 'v731','v732','v714',
   'v743a', 'v743b', 'v157','v158', 'v159','v106', 'v169a','v171a',
   'b19','b4', 'bord', 'b0', 'b5', 'h2', 'h4', 'h6','h7', 'h8', 'h3','h5', 'h9' 
)

#==== Nouvelle bases de données avec les variables d'intéret====

#===* PAYS: COTE D'IVOIRE*====
# Mères enfants
Civ_MF <- subset(CIK, select = mere_enfant)
names(Civ_MF)

# Selection des variables ménages 
CivM <- subset(CIKM, select = menage)

#=====Filtrer la base mère enfant pour ne rétenir que les enfant de 12- 23 mois et vivant et unique====
## Cela évide  le problème de multicolinéarite dans les CPN
Civ_MF_F <- Civ_MF%>%
  filter(Civ_MF$b19 >= 12 & Civ_MF$b19 <= 23 & Civ_MF$b5==1 & Civ_MF$bidx==1)

Civ_MF_F <- Civ_MF_F %>% distinct(across(all_of(c('v001', 'v002'))), .keep_all = TRUE)

# Rénommer des colonnes  de ménage  pour le merge des deux dataframe
CivM <- CivM %>%
  rename('v001' = 'hv001',
         'v002' = 'hv002')

BaseRCI <- inner_join(Civ_MF_F , CivM, by = join_by('v001', 'v002'))

#====Variables accès a l'infos de mères ACM femme====
varACM1 <- c('v157', 'v158','v159', 'v171a', 'v169a')
ACM1 <- subset(BaseRCI, select =varACM1 )
colSums(is.na(ACM1))

# Convertir toutes les colonnes de ACM1 en facteurs
ACM1 <- sapply(ACM1, factor)

# Convertir le résultat en data.frame
ACM1 <- data.frame(ACM1)

# Recoder la variable v157 en une variable binaire
table(ACM1$v158)
ACM1 <- ACM1 %>%
  mutate(v158 = ifelse(v158 %in% c("1", "2"), '1', '0'))

table(ACM1$v157)
ACM1 <- ACM1 %>%
  mutate(v157 = ifelse(v157 %in% c("1", "2"), '1', '0'))

table(ACM1$v159)

ACM1 <- ACM1 %>%
  mutate(v159 = ifelse(v159 %in% c("1", "2"), '1', '0'))

table(ACM1$v171a)
ACM1 <- ACM1 %>%
  mutate(v171a = ifelse(v171a %in% c("1", "2"), '1', '0'))

# Faire l'acm
res.ACM1 <- MCA(ACM1, graph = FALSE)
summary(res.ACM1)
res.ACM1$ind$coord

# Extraire les contributions des individus à la première dimension et arrondir
ind_coord<- round(res.ACM1$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseRCI$acces_infos_mere <- ind_coord

#==== FRécodage de certaines variable====

#  CPN: Nombre de consultation inf àu 3 = 0 ou 98 Don't know= 0
table(BaseRCI$m14)
BaseRCI <- BaseRCI %>%
  mutate(m14= ifelse(m14  %in% c(0,1,2,3, 98), 0, 1))

# la distance du centre de centé  1= GRos problème, 2= pas un gros problème
table(BaseRCI$v467d)

# Le lieu d'accouchement, = 0(10: 19 = home , 96= autre ==> home)  Hopital= 1
table(BaseRCI$m15)
BaseRCI <- BaseRCI %>%
  mutate(m15= ifelse(m15  %in% c(10:19, 96), 0, 1))

# Niveau d'éducation de la mère 0= No éducation, 1= Primaire, 2= Secondaire, 3= Super
table(BaseRCI$v106)

# Sexe de l'enfant  0= Male, 1= Female
table(BaseRCI$b4)
BaseRCI <- BaseRCI %>%
  mutate(b4 = ifelse(b4 == 1, 0, 1))

# Numero d'odre de naissance
table(BaseRCI$bord)


### Filtrer par midx ou bidx pour ne rétenire que les naissances uniques(car les
## jumeuax  entraine une répetion dans la colonne cpn = la meme CPN est répeter pour 
## les deux === > problème de multicolinéarité)

#===Variable de pouvoir de décision des femmes====

# Lorsque l’enfant est gravement malade, peut décider si
    ## Variable que des NA

# Obtenir l'autorisation de partir, 0= No Problem, 1= Big Problem, 2= Not a big problem
table(BaseRCI$v467b); sum(is.na(BaseRCI$v467b))

# Dernier mot en famille sur les Soins de santé pour les femmes vivants en couple
#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     NA= femmes célibataires

table(BaseRCI$v743a); sum(is.na((BaseRCI$v743a)))    # 267 NA

BaseRCI <- BaseRCI %>%
  mutate(v743a = ifelse(v743a %in% c(4, 5, 6), 0,
                        ifelse(v743a %in% c(2,3), 2, v743a)))

# Personne qui : Effectuer des achats importants à la maison

#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     NA= femme vivants seules

table(BaseRCI$v743b); sum(is.na((BaseRCI$v743b)))  # 267 NA

BaseRCI <- BaseRCI %>%
  mutate(v743b = case_when(
    v743b %in% c(4, 5,6) ~ 0,
    v743b %in% c(2,3) ~ 2,
    TRUE ~ v743b
  ))

#==== ACCES DES FEMMES AU MARCHE DU TRAVAIL====

# Le répondant a travaillé au cours des 12 derniers mois (femmes)

table(BaseRCI$v731); sum(is.na(BaseRCI$v731))

# Travaille toute l'année, de façon saisonnière ou 
#           1   all year
#           2   seasonal
#           3 occasional 
#           NA: celles qui n'ont pas travaillées les 12 mois mois précédent l'enquête
table(BaseRCI$v732); sum(is.na(BaseRCI$v732))   # 618 NA

# la Répondante travaille actuellement
#               0=No 
#               1=Yes 
 
table(BaseRCI$v714); sum(is.na(BaseRCI$v714)) 
  
#=== VACCINATION INFANTILE ====

#     0 <== NO = 0 & 8     ===> Non vacciné
#     1 <==YES =  1, 2, 3 ====> vacciné


#  verifications des modalitées et des valeurs manquantes
table(BaseRCI$h2); sum(is.na(BaseRCI$h2))     # BCG
table(BaseRCI$h3); sum(is.na(BaseRCI$h3))     # DTC 1
table(BaseRCI$h4); sum(is.na(BaseRCI$h4))     # POLIO 1
table(BaseRCI$h5); sum(is.na(BaseRCI$h5))     # DTC 2
table(BaseRCI$h6); sum(is.na(BaseRCI$h6))     # POLIO 2
table(BaseRCI$h7); sum(is.na(BaseRCI$h7))     # DTC 3
table(BaseRCI$h8); sum(is.na(BaseRCI$h8))     # POLIO 3
table(BaseRCI$h9); sum(is.na(BaseRCI$h9))     # Rougéole 1

#     0 <== NO = 0 & 8     ===> Non vacciné
#     1 <==YES =  1, 2, 3 ====> vacciné

# Définir les noms des variables à modifier
vacc_names <- c('h2','h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9')

# Utiliser une boucle pour appliquer le même code à chaque variable
for (vacc in vacc_names) {
  BaseRCI <- BaseRCI %>%
    mutate(!!sym(vacc) := ifelse(!!sym(vacc) %in% c(0, 8), 0, 1))
}

#=== CREATION DE LA COLONNE STATUT VACC ====

BaseRCI <- BaseRCI %>%
  mutate(statut_vacc = ifelse(if_all(all_of(vacc_names), ~ .x == 1), 1, 0))


#====Variables accès a l'infos des ménages ====
varACM_M <- c('hv207', 'hv208','hv243a')
ACM_M <- subset(BaseRCI, select =varACM_M )
colSums(is.na(ACM_M))

# Convertir toutes les colonnes de ACM1 en facteurs
ACM_M <- sapply(ACM_M, factor)

# Convertir le résultat en data.frame
ACM_M <- data.frame(ACM_M)

# Faire l'acm
res.ACM_M1 <- MCA(ACM_M, graph = FALSE)
summary(res.ACM_M1)
res.ACM_M1$ind$coord

# Extraire les coordonnées des individus à la première dimension et arrondir
ind_coord_M<- round(res.ACM_M1$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseRCI$acces_infos_menage <- ind_coord_M

BaseRCI$Id <- paste(BaseRCI[['v000']], BaseRCI[['v001']], BaseRCI[['v002']], sep = "-")

# Exporter la base en CSV
write.csv(BaseRCI, 'BaseCIV.csv', row.names = FALSE)

#===* PAYS: MADAGASCAR*====
# Mères enfants
Mdg_MF <- subset(MDKR, select = mere_enfant)

# Menage
MdgM <- subset(MDKRM, select = menage)

## Cela évide  le problème de multicolinéarite dans les CPN
Mdg_MF_F <- Mdg_MF%>%
  filter(Mdg_MF$b19 >= 12 & Mdg_MF$b19 <= 23 & Mdg_MF$b5==1 & Mdg_MF$bidx==1)

# Supression des doublons
Mdg_MF_F <- Mdg_MF_F %>% distinct(across(all_of(c('v001', 'v002'))), .keep_all = TRUE)

# Rénommer des colonnes  de ménage  pour le merge des deux dataframe
MdgM <- MdgM %>%
  rename('v001' = 'hv001',
         'v002' = 'hv002')

BaseMDK <- inner_join(Mdg_MF_F , MdgM, by = c('v001', 'v002'))

#====Variables accès a l'infos de mères ACM femme====
varACM2 <- c('v157', 'v158','v159', 'v171a', 'v169a')
ACM2 <- subset(BaseMDK, select =varACM2 )
colSums(is.na(ACM2))

# Convertir toutes les colonnes de ACM2 en facteurs
ACM2 <- sapply(ACM2, factor)

# Convertir le résultat en data.frame
ACM2 <- data.frame(ACM2)

# Recoder la variable v157 en une variable binaire
table(ACM2$v158)
ACM2 <- ACM2 %>%
  mutate(v158 = ifelse(v158 %in% c("1", "2"), '1', '0'))

table(ACM2$v157)
ACM2 <- ACM2 %>%
  mutate(v157 = ifelse(v157 %in% c("1", "2"), '1', '0'))

table(ACM2$v159)

ACM2 <- ACM2 %>%
  mutate(v159 = ifelse(v159 %in% c("1", "2"), '1', '0'))

table(ACM2$v171a)
ACM2 <- ACM2 %>%
  mutate(v171a = ifelse(v171a %in% c("1", "2"), '1', '0'))

# Faire l'acm
res.ACM2 <- MCA(ACM2, graph = FALSE)
summary(res.ACM2)
res.ACM2$ind$coord

# Extraire les contributions des individus à la première dimension et arrondir
ind_coord1 <- round(res.ACM2$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseMDK$acces_infos_mere <- ind_coord1

#==== FRécodage de certaines variable====

#  CPN: Nombre de consultation inf àu 3 = 0 ou 98 Don't know= 0
table(BaseMDK$m14); sum(is.na(BaseMDK$m14))

BaseMDK <- BaseMDK %>%
  mutate(m14= ifelse(m14  %in% c(0,1,2,3, 98), 0, 1))

# la distance du centre de centé  1= GRos problème, 2= pas un gros problème
table(BaseMDK$v467d); sum(is.na(BaseMDK$v467d))

# Le lieu d'accouchement, = 0(10: 19 = home , 96= autre ==> home)  Hopital= 1
table(BaseMDK$m15); sum(is.na(BaseMDK$m15))

BaseMDK <- BaseMDK %>%
  mutate(m15= ifelse(m15  %in% c(10:19, 96), 0, 1))

# Niveau d'éducation de la mère 0= No éducation, 1= Primaire, 2= Secondaire, 3= Super
table(BaseMDK$v106); sum(is.na(BaseMDK$v106))

# Situation matrimoniale (Voir le récodage si nécessaire)
#table(BaseRCI$v504)

# Sexe de l'enfant  0= Male, 1= Female
table(BaseMDK$b4); sum(is.na(BaseMDK$b4))

BaseMDK <- BaseMDK %>%
  mutate(b4 = ifelse(b4 == 1, 0, 1))

# Numero d'odre de naissance
table(BaseMDK$bord)

#===Variable de pouvoir de décision des femmes====

# Obtenir l'autorisation de partir, 0= No Problem, 1= Big Problem, 2= Not a big problem
table(BaseMDK$v467b); sum(is.na((BaseMDK$v467b)))

# Dernier mot en famille sur les Soins de santé 
#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     NA= femme non en couple

table(BaseMDK$v743a); sum(is.na((BaseMDK$v743a)))    # 444NA

BaseMDK <- BaseMDK %>%
  mutate(v743a = ifelse(v743a %in% c(4, 5, 6), 0,
                        ifelse(v743a %in% c(2,3), 2, v743a)))

# Personne qui : Effectuer des achats importants à la maison(femme vivants en couple)

#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     453 NA: femme vivants seules
table(BaseMDK$v743b); sum(is.na((BaseMDK$v743b)))  # 453 NA

BaseMDK <- BaseMDK %>%
  mutate(v743b = case_when(
    v743b %in% c(4, 5,6) ~ 0,
    v743b %in% c(2,3) ~ 2,
    TRUE ~ v743b
  ))

#==== ACCES DES FEMMES AU MARCHE DU TRAVAIL====

# Le répondant a travaillé au cours des 12 derniers mois (femmes)

table(BaseMDK$v731); sum(is.na(BaseMDK$v731))  

# Travaille toute l'année, de façon saisonnière ou occassionnelle(Base: Femmes ayants travaillées les 12 derniers mois)
#           1   all year
#           2   seasonal
#           3 occasional
#           NA= 303: 
table(BaseMDK$v732); sum(is.na(BaseMDK$v732))   # 303 NA

# la Répondante travaille actuellement
#               0=No 
#               1=Yes 

table(BaseMDK$v714); sum(is.na(BaseMDK$v714))
   
#=== VACCINATION INFANTILE ====

#verifications des modalitées et des valeurs manquantes

table(BaseMDK$h2); sum(is.na(BaseMDK$h2))     # BCG
table(BaseMDK$h3); sum(is.na(BaseMDK$h3))     # DTC 1
table(BaseMDK$h4); sum(is.na(BaseMDK$h4))     # POLIO 1
table(BaseMDK$h5); sum(is.na(BaseMDK$h5))     # DTC 2
table(BaseMDK$h6); sum(is.na(BaseMDK$h6))     # POLIO 2
table(BaseMDK$h7); sum(is.na(BaseMDK$h7))     # DTC 3
table(BaseMDK$h8); sum(is.na(BaseMDK$h8))     # POLIO 3
table(BaseMDK$h9); sum(is.na(BaseMDK$h9))     # Rougéole 1


#     0 <== NO = 0 & 8     ===> Non vacciné
#     1 <==YES =  1, 2, 3 ====> vacciné

# Utiliser une boucle pour appliquer le même code à chaque variable dans vacc_name
for (vacc in vacc_names) {
  BaseMDK <- BaseMDK %>%
    mutate(!!sym(vacc) := ifelse(!!sym(vacc) %in% c(0, 8), 0, 1))
}

#=== CREATION DE LA COLONNE STATUT VACC ====

BaseMDK <- BaseMDK %>%
  mutate(statut_vacc = ifelse(if_all(all_of(vacc_names), ~ .x == 1), 1, 0))

#====Variables accès a l'infos des ménages ====
varACM_M <- c('hv207', 'hv208','hv243a')
ACM_M1 <- subset(BaseMDK, select =varACM_M )
colSums(is.na(ACM_M1))

# Convertir toutes les colonnes de ACM1 en facteurs
ACM_M1 <- sapply(ACM_M1, factor)

# Convertir le résultat en data.frame
ACM_M1 <- data.frame(ACM_M1)

# Faire l'acm
res.ACM_M2 <- MCA(ACM_M1, graph = FALSE)
summary(res.ACM_M2)
res.ACM_M2$ind$coord

# Extraire les coordonnées des individus à la première dimension et arrondir
ind_coord_M1<- round(res.ACM_M2$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseMDK$acces_infos_menage <- ind_coord_M1

# BASE FINAL
BaseMDK$Id <- paste(BaseMDK[['v000']], BaseMDK[['v001']], BaseMDK[['v002']], sep = "-")

# Exporter la base en CSV
write.csv(BaseMDK, 'BaseMDK.csv', row.names = FALSE)

#===*PAYS:  GABON*====
#*Mères enfants

Gab_MF <- subset(GAB, select = mere_enfant)

# Menage
GabM <- subset(GABM, select = menage)

## Cela évide  le problème de multicolinéarite dans les CPN
Gab_MF_F <- Gab_MF%>%
  filter(Gab_MF$b19 >= 12 & Gab_MF$b19 <= 23 & Gab_MF$b5==1 & Gab_MF$bidx==1)

# Suppression des doublons
Gab_MF_F <- Gab_MF_F %>% distinct(across(all_of(c('v001', 'v002'))), .keep_all = TRUE)

# Rénommer des colonnes  de ménage  pour le merge des deux dataframe
GabM <- GabM %>%
  rename('v001' = 'hv001',
         'v002' = 'hv002')

BaseGAB <- inner_join(Gab_MF_F , GabM, by = c('v001', 'v002'))

#====Variables accès a l'infos de mères ACM femme====
varACM3 <- c('v157', 'v158','v159', 'v171a', 'v169a')
ACM3 <- subset(BaseGAB, select =varACM3 )
colSums(is.na(ACM3))

# Convertir toutes les colonnes de ACM1 en facteurs
ACM3 <- sapply(ACM3, factor)

# Convertir le résultat en data.frame
ACM3 <- data.frame(ACM3)

# Recoder la variable v157 en une variable binaire
table(ACM3$v158)
ACM3 <- ACM3 %>%
  mutate(v158 = ifelse(v158 %in% c("1", "2"), '1', '0'))

table(ACM3$v157)
ACM3 <- ACM3 %>%
  mutate(v157 = ifelse(v157 %in% c("1", "2"), '1', '0'))

table(ACM3$v159)
ACM3 <- ACM3 %>%
  mutate(v159 = ifelse(v159 %in% c("1", "2"), '1', '0'))

table(ACM3$v171a)
ACM3 <- ACM3 %>%
  mutate(v171a = ifelse(v171a %in% c("1", "2"), '1', '0'))

# Faire l'acm
res.ACM3 <- MCA(ACM3, graph = FALSE)
summary(res.ACM3)
res.ACM3$ind$coord

# Extraire les contributions des individus à la première dimension et arrondir
ind_coord2 <- round(res.ACM3$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseGAB$acces_infos_mere <- ind_coord2

#==== FRécodage de certaines variable====

#  CPN: Nombre de consultation inf àu 3 = 0 ou 98 Don't know= 0
table(BaseGAB$m14); sum(is.na(BaseGAB$m14))

BaseGAB <- BaseGAB %>%
  mutate(m14= ifelse(m14  %in% c(0,1,2,3, 98), 0, 1))

# la distance du centre de centé  1= GRos problème, 2= pas un gros problème
table(BaseGAB$v467d); sum(is.na(BaseGAB$v467d))   # 399 NA

# Le lieu d'accouchement, = 0(10: 19 = home , 96= autre ==> home)  Hopital= 1
table(BaseGAB$m15); sum(is.na(BaseGAB$m15))

BaseGAB <- BaseGAB %>%
  mutate(m15= ifelse(m15  %in% c(10:19, 96), 0, 1))

# Niveau d'éducation de la mère 0= No éducation, 1= Primaire, 2= Secondaire, 3= Super
table(BaseGAB$v106); sum(is.na(BaseGAB$v106))

# Sexe de l'enfant  0= Male, 1= Female
table(BaseGAB$b4); sum(is.na(BaseGAB$b4))

BaseGAB <- BaseGAB %>%
  mutate(b4 = ifelse(b4 == 1, 0, 1))

# Numero d'odre de naissance
table(BaseGAB$bord)

#===Variable de pouvoir de décision des femmes====

# Obtenir l'autorisation de partir, 0= No Problem, 1= Big Problem, 2= Not a big problem
table(BaseGAB$v467b); sum(is.na((BaseGAB$v467b)))   # 399 NA

# Dernier mot en famille sur les Soins de santé 
#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)

table(BaseGAB$v743a)
sum(is.na((BaseGAB$v743a)))    # 426 NA

BaseGAB <- BaseGAB %>%
  mutate(v743a = ifelse(v743a %in% c(4, 5, 6), 0,
                        ifelse(v743a %in% c(2,3), 2, v743a)))

# Personne qui : Effectuer des achats importants à la maison

#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
table(BaseGAB$v743b); sum(is.na((BaseGAB$v743b)))  # 426 NA

BaseGAB <- BaseGAB %>%
  mutate(v743b = case_when(
    v743b %in% c(4, 5,6) ~ 0,
    v743b %in% c(2,3) ~ 2,
    TRUE ~ v743b
  ))

#==== ACCES DES FEMMES AU MARCHE DU TRAVAIL====

# Le répondant a travaillé au cours des 12 derniers mois (femmes)

table(BaseGAB$v731); sum(is.na(BaseGAB$v731))  

# Travaille toute l'année, de façon saisonnière ou 
#           1   all year
#           2   seasonal
#           3 occasional 
table(BaseGAB$v732); sum(is.na(BaseGAB$v732))   # 653 NA(Femme qui ne travail pas)

# la Répondante travaille actuellement
#               0=No 
#               1=Yes 

table(BaseGAB$v714); sum(is.na(BaseGAB$v714))

#=== VACCINATION INFANTILE ====

#     0 <== NO = 0 & 8     ===> Non vacciné
#     1 <==YES =  1, 2, 3 ====> vacciné

#verifications des modalitées et des valeurs manquantes

table(BaseGAB$h2); sum(is.na(BaseGAB$h2))     # BCG
table(BaseGAB$h3); sum(is.na(BaseGAB$h3))     # DTC 1
table(BaseGAB$h4); sum(is.na(BaseGAB$h4))     # POLIO 1
table(BaseGAB$h5); sum(is.na(BaseGAB$h5))     # DTC 2
table(BaseGAB$h6); sum(is.na(BaseGAB$h6))     # POLIO 2
table(BaseGAB$h7); sum(is.na(BaseGAB$h7))     # DTC 3
table(BaseGAB$h8); sum(is.na(BaseGAB$h8))     # POLIO 3
table(BaseGAB$h9); sum(is.na(BaseGAB$h9))     # Rougéole 1

# Définir les noms des variables à modifier
vacc_names <- c('h2','h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9')

# Utiliser une boucle pour appliquer le même code à chaque variable dans vacc_name
for (vacc in vacc_names) {
  BaseGAB <- BaseGAB %>%
    mutate(!!sym(vacc) := ifelse(!!sym(vacc) %in% c(0, 8), 0, 1))
}

#=== CREATION DE LA COLONNE STATUT VACC ====

BaseGAB <- BaseGAB %>%
  mutate(statut_vacc = ifelse(if_all(all_of(vacc_names), ~ .x == 1), 1, 0))

#====Variables accès a l'infos des ménages ====
varACM_M <- c('hv207', 'hv208','hv243a')
ACM_M2 <- subset(BaseGAB, select =varACM_M )
colSums(is.na(ACM_M2))

# Convertir toutes les colonnes de ACM1 en facteurs
ACM_M2 <- sapply(ACM_M2, factor)

# Convertir le résultat en data.frame
ACM_M2 <- data.frame(ACM_M2)

# Faire l'acm
res.ACM_M3 <- MCA(ACM_M2, graph = FALSE)
summary(res.ACM_M3)
res.ACM_M3$ind$coord

# Extraire les coordonnées des individus à la première dimension et arrondir
ind_coord_M2<- round(res.ACM_M3$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseGAB$acces_infos_menage <- ind_coord_M2

# BAse Finale
BaseGAB$Id <- paste(BaseGAB[['v000']], BaseGAB[['v001']], BaseGAB[['v002']], sep = "-")

# Exporter la base en CSV
write.csv(BaseGAB, 'BaseGAB.csv', row.names = FALSE)

#===*PAYS :  TANZANIE*====
#Mères enfants
Tan_MF <- subset(TANZ, select = mere_enfant)

# Ménage
Tan_M <- subset(TANZM, select = menage)

## Cela évite  le problème de multicolinéarite dans les CPN
Tan_MF_F <- Tan_MF%>%
  filter(Tan_MF$b19 >= 12 & Tan_MF$b19 <= 23 & Tan_MF$b5==1 & Tan_MF$bidx==1)

# Suppression des doublons
Tan_MF_F <- Tan_MF_F %>% distinct(across(all_of(c('v001', 'v002'))), .keep_all = TRUE)

# Rénommer des colonnes  de ménage  pour le merge des deux dataframe
Tan_M <- Tan_M %>%
  rename('v001' = 'hv001',
         'v002' = 'hv002')

BaseTANZ<- inner_join(Tan_MF_F , Tan_M, by = c('v001', 'v002'))

#====Variables accès a l'infos de mères ACM femme====
varACM4 <- c('v157', 'v158','v159', 'v171a', 'v169a')
ACM4 <- subset(BaseTANZ, select =varACM4 )
colSums(is.na(ACM4))

# Convertir toutes les colonnes de ACM1 en facteurs
ACM4 <- sapply(ACM4, factor)

# Convertir le résultat en data.frame
ACM4 <- data.frame(ACM4)

# Recoder la variable v157 en une variable binaire
table(ACM4$v158)
ACM4 <- ACM4 %>%
  mutate(v158 = ifelse(v158 %in% c("1", "2"), '1', '0'))

table(ACM4$v157)
ACM4 <- ACM4 %>%
  mutate(v157 = ifelse(v157 %in% c("1", "2"), '1', '0'))

table(ACM4$v159)
ACM4 <- ACM4 %>%
  mutate(v159 = ifelse(v159 %in% c("1", "2"), '1', '0'))

table(ACM4$v171a)
ACM4 <- ACM4 %>%
  mutate(v171a = ifelse(v171a %in% c("1", "2"), '1', '0'))

# Faire l'acm
res.ACM4 <- MCA(ACM4, graph = FALSE)
summary(res.ACM4)
res.ACM4$ind$coord

# Extraire les coordonnées des individus sur le prémier axe  et arrondir
ind_coord3 <- round(res.ACM4$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseTANZ$acces_infos_mere <- ind_coord3

#==== FRécodage de certaines variable====

#  CPN: Nombre de consultation inf àu 3 = 0 ou 98 Don't know= 0
table(BaseTANZ$m14); sum(is.na(BaseTANZ$m14))    # PAS DE NA

BaseTANZ <- BaseTANZ %>%
  mutate(m14= ifelse(m14  %in% c(0,1,2,3, 98), 0, 1))

# la distance du centre de centé  1= GRos problème, 2= pas un gros problème
table(BaseTANZ$v467d); sum(is.na(BaseTANZ$v467d))   # PAS DE NA

# Le lieu d'accouchement, = 0(10: 19 = home , 96= autre ==> home)  Hopital= 1
table(BaseTANZ$m15); sum(is.na(BaseTANZ$m15))

BaseTANZ <- BaseTANZ %>%
  mutate(m15= ifelse(m15  %in% c(10:19, 96), 0, 1))

# Niveau d'éducation de la mère 0= No éducation, 1= Primaire, 2= Secondaire, 3= Super
table(BaseTANZ$v106); sum(is.na(BaseTANZ$v106))

# Sexe de l'enfant  0= Male, 1= Female
table(BaseTANZ$b4); sum(is.na(BaseTANZ$b4))

BaseTANZ <- BaseTANZ %>%
  mutate(b4 = ifelse(b4 == 1, 0, 1))

# Numero d'odre de naissance
table(BaseTANZ$bord)

#===Variable de pouvoir de décision des femmes====

# Obtenir l'autorisation de partir, 0= No Problem, 1= Big Problem, 2= Not a big problem
table(BaseTANZ$v467b); sum(is.na((BaseTANZ$v467b)))   # PAS DE NA

# Dernier mot en famille sur les Soins de santé 
#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     NA= femmes vivant seules

table(BaseTANZ$v743a); sum(is.na((BaseTANZ$v743a)))    # 352 NA

BaseTANZ <- BaseTANZ %>%
  mutate(v743a = ifelse(v743a %in% c(4, 5, 6), 0,
                        ifelse(v743a %in% c(2,3), 2, v743a)))

# Personne qui : Effectuer des achats importants à la maison

#     0= Autre personne qui décide (mari compris)
#     1= la femme seule qui décide 
#     2= La femme et une autre personne (mari inclus)
#     NA= femmes vivant seules
table(BaseTANZ$v743b); sum(is.na((BaseTANZ$v743b)))  # 352 NA

BaseTANZ <- BaseTANZ %>%
  mutate(v743b = case_when(
    v743b %in% c(4, 5,6) ~ 0,
    v743b %in% c(2,3) ~ 2,
    TRUE ~ v743b
  ))

#==== ACCES DES FEMMES AU MARCHE DU TRAVAIL====

# Le répondant a travaillé au cours des 12 derniers mois (femmes)

table(BaseTANZ$v731); sum(is.na(BaseTANZ$v731))  

# Travaille toute l'année, de façon saisonnière ou 
#           1   all year
#           2   seasonal
#           3 occasional 
#           NA= Femmes qui ne travaillent pas 
table(BaseTANZ$v732); sum(is.na(BaseTANZ$v732))   # 692 NA

# la Répondante travaille actuellement
#               0=No 
#               1=Yes 

table(BaseTANZ$v714); sum(is.na(BaseTANZ$v714))

#=== VACCINATION INFANTILE ====

#     0 <== NO = 0 & 8     ===> Non vacciné
#     1 <==YES =  1, 2, 3 ====> vacciné

#verifications des modalitées et des valeurs manquantes

table(BaseTANZ$h2); sum(is.na(BaseTANZ$h2))     # BCG
table(BaseTANZ$h3); sum(is.na(BaseTANZ$h3))     # DTC 1
table(BaseTANZ$h4); sum(is.na(BaseTANZ$h4))     # POLIO 1
table(BaseTANZ$h5); sum(is.na(BaseTANZ$h5))     # DTC 2
table(BaseTANZ$h6); sum(is.na(BaseTANZ$h6))     # POLIO 2
table(BaseTANZ$h7); sum(is.na(BaseTANZ$h7))     # DTC 3
table(BaseTANZ$h8); sum(is.na(BaseTANZ$h8))     # POLIO 3
table(BaseTANZ$h9); sum(is.na(BaseTANZ$h9))     # Rougéole 1

# Définir les noms des variables à modifier
vacc_names <- c('h2','h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9')

# Utiliser une boucle pour appliquer le même code à chaque variable dans vacc_name
for (vacc in vacc_names) {
  BaseTANZ <- BaseTANZ %>%
    mutate(!!sym(vacc) := ifelse(!!sym(vacc) %in% c(0, 8), 0, 1))
}

#=== CREATION DE LA COLONNE STATUT VACC ====

BaseTANZ <- BaseTANZ %>%
  mutate(statut_vacc = ifelse(if_all(all_of(vacc_names), ~ .x == 1), 1, 0))

#====Variables accès a l'infos des ménages ====
varACM_M <- c('hv207', 'hv208','hv243a')
ACM_M3 <- subset(BaseTANZ, select =varACM_M )
colSums(is.na(ACM_M3))

# Convertir toutes les colonnes de ACM1 en facteurs
ACM_M3 <- sapply(ACM_M3, factor)

# Convertir le résultat en data.frame
ACM_M3 <- data.frame(ACM_M3)

# Faire l'acm
res.ACM_M4 <- MCA(ACM_M3, graph = FALSE)
summary(res.ACM_M4)
res.ACM_M4$ind$coord

# Extraire les coordonnées des individus à la première dimension et arrondir
ind_coord_M3<- round(res.ACM_M4$ind$coord[,1],4)

# Ajouter cette variable a la base
BaseTANZ$acces_infos_menage <- ind_coord_M3

# BAse Finale
BaseTANZ$Id <- paste(BaseTANZ[['v000']], BaseTANZ[['v001']], BaseTANZ[['v002']], sep = "-")

# Exporter la base en CSV
write.csv(BaseTANZ, 'BaseTANZ.csv', row.names = FALSE)

#==== BASE FINALE POUR L'ETUDE =====

BaseFinal <- bind_rows(BaseRCI, BaseMDK, BaseGAB, BaseTANZ)
colnames(BaseFinal)

BaseFinal <- BaseFinal %>%
  rename('CPN' = 'm14',
         'Lieu_accou' = 'm15',
         'Autorisation'='v467b',
         'Dist_Etablss'= 'v467d',
         'worked_12month_last'='v731',
         'forme_travail'= 'v732',
         'statut_travail'='v714',
         'Dernier_mot'= 'v743a',
         'Decision_achat'='v743b',
         'Educ_mere'= 'v106',
         'sexe_enft'='b4',
         'BCG'= 'h2',
         'DTC1'='h3',
         'VPO1'='h4',
         'DTC2'='h5',
         'VPO2'='h6',
         'DTC3'='h7',
         'VPO3'='h8',
         'RR'='h9',
         'Lieu_residence'='hv025',
         'Indice_richesse'='hv270a',
         'Sexe_menage'='hv219',
         'age_mere'='v012'
         )


# Variables accès infos ménages plus corréles

for(col in colnames(BaseFinal)){
  if (sum(is.na(BaseFinal[[col]])) > 0) {
    cat(sprintf("la colonne %s contient %2.f%% de valeur manquante \n", col, (sum(is.na(BaseFinal[[col]])) / length(BaseFinal[[col]]) * 100)))
  } else {
    cat(sprintf("la colonne %s ne contient pas de valeur manquantes\n", col))
  }
}


#=== Base FINALE Suppression des colonnes spécifiées====

# SUPPRESSION DE CERTAINS FILTRES

BaseFinal <- subset(BaseFinal, select = -c(b0, b5, bidx, v001, v002))

# Exporter le dataframe en fichier CSV
write.csv(BaseFinal, "DataFinal.csv", row.names = FALSE)

#######################################################################################

#===== ANALYSES STATAISTIQUES====

#=== Importer la base Finale====
data <- read.csv('DataFinal.csv')
data <- subset(data, select = -c(forme_travail, Dernier_mot, Decision_achat))
# Renommer les valeurs dans la colonne "Pays"

data <- data %>%
  mutate(v000 = case_when(
    v000 == "CI8" ~ "COTE D'IVOIRE",
    v000 == "GA7" ~ "GABON",
    v000 == "MD7" ~ "MADAGASCAR",
    v000 == "TZ8" ~ "TANZANIE",
    TRUE ~ v000  
  ))

# Femme = 2, Homme = 1: Recode Femme (2= 0) et Homme(1=1)
data <- data %>%
  mutate(Sexe_menage= ifelse(Sexe_menage==2, 0, 1))


# Création de colonne DTC_all(Enfant Zero dose)
data <- data %>%
  mutate(DTC_all = ifelse(DTC1 == 1 & DTC2 == 1 & DTC3 == 1, 1, 0))
data <- data %>%
  mutate(Zero_DTC = ifelse(DTC1 == 0 & DTC2 == 0 & DTC3 == 0, 0, 1))
data <- data %>%
  mutate(All_VPO = ifelse(VPO1 == 1 & VPO2 == 1 & VPO3 == 1, 1, 0))

data <- data %>%
  mutate(Lieu_residence = ifelse(Lieu_residence == 2, 0, 1)) # 2= rural

# Verifier le NA (Non applicable)
for(col in colnames(data)){
  if (sum(is.na(data[[col]])) > 0) {
    cat(sprintf("la colonne %s contient %2.f%% de valeur manquante \n", col, (sum(is.na(data[[col]])) / length(data[[col]]) * 100)))
  } else {
    cat(sprintf("la colonne %s ne contient pas de valeur manquantes\n", col))
  }
}

# Suppression des NA:(Personne non malade: pas de besion de santé exprimé)  pour Dist_sante
data <- na.omit(data)
 
attach(data)
#=== ANALYSE UNIVARIEE ====

#==== PREVALENCE DE LA VACCINATION PAR PAYS====
# Chargement des packages nécessaires
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(openxlsx)

#===== Effectif par pays=====
Eff <- data %>% 
  group_by(v000) %>%
  summarise(effectifs= n())


# Définition de la fonction pour calculer la prévalence et les intervalles de confiance
calc_prevalence_ci <- function(x) {
  result <- prop.test(sum(x == 1), length(x))
  prevalence <- result$estimate
  conf_int <- result$conf.int
  # Formatage de l'intervalle de confiance en une seule colonne
  ci_string <- paste0(format(round(conf_int[1], 2), nsmall = 2), " - ", format(round(conf_int[2], 2), nsmall = 2))
  return(data.frame(Prevalence = prevalence, CI = ci_string))
}

# Calcul des prévalences et des intervalles de confiance pour chaque vaccin
prevalences <- data.frame(
  VACCINS = c("VACCINATION COMPLETE", "DTC1", "DTC2", "DTC3", "BCG", "VPO1", "VPO2", "VPO3", "ROUGEOLE (RR)", "DTC COMPLET"),
  rbind(
    calc_prevalence_ci(data$statut_vacc),
    calc_prevalence_ci(data$DTC1),
    calc_prevalence_ci(data$DTC2),
    calc_prevalence_ci(data$DTC3),
    calc_prevalence_ci(data$BCG),
    calc_prevalence_ci(data$VPO1),
    calc_prevalence_ci(data$VPO2),
    calc_prevalence_ci(data$VPO3),
    calc_prevalence_ci(data$RR),
    calc_prevalence_ci(data$DTC_all)
  )
)

# Formater le tableau avec kableExtra
prevalences %>%
  mutate(Prevalence = scales::percent(Prevalence, accuracy = 0.1),
         CI = CI) %>%
  kbl(caption = "Prévalence globale des différents vaccins avec intervalles de confiance") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Prévalence Globale" = 1, "Intervalle de Confiance" = 1), header_bold = TRUE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, width = "15em")  # Ajuster la largeur de la colonne CI si nécessaire


#=====================ANALYSE BIVARIEE================

# Calcul de la prévalence par pays des vaccins 
# Je remplace les variables sans dupliquer le code plusieur fois

result <- data %>%
  group_by(v000, Autorisation) %>%
  summarise(pourcentage_vaccines = mean(statut_vacc, na.rm = TRUE) * 100) %>%
  ungroup()

# Calcul des moyennes globales pour CPN = 0 et CPN = 1
global_means <- data %>%
  group_by(Autorisation) %>%
  summarise(pourcentage_vaccines = mean(statut_vacc, na.rm = TRUE) * 100) %>%
  mutate(v000 = "Global")

# Combinaison des résultats par pays et des moyennes globales
result <- bind_rows(result, global_means) %>%
  arrange(Autorisation, v000)

print(result)

#=====TEST D'ASSOCIATION  AVEC LA VARIABLE D'INTERET: Statut vaccinal =====

# Calcul des tableaux croisés et des tests de chi-carré
table_cpn <- table(data$CPN, data$statut_vacc)
prop_cpn <- prop.table(table_cpn, margin = 1)
test_cpn <- chisq.test(table_cpn)

table_education <- table(data$Educ_mere, data$statut_vacc)
prop_education <- prop.table(table_education, margin = 1)
test_education <- chisq.test(table_education)

table_wealth <- table(data$Indice_richesse, data$statut_vacc)
prop_wealth <- prop.table(table_wealth, margin = 1)
test_wealth <- chisq.test(table_wealth)

table_accou <- table(data$Lieu_accou, data$statut_vacc)
prop_accou <- prop.table(table_accou, margin = 1)
test_accou <- chisq.test(table_accou)

table_residence <- table(data$Lieu_residence, data$statut_vacc)
prop_residence <- prop.table(table_residence, margin = 1)
test_residence <- chisq.test(table_residence)

table_distance <- table(data$Dist_Etablss, data$statut_vacc)
prop_distance <- prop.table(table_distance, margin = 1)
test_distance <- chisq.test(table_distance)

table_autorisation <- table(data$Autorisation, data$statut_vacc)
prop_autorisation <- prop.table(table_autorisation, margin = 1)
test_autorisation <- chisq.test(table_autorisation)

table_travail <- table(data$statut_travail, data$statut_vacc)
prop_travail <- prop.table(table_travail, margin = 1)
test_travail <- chisq.test(table_travail)

table_sexe_chef_menage <- table(data$Sexe_menage, data$statut_vacc)
prop_sexe_chef_menage <- prop.table(table_sexe_chef_menage, margin = 1)
test_sexe_chef_menage <- chisq.test(table_sexe_chef_menage)

table_sexe_enfant <- table(data$sexe_enft, data$statut_vacc)
prop_sexe_enfant <- prop.table(table_sexe_enfant, margin = 1)
test_sexe_enfant <- chisq.test(table_sexe_enfant)

# Créer un dataframe pour résumer les résultats
summary_table <- data.frame(
  Variable = c("Nombre de CPN", "Niveau d'éducation de la mère", "Quintile de richesse", "Lieu d'accouchement", 
               "Lieu de résidence", "Distance à l'établissement", "Autorisation", "Statut de travail", 
               "Sexe du chef de ménage", "Sexe de l'enfant"),
  Chi_square_statistic = c(test_cpn$statistic, test_education$statistic, test_wealth$statistic, test_accou$statistic, 
                           test_residence$statistic, test_distance$statistic, test_autorisation$statistic, 
                           test_travail$statistic, test_sexe_chef_menage$statistic, test_sexe_enfant$statistic),
  DDL = c(test_cpn$parameter, test_education$parameter, test_wealth$parameter, test_accou$parameter, 
          test_residence$parameter, test_distance$parameter, test_autorisation$parameter, 
          test_travail$parameter, test_sexe_chef_menage$parameter, test_sexe_enfant$parameter),
  P_value = c(test_cpn$p.value, test_education$p.value, test_wealth$p.value, test_accou$p.value, 
              test_residence$p.value, test_distance$p.value, test_autorisation$p.value, 
              test_travail$p.value, test_sexe_chef_menage$p.value, test_sexe_enfant$p.value),
  Significant = c(test_cpn$p.value < 0.05, test_education$p.value < 0.05, test_wealth$p.value < 0.05, 
                  test_accou$p.value < 0.05, test_residence$p.value < 0.05, test_distance$p.value < 0.05, 
                  test_autorisation$p.value < 0.05, test_travail$p.value < 0.05, test_sexe_chef_menage$p.value < 0.05, 
                  test_sexe_enfant$p.value < 0.05)
)

# Convertir les valeurs booléennes en étoiles
summary_table$Significant <- ifelse(summary_table$Significant, "*", "")

# Créer un tableau esthétique avec kableExtra
summary_table %>%
  kbl(caption = "Résultats des tests de chi-carré pour les différentes variables et le statut vaccinal") %>%
  kable_styling(full_width = F) %>%
  column_spec(5, bold = TRUE, color = "white", background = ifelse(summary_table$Significant == "*", "green", "red"))

#========== explicatives quantitatives========

# Calcul des tests de corrélation
cor_acces_infos_menage <- cor.test(data$statut_vacc, data$acces_infos_menage)
cor_acces_infos_mere <- cor.test(data$statut_vacc, data$acces_infos_mere)


# BOXPLOT: EXPLICATIVES QUANTITATIVE & VARIABLE D'INTERET

# Accès à l'information et statut vaccinal de l'enfant
ggplot(data, aes(x = factor(statut_vacc), y = acces_infos_mere, fill = factor(statut_vacc))) +
  geom_boxplot() +
  labs(
    title = "Relation entre statut vaccinal de l'enfant et l'accès des mères à l'information",
    x = "Statut Vaccinal de l'enfant",
    y = "Accèssibilté des mères à l'information "
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5)
  ) +
  scale_x_discrete(labels = c("Vaccination incomplète", "Vaccination complète")) +
  guides(fill = FALSE)


# Age maternel et statut vaccinal de l'enfant
ggplot(data, aes(x = factor(statut_vacc), y = age_mere, fill = factor(statut_vacc))) +
  geom_boxplot() +
  labs(
    title = "Relation entre statut vaccinal de l'enfant et l'âge de la mère",
    x = "Statut Vaccinal de l'enfant",
    y = "Age maternel"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5)
  ) +
  scale_x_discrete(labels = c("Vaccination incomplète", "Vaccination complète")) +
  guides(fill = FALSE)

# Rang de naissance et statut vaccinal de l'enfant
ggplot(data, aes(x = factor(statut_vacc), y = bord, fill = factor(statut_vacc))) +
  geom_boxplot() +
  labs(
    title = "Relation entre vaccination complète et ordre de naissance de l'enfant",
    x = "Statut Vaccinal de l'enfant",
    y = "Rang de naissance de l'enfant"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5)
  ) +
  scale_x_discrete(labels = c("Vaccination incomplète", "Vaccination complète")) +
  guides(fill = FALSE)


#########################################################################################


# Charger les données géographiques de l'Afrique (pays)
world <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Exemple de données de pourcentage de vaccination par pays
dd <- data.frame(
  PAYS = c("Côte d'Ivoire", 'Tanzania', 'Gabon', 'Madagascar'),
  Pourcentage_Vaccination = c(38.5, 55.1, 39.3, 49.1)
)

# Fusionner les données géographiques avec les données de résultats
merged <- merge(world, dd, by.x = "name", by.y = "PAYS", all.x = TRUE)

# Créer la carte choroplèthe avec ggplot2
ggplot() +
  geom_sf(data = merged, aes(fill = Pourcentage_Vaccination), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  
  theme_minimal() +
  theme(legend.position = "right")  # Déplacer la légende à droite




#===================== ANALYSE MULTIVARIEES ===================
# Conversion des variables catégorielles en facteurs

data <- data %>%
  mutate(
    statut_vacc = as.numeric(statut_vacc),  # Statut vaccinal binaire (1 = vacciné, 0 = non vacciné)
    Educ_mere = as.factor(Educ_mere),
    Indice_richesse = as.factor(Indice_richesse),
    Lieu_accou = as.factor(Lieu_accou),
    sexe_enft = as.factor(sexe_enft),
    Lieu_residence = as.factor(Lieu_residence),
    CPN = as.factor(CPN),
    Sexe_menage = as.factor(Sexe_menage),
    statut_travail = as.factor(statut_travail),
    Autorisation = as.factor(Autorisation),
    Dist_Etablss = as.factor(Dist_Etablss),
    v000 = as.factor(v000)
  )

# REGRESSION LOGISTIQUE

# NB: Multicolinéarité entre accès à l'information  et Indice de richesse(contruit avec des variables de accès à l'infos)
library(broom)
library(sjPlot)
library(knitr)

# Convertir v000 en facteur et spécifier l'ordre des niveaux
data$v000 <- factor(data$v000, levels = c("COTE D'IVOIRE", "MADAGASCAR","GABON", "TANZANIE"))

# Vérifier les niveaux de v000
levels(data$v000)

# Model 1: Variables sociodemographiques, geographiques 

# Ajuster le modèle de régression logistique
model1 <- glm(statut_vacc ~ v000 + Lieu_residence + Indice_richesse + Educ_mere 
              + age_mere +  bord , 
              family = binomial(link = "logit"), data = data)

summary(model1)
# Résumer les résultats avec broom::tidy
results <- tidy(model1, conf.int = TRUE, exponentiate = TRUE)

# Supprimer les lignes correspondant aux variables à exclure (comme l'intercept et Sexe_menage)
results <- results %>% filter(!term %in% c("(Intercept)", "Sexe_menage"))

# Sélectionner les colonnes pertinentes et renommer les colonnes
results <- results %>%
  select(term, estimate = estimate, conf.low, conf.high, p.value) %>%
  mutate(
    OR = paste0(round(estimate, 3), " (", round(conf.low, 3), "-", round(conf.high, 3), ")")
  ) %>%
  select(term, OR, p.value)

# Afficher le tableau des résultats
kable(results, caption = "Tableau des Odds Ratios (OR) et p-values du modèle de régression logistique", digits = 3)

# Modèle vide (intercept uniquement)
model_vide <- glm(statut_vacc ~ 1, family = binomial(link = "logit"), data = data)

# Comparaison des modèles avec le test de rapport de vraisemblance
lr_test <- lrtest(model_vide, model1)

# Affichage des résultats
print("Test de rapport de vraisemblance :")
print(lr_test)



# Model 2/ FACTEURS SOCIO SANITAIRES: Accès au soins , à l'information et autonomisations des femmes

model2 <- glm(statut_vacc ~ CPN + Lieu_accou + statut_travail + Autorisation + 
                acces_infos_mere + Dist_Etablss, family = binomial(link = "logit"), data = data)

summary(model2)

results1 <- tidy(model2, conf.int = TRUE, exponentiate = TRUE)

# Supprimer les lignes correspondant aux variables à exclure (comme l'intercept et Sexe_menage)
results1 <- results1 %>% filter(!term %in% c("(Intercept)", "Sexe_menage"))

# Sélectionner les colonnes pertinentes et renommer les colonnes
results1 <- results1 %>%
  select(term, estimate = estimate, conf.low, conf.high, p.value) %>%
  mutate(
    OR = paste0(round(estimate, 3), " (", round(conf.low, 3), "-", round(conf.high, 3), ")")
  ) %>%
  select(term, OR, p.value)

# Afficher le tableau des résultats
kable(results1, caption = "Tableau des Odds Ratios (OR) et p-values du modèle de régression logistique", digits = 3)

# Comparaison des modèles avec le test de rapport de vraisemblance
lr_test <- lrtest(model_vide, model2)

# Affichage des résultats
print("Test de rapport de vraisemblance :")
print(lr_test)

# Modele mixte
library(lme4)
library(broom.mixed)

model3 <- glmer(statut_vacc ~   CPN + Educ_mere + Lieu_residence +  Lieu_accou + acces_infos_mere*Lieu_residence+ 
                + age_mere + bord   + Autorisation +  (1 | v000) ,
                family = binomial(link = "logit"), data = data)
summary(model3)


# Utiliser broom.mixed::tidy pour obtenir les résultats
results2 <- broom.mixed::tidy(model3, conf.int = TRUE, exponentiate = TRUE)

# Supprimer les lignes correspondant aux variables à exclure (comme l'intercept)
results2 <- results2 %>% filter(!term %in% c("(Intercept)", "Sexe_menage"))

# Sélectionner les colonnes pertinentes et renommer les colonnes
results2 <- results2 %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    OR = paste0(round(estimate, 3), " (", round(conf.low, 3), "-", round(conf.high, 3), ")")
  ) %>%
  select(term, OR, p.value)

# Afficher le tableau des résultats

kable(results2, caption = "Tableau des Odds Ratios (OR) et p-values du modèle de régression logistique", digits = 3)

# Comparaison des modèles avec le test de rapport de vraisemblance
lr_test <- lrtest(model_vide, model3)

# Affichage des résultats
print("Test de rapport de vraisemblance :")
print(lr_test)

#==== ANALYSE DE DECOMPSITION====
# Installation du package nécessaire
#install.packages("oaxaca")

# Chargement des packages

library(oaxaca)
