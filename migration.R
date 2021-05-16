#Chargement les librairies
library(openxlsx)
library(corrplot)
library(tidyr)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(tidyverse)

#import les données
data1 <- read.xlsx(xlsxFile = "Data/migration1.xlsx", sheet = 1)
data2 <- read.xlsx(xlsxFile = "Data/migration2.xlsx", sheet = 1)

#choisie les variables
myvars1<- c("id_03","id_04","ic_051","id_061","S10_11","TYPE_EDU","m104_01",
            "hhsize","m110_01","m110_02","m110_03","m110_04","m110_05",
            "m110_06","m110_07","m110_08","m110_09","m110_10","m204","m205",
            "TRANS_PC","m207_a","m207_b","tot_migr","m301","PERS_CHA",
            "nb_equip","ubt_tot","nb_act","m634_12","m635_12","dep_tot",
            "dtot_pc","pedu_dto","nb_chocs","type_str","V103_004","V107_013",
            "V107_014","V107_015","V301_001","V302_002","PROF_ELE","CLAS_ELE",
            "revenue","agriculture","livestock","fishing","garden","pettytr",
            "trading","wagelab","handtrsp","salaries","remittance","aidgift",
            "otheract")

df1<-data1[myvars1]

myvars2<- c("id_03","id_04","id_05","id_06","S10_11","TYPE_EDU","m104_01",
             "hhsize","m110_01","m110_02","m110_03","m110_04","m110_05",
             "m110_06","m110_07","m110_08","m110_09","m110_10","m204","m205",
             "TRANS_PC","m207_a","m207_b","tot_migr","m301","PERS_CHA",
             "nb_equip","ubt_tot","nb_act","m634_12","m635_12","dep_tot",
             "dtot_pc","pedu_dto","nb_chocs","type_str","V103_004","V107_013",
             "V107_014","V107_015","V301_001","V302_002","PROF_ELE","CLAS_ELE",
             "revenue","agriculture","livestock","fishing","garden","pettytr",
             "trading","wagelab","handtrsp","salaries","remittance","aidgift",
             "otheract")

df2<-data2[myvars2]

#renommé les variable de donnée 1
names(df1)[1] <-"Code région"
names(df1)[2]<-"Cercle"
names(df1)[3]<-"Code commune"
names(df1)[4]<-"Code village"
names(df1)[5]<-"Nombre enfants"
names(df1)[6]<-"Éducation mère"
names(df1)[7]<-"Education chef du ménage"
names(df1)[8]<-"Taille ménage"
names(df1)[9]<-"Garçon à école"
names(df1)[10]<-"Fille à école"
names(df1)[11]<-"Garçon jamais à école"
names(df1)[12]<-"Raison garçon jamais à école"
names(df1)[13]<-"Fille jamais à école"
names(df1)[14]<-"Raison fille jamais à école"
names(df1)[15]<-"Garçon arrète école"
names(df1)[16]<-"Raison garçon arrète école"
names(df1)[17]<-"Fille arrète école"
names(df1)[18]<-"Raison fille arrète école"
names(df1)[19]<-"Migrant est chef du ménage"
names(df1)[20]<-"Transferts de migrant 12 derniers mois"
names(df1)[21]<-"Trasnferts de migrant par tête"
names(df1)[22]<-"Membres masculin migré"
names(df1)[23]<-"Membres feminin migré"
names(df1)[24]<-"membres total en migration"
names(df1)[25]<-"Proprietaire de logement"
names(df1)[26]<-"Nombre de personne par pièce"
names(df1)[27]<-"Nombre équipement possédés par ménage"
names(df1)[28]<-"Nombre total de tête en bétail"
names(df1)[29]<-"Nombre source de revenu"
names(df1)[30]<-"Dépense mensuelle pour santé"
names(df1)[31]<-"Dépense mensuelle pour éducation"
names(df1)[32]<-"Dépense total"
names(df1)[33]<-"Dépense total par tête"
names(df1)[34]<-"Part de dépense total pour éducation"
names(df1)[35]<-"Nombre de choc 12 dernier mois"
names(df1)[36]<-"Type de stratégie"
names(df1)[37]<-"Type de migration"
names(df1)[38]<-"Destination de migration 1"
names(df1)[39]<-"Destination de migration 2"
names(df1)[40]<-"Destination de migration 3"
names(df1)[41]<-"Centre alphabétisation dans village"
names(df1)[42]<-"École primaire dans village"
names(df1)[43]<-"Taux élève-professeur"
names(df1)[44]<-"Taux salle de classe-élève"
names(df1)[45]<-"Revenue total mensuel"
names(df1)[46]<-"Revenue mensuel agriculture"
names(df1)[47]<-"Revenue mensuel bétail"
names(df1)[48]<-"Revenue mensuel pêcherie"
names(df1)[49]<-"Revenue mensuel maraîchage"
names(df1)[50]<-"Revenue mensuel petit commerce"
names(df1)[51]<-"Revenue mensuel commerce"
names(df1)[52]<-"Revenue mensuel salaire de travail"
names(df1)[53]<-"Revenue mensuel artisanat et transport"
names(df1)[54]<-"Revenue mensuel salaire inclu pension"
names(df1)[55]<-"Revenue mensuel envois de fonds"
names(df1)[56]<-"Revenue mensuel aides et cadeau"
names(df1)[57]<-"Revenue mensuel autres activités"

#renommé les variables de donnée 2
names(df2)[1] <- "Code région"
names(df2)[2]<-"Cercle"
names(df2)[3]<-"Code commune"
names(df2)[4]<-"Code village"
names(df2)[5]<-"Nombre enfants"
names(df2)[6]<-"Éducation mère"
names(df2)[7]<-"Education chef du ménage"
names(df2)[8]<-"Taille ménage"
names(df2)[9]<-"Garçon à école"
names(df2)[10]<-"Fille à école"
names(df2)[11]<-"Garçon jamais à école"
names(df2)[12]<-"Raison garçon jamais à école"
names(df2)[13]<-"Fille jamais à école"
names(df2)[14]<-"Raison fille jamais à école"
names(df2)[15]<-"Garçon arrète école"
names(df2)[16]<-"Raison garçon arrète école"
names(df2)[17]<-"Fille arrète école"
names(df2)[18]<-"Raison fille arrète école"
names(df2)[19]<-"Migrant est chef du ménage"
names(df2)[20]<-"Transferts de migrant 12 derniers mois"
names(df2)[21]<-"Trasnferts de migrant par tête"
names(df2)[22]<-"Membres masculin migré"
names(df2)[23]<-"Membres feminin migré"
names(df2)[24]<-"membres total en migration"
names(df2)[25]<-"Proprietaire de logement"
names(df2)[26]<-"Nombre de personne par pièce"
names(df2)[27]<-"Nombre équipement possédés par ménage"
names(df2)[28]<-"Nombre total de tête en bétail"
names(df2)[29]<-"Nombre source de revenu"
names(df2)[30]<-"Dépense mensuelle pour santé"
names(df2)[31]<-"Dépense mensuelle pour éducation"
names(df2)[32]<-"Dépense total"
names(df2)[33]<-"Dépense total par tête"
names(df2)[34]<-"Part de dépense total pour éducation"
names(df2)[35]<-"Nombre de choc 12 dernier mois"
names(df2)[36]<-"Type de stratégie"
names(df2)[37]<-"Type de migration"
names(df2)[38]<-"Destination de migration 1"
names(df2)[39]<-"Destination de migration 2"
names(df2)[40]<-"Destination de migration 3"
names(df2)[41]<-"Centre alphabétisation dans village"
names(df2)[42]<-"École primaire dans village"
names(df2)[43]<-"Taux élève-professeur"
names(df2)[44]<-"Taux salle de classe-élève"
names(df2)[45]<-"Revenue total mensuel"
names(df2)[46]<-"Revenue mensuel agriculture"
names(df2)[47]<-"Revenue mensuel bétail"
names(df2)[48]<-"Revenue mensuel pêcherie"
names(df2)[49]<-"Revenue mensuel maraîchage"
names(df2)[50]<-"Revenue mensuel petit commerce"
names(df2)[51]<-"Revenue mensuel commerce"
names(df2)[52]<-"Revenue mensuel salaire de travail"
names(df2)[53]<-"Revenue mensuel artisanat et transport"
names(df2)[54]<-"Revenue mensuel salaire inclu pension"
names(df2)[55]<-"Revenue mensuel envois de fonds"
names(df2)[56]<-"Revenue mensuel aides et cadeau"
names(df2)[57]<-"Revenue mensuel autres activités"

#transformé les valeurs manquantes en NA
df1[df1=="."]<-NA
df2[df2=="."]<-NA

#Transformé les caractère en valeur numériques (donnée 1)
df1$`Nombre enfants`<- as.numeric(sub(",",".",df1$`Nombre enfants`))
df1$`Taille ménage`<- as.numeric(sub(",",".",df1$`Taille ménage`))
df1$`Garçon à école`<- as.numeric(sub(",",".",df1$`Garçon à école`))
df1$`Fille à école`<-as.numeric(sub(",",".",df1$`Fille à école`))
df1$`Garçon jamais à école`<-as.numeric(sub(",",".",df1$`Garçon jamais à école`))
df1$`Fille jamais à école`<-as.numeric(sub(",",".",df1$`Fille jamais à école`))
df1$`Garçon arrète école`<-as.numeric(sub(",",".",df1$`Garçon arrète école`))
df1$`Fille arrète école`<- as.numeric(sub(",",".",df1$`Fille arrète école`))
df1$`Transferts de migrant 12 derniers mois`<-as.numeric(sub(",",".",df1$`Transferts de migrant 12 derniers mois`))
df1$`Trasnferts de migrant par tête`<-as.numeric(sub(",",".",df1$`Trasnferts de migrant par tête`))
df1$`membres total en migration`<-as.numeric(sub(",",".",df1$`membres total en migration`))
df1$`Nombre de personne par pièce`<-as.numeric(sub(",",".",df1$`Nombre de personne par pièce`))
df1$`Nombre équipement possédés par ménage`<-as.numeric(sub(",",".",df1$`Nombre équipement possédés par ménage`))
df1$`Nombre total de tête en bétail`<-as.numeric(sub(",",".",df1$`Nombre total de tête en bétail`))
df1$`Nombre source de revenu`<-as.numeric(sub(",",".",df1$`Nombre source de revenu`))
df1$`Dépense mensuelle pour santé`<-as.numeric(sub(",",".",df1$`Dépense mensuelle pour santé`))
df1$`Dépense mensuelle pour éducation`<-as.numeric(sub(",",".",df1$`Dépense mensuelle pour éducation`))
df1$`Dépense total`<-as.numeric(sub(",",".",df1$`Dépense total`))
df1$`Dépense total par tête`<-as.numeric(sub(",",".",df1$`Dépense total par tête`))
df1$`Part de dépense total pour éducation`<-as.numeric(sub(",",".",df1$`Part de dépense total pour éducation`))
df1$`Taux salle de classe-élève`<-as.numeric(sub(",",".",df1$`Taux salle de classe-élève`))
df1$`Taux élève-professeur`<-as.numeric(sub(",",".",df1$`Taux élève-professeur`))
df1$`Revenue total mensuel`<-as.numeric(sub(",",".",df1$`Revenue total mensuel`))
df1$`Revenue mensuel agriculture`<-as.numeric(sub(",",".",df1$`Revenue mensuel agriculture`))
df1$`Revenue mensuel aides et cadeau`<-as.numeric(sub(",",".",df1$`Revenue mensuel aides et cadeau`))
df1$`Revenue mensuel artisanat et transport`<-as.numeric(sub(",",".",df1$`Revenue mensuel artisanat et transport`))
df1$`Revenue mensuel autres activités`<- as.numeric(sub(",",".",df1$`Revenue mensuel autres activités`))
df1$`Revenue mensuel bétail`<-as.numeric(sub(",",".",df1$`Revenue mensuel bétail`))
df1$`Revenue mensuel commerce`<-as.numeric(sub(",",".",df1$`Revenue mensuel commerce`))
df1$`Revenue mensuel envois de fonds`<-as.numeric(sub(",",".",df1$`Revenue mensuel envois de fonds`))
df1$`Revenue mensuel maraîchage`<-as.numeric(sub(",",".",df1$`Revenue mensuel maraîchage`))
df1$`Revenue mensuel petit commerce`<-as.numeric(sub(",",".",df1$`Revenue mensuel petit commerce`))
df1$`Revenue mensuel pêcherie`<-as.numeric(sub(",",".",df1$`Revenue mensuel pêcherie`))
df1$`Revenue mensuel salaire de travail`<-as.numeric(sub(",",".",df1$`Revenue mensuel salaire de travail`))
df1$`Revenue mensuel salaire inclu pension`<-as.numeric(sub(",",".",df1$`Revenue mensuel salaire inclu pension`))
df1$`Éducation mère`<-as.numeric(sub(",",".",df1$`Éducation mère`))
df1$`Education chef du ménage`<-as.numeric(sub(",",".",df1$`Education chef du ménage`))
df1$`Raison fille arrète école`<-as.numeric(sub(",",".",df1$`Raison fille arrète école`))
df1$`Raison fille jamais à école`<-as.numeric(sub(",",".",df1$`Raison fille jamais à école`))
df1$`Raison garçon arrète école`<-as.numeric(sub(",",".",df1$`Raison garçon arrète école`))
df1$`Raison garçon jamais à école`<-as.numeric(sub(",",".",df1$`Raison garçon jamais à école`))
df1$`Migrant est chef du ménage`<-as.numeric(sub(",",".",df1$`Migrant est chef du ménage`))
df1$`Proprietaire de logement`<-as.numeric(sub(",",".",df1$`Proprietaire de logement`))
df1$`Nombre de choc 12 dernier mois`<-as.numeric(sub(",",".",df1$`Nombre de choc 12 dernier mois`))
df1$`Type de migration`<-as.numeric(sub(",",".",df1$`Type de migration`))
df1$`Type de stratégie`<-as.numeric(sub(",",".",df1$`Type de stratégie`))
df1$`Destination de migration 1`<-as.numeric(sub(",",".",df1$`Destination de migration 1`))
df1$`Destination de migration 2`<-as.numeric(sub(",",".",df1$`Destination de migration 2`))
df1$`Destination de migration 3`<-as.numeric(sub(",",".",df1$`Destination de migration 3`))
df1$`Centre alphabétisation dans village`<-as.numeric(sub(",",".",df1$`Centre alphabétisation dans village`))
df1$`École primaire dans village`<-as.numeric(sub(",",".",df1$`École primaire dans village`))

#Transformé les caractère en valeurs numériques (donnée 2)
df2$`Nombre enfants`<- as.numeric(sub(",",".",df2$`Nombre enfants`))
df2$`Taille ménage`<- as.numeric(sub(",",".",df2$`Taille ménage`))
df2$`Garçon à école`<- as.numeric(sub(",",".",df2$`Garçon à école`))
df2$`Fille à école`<-as.numeric(sub(",",".",df2$`Fille à école`))
df2$`Garçon jamais à école`<-as.numeric(sub(",",".",df2$`Garçon jamais à école`))
df2$`Fille jamais à école`<-as.numeric(sub(",",".",df2$`Fille jamais à école`))
df2$`Garçon arrète école`<-as.numeric(sub(",",".",df2$`Garçon arrète école`))
df2$`Fille arrète école`<- as.numeric(sub(",",".",df2$`Fille arrète école`))
df2$`Transferts de migrant 12 derniers mois`<-as.numeric(sub(",",".",df2$`Transferts de migrant 12 derniers mois`))
df2$`Trasnferts de migrant par tête`<-as.numeric(sub(",",".",df2$`Trasnferts de migrant par tête`))
df2$`membres total en migration`<-as.numeric(sub(",",".",df2$`membres total en migration`))
df2$`Nombre de personne par pièce`<-as.numeric(sub(",",".",df2$`Nombre de personne par pièce`))
df2$`Nombre équipement possédés par ménage`<-as.numeric(sub(",",".",df2$`Nombre équipement possédés par ménage`))
df2$`Nombre total de tête en bétail`<-as.numeric(sub(",",".",df2$`Nombre total de tête en bétail`))
df2$`Nombre source de revenu`<-as.numeric(sub(",",".",df2$`Nombre source de revenu`))
df2$`Dépense mensuelle pour santé`<-as.numeric(sub(",",".",df2$`Dépense mensuelle pour santé`))
df2$`Dépense mensuelle pour éducation`<-as.numeric(sub(",",".",df2$`Dépense mensuelle pour éducation`))
df2$`Dépense total`<-as.numeric(sub(",",".",df2$`Dépense total`))
df2$`Dépense total par tête`<-as.numeric(sub(",",".",df2$`Dépense total par tête`))
df2$`Part de dépense total pour éducation`<-as.numeric(sub(",",".",df2$`Part de dépense total pour éducation`))
df2$`Taux salle de classe-élève`<-as.numeric(sub(",",".",df2$`Taux salle de classe-élève`))
df2$`Taux élève-professeur`<-as.numeric(sub(",",".",df2$`Taux élève-professeur`))
df2$`Revenue total mensuel`<-as.numeric(sub(",",".",df2$`Revenue total mensuel`))
df2$`Revenue mensuel agriculture`<-as.numeric(sub(",",".",df2$`Revenue mensuel agriculture`))
df2$`Revenue mensuel aides et cadeau`<-as.numeric(sub(",",".",df2$`Revenue mensuel aides et cadeau`))
df2$`Revenue mensuel artisanat et transport`<-as.numeric(sub(",",".",df2$`Revenue mensuel artisanat et transport`))
df2$`Revenue mensuel autres activités`<- as.numeric(sub(",",".",df2$`Revenue mensuel autres activités`))
df2$`Revenue mensuel bétail`<-as.numeric(sub(",",".",df2$`Revenue mensuel bétail`))
df2$`Revenue mensuel commerce`<-as.numeric(sub(",",".",df2$`Revenue mensuel commerce`))
df2$`Revenue mensuel envois de fonds`<-as.numeric(sub(",",".",df2$`Revenue mensuel envois de fonds`))
df2$`Revenue mensuel maraîchage`<-as.numeric(sub(",",".",df2$`Revenue mensuel maraîchage`))
df2$`Revenue mensuel petit commerce`<-as.numeric(sub(",",".",df2$`Revenue mensuel petit commerce`))
df2$`Revenue mensuel pêcherie`<-as.numeric(sub(",",".",df2$`Revenue mensuel pêcherie`))
df2$`Revenue mensuel salaire de travail`<-as.numeric(sub(",",".",df2$`Revenue mensuel salaire de travail`))
df2$`Revenue mensuel salaire inclu pension`<-as.numeric(sub(",",".",df2$`Revenue mensuel salaire inclu pension`))

#Arondie les décimal (donnée 1)
df1$`Trasnferts de migrant par tête`<- round(df1$`Trasnferts de migrant par tête`,digits = 2)
df1$`Nombre de personne par pièce`<-round(df1$`Nombre de personne par pièce`,digits = 2)
df1$`Nombre total de tête en bétail`<-round(df1$`Nombre total de tête en bétail`,digits = 2)
df1$`Dépense mensuelle pour santé`<-round(df1$`Dépense mensuelle pour santé`,digits = 2)
df1$`Dépense mensuelle pour éducation`<-round(df1$`Dépense mensuelle pour éducation`,digits = 2)
df1$`Dépense total`<-round(df1$`Dépense total`,digits = 2)
df1$`Dépense total par tête`<-round(df1$`Dépense total par tête`,digits = 2)
df1$`Part de dépense total pour éducation`<-round(df1$`Part de dépense total pour éducation`,digits = 2)
df1$`Taux salle de classe-élève`<-round(df1$`Taux salle de classe-élève`,digits = 2)
df1$`Taux élève-professeur`<-round(df1$`Taux élève-professeur`,digits = 2)
df1$`Revenue total mensuel`<-round(df1$`Revenue total mensuel`,digits = 2)
df1$`Revenue mensuel agriculture`<-round(df1$`Revenue mensuel agriculture`,digits = 2)
df1$`Revenue mensuel aides et cadeau`<-round(df1$`Revenue mensuel aides et cadeau`,digits = 2)
df1$`Revenue mensuel artisanat et transport`<-round(df1$`Revenue mensuel artisanat et transport`,digits = 2)
df1$`Revenue mensuel autres activités`<-round(df1$`Revenue mensuel autres activités`,digits = 2)
df1$`Revenue mensuel bétail`<-round(df1$`Revenue mensuel bétail`,digits = 2)
df1$`Revenue mensuel commerce`<-round(df1$`Revenue mensuel commerce`,digits = 2)
df1$`Revenue mensuel envois de fonds`<-round(df1$`Revenue mensuel envois de fonds`,digits = 2)
df1$`Revenue mensuel maraîchage`<-round(df1$`Revenue mensuel maraîchage`,digits = 2)
df1$`Revenue mensuel petit commerce`<-round(df1$`Revenue mensuel petit commerce`,digits = 2)
df1$`Revenue mensuel pêcherie`<-round(df1$`Revenue mensuel pêcherie`,digits = 2)
df1$`Revenue mensuel salaire de travail`<-round(df1$`Revenue mensuel salaire de travail`,digits = 2)
df1$`Revenue mensuel salaire inclu pension`<-round(df1$`Revenue mensuel salaire inclu pension`,digits = 2)

#Arondie les décimal (donnée 2)
df2$`Trasnferts de migrant par tête`<- round(df2$`Trasnferts de migrant par tête`,digits = 2)
df2$`Nombre de personne par pièce`<-round(df2$`Nombre de personne par pièce`,digits = 2)
df2$`Nombre total de tête en bétail`<-round(df2$`Nombre total de tête en bétail`,digits = 2)
df2$`Dépense mensuelle pour santé`<-round(df2$`Dépense mensuelle pour santé`,digits = 2)
df2$`Dépense mensuelle pour éducation`<-round(df2$`Dépense mensuelle pour éducation`,digits = 2)
df2$`Dépense total`<-round(df2$`Dépense total`,digits = 2)
df2$`Dépense total par tête`<-round(df2$`Dépense total par tête`,digits = 2)
df2$`Part de dépense total pour éducation`<-round(df2$`Part de dépense total pour éducation`,digits = 2)
df2$`Taux salle de classe-élève`<-round(df2$`Taux salle de classe-élève`,digits = 2)
df2$`Taux élève-professeur`<-round(df2$`Taux élève-professeur`,digits = 2)
df2$`Revenue total mensuel`<-round(df2$`Revenue total mensuel`,digits = 2)
df2$`Revenue mensuel agriculture`<-round(df2$`Revenue mensuel agriculture`,digits = 2)
df2$`Revenue mensuel aides et cadeau`<-round(df2$`Revenue mensuel aides et cadeau`,digits = 2)
df2$`Revenue mensuel artisanat et transport`<-round(df2$`Revenue mensuel artisanat et transport`,digits = 2)
df2$`Revenue mensuel autres activités`<-round(df2$`Revenue mensuel autres activités`,digits = 2)
df2$`Revenue mensuel bétail`<-round(df2$`Revenue mensuel bétail`,digits = 2)
df2$`Revenue mensuel commerce`<-round(df2$`Revenue mensuel commerce`,digits = 2)
df2$`Revenue mensuel envois de fonds`<-round(df2$`Revenue mensuel envois de fonds`,digits = 2)
df2$`Revenue mensuel maraîchage`<-round(df2$`Revenue mensuel maraîchage`,digits = 2)
df2$`Revenue mensuel petit commerce`<-round(df2$`Revenue mensuel petit commerce`,digits = 2)
df2$`Revenue mensuel pêcherie`<-round(df2$`Revenue mensuel pêcherie`,digits = 2)
df2$`Revenue mensuel salaire de travail`<-round(df2$`Revenue mensuel salaire de travail`,digits = 2)
df2$`Revenue mensuel salaire inclu pension`<-round(df2$`Revenue mensuel salaire inclu pension`,digits = 2)

#Statistique descriptive des variables numeriques
summary(df2$`Nombre enfants`)
summary(df2$`Taille ménage`)
summary(df2$`Garçon à école`)
summary(df2$`Fille à école`)
summary(df2$`Garçon jamais à école`)
summary(df2$`Fille jamais à école`)
summary(df2$`Garçon arrète école`)
summary(df2$`Fille arrète école`)
summary(df2$`Transferts de migrant 12 derniers mois`)
summary(df2$`Trasnferts de migrant par tête`)
summary(df2$`Membres masculin migré`)
summary(df2$`Membres feminin migré`)
summary(df2$`membres total en migration`)
summary(df2$`Nombre de personne par pièce`)
summary(df2$`Nombre équipement possédés par ménage`)
summary(df2$`Nombre total de tête en bétail`)
summary(df2$`Nombre source de revenu`)
summary(df2$`Dépense mensuelle pour santé`)
summary(df2$`Dépense mensuelle pour éducation`)
summary(df2$`Dépense total`)
summary(df2$`Dépense total par tête`)
summary(df2$`Part de dépense total pour éducation`)
summary(df2$`Taux élève-professeur`)
summary(df2$`Taux salle de classe-élève`)
summary(df2$`Revenue total mensuel`)
summary(df2$`Revenue mensuel agriculture`)
summary(df2$`Revenue mensuel bétail`)
summary(df2$`Revenue mensuel pêcherie`)
summary(df2$`Revenue mensuel maraîchage`)
summary(df2$`Revenue mensuel petit commerce`)
summary(df2$`Revenue mensuel commerce`)
summary(df2$`Revenue mensuel salaire de travail`)
summary(df2$`Revenue mensuel artisanat et transport`)
summary(df2$`Revenue mensuel salaire inclu pension`)
summary(df2$`Revenue mensuel envois de fonds`)
summary(df2$`Revenue mensuel aides et cadeau`)
summary(df2$`Revenue mensuel autres activités`)

#Pourcentage des valeurs manquantes dans la base de donnée
sum(is.na(df1))/prod(dim(df1))
sum(is.na(df2))/prod(dim(df2))

#La somme des valeurs manquantes pour chaque variable
sum(is.na(df2$`Transferts de migrant 12 derniers mois`))

#Supprimer les valeurs manquantes de la variable "Transferts de migrant 12 derniers mois"
df1<-df1%>%drop_na(`Transferts de migrant 12 derniers mois`)
df2<-df2%>%drop_na(`Transferts de migrant 12 derniers mois`)

#Affiche les différentes quantiles pour les variables utilisés
quantile(df2$`Revenue total mensuel`)
quantile(df2$`Dépense mensuelle pour éducation`)
quantile(df2$`Transferts de migrant 12 derniers mois`)
quantile(df2$`Dépense mensuelle pour santé`)

#Corrélation entre les variables numériques utilisées
var1<-df2[,c(20:45)]
var1<-var1[,-c(2:10)]
var1<-var1[,-c(4:16)]
cor<-cor(var1)
corrplot(cor,order = "original",method = "number",type = "lower",tl.cex = 0.7)

#Histogramme
barplot1<-ggplot(data = df2,mapping=aes(x=df2$`Code région`,y=df2$`Transferts de migrant 12 derniers mois`))+
  geom_bar(stat = "identity",aes(fill=df2$`Code région`),position = "dodge",col="transparent")+
  labs(subtitle = "Transferts de migrant 12 derniers mois par région",x="Région",y="Transferts de migrant 12 derniers mois")+
  guides(fill=guide_legend(title="Code de région"))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 14,face = "bold"))
barplot1

barplot2<-ggplot(data = df2,mapping=aes(x=df2$`Code région`,y=df2$`Dépense mensuelle pour santé`))+
  geom_bar(stat = "identity",aes(fill=df2$`Code région`),position = "dodge",col="transparent")+
  labs(subtitle = "Dépense mensuelle pour santé par région",x="Région",y="Dépense mensuelle pour santé")+
  guides(fill=guide_legend(title="Code de région"))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 14,face = "bold"))
barplot2

barplot3<-ggplot(data = df2,mapping=aes(x=df2$`Code région`,y=df2$`Dépense mensuelle pour éducation`))+
  geom_bar(stat = "identity",aes(fill=df2$`Code région`),position = "dodge",col="transparent")+
  labs(subtitle = "Dépense mensuelle pour éducation par région",x="Région",y="Dépense mensuelle pour éducation")+
  guides(fill=guide_legend(title="Code de région"))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 14,face = "bold"))
barplot3

barplot4<-ggplot(data = df2,mapping=aes(x=df2$`Code région`,y=df2$`Revenue total mensuel`))+
  geom_bar(stat = "identity",aes(fill=df2$`Code région`),position = "dodge",col="transparent")+
  labs(subtitle = "Revenue mensuel par région",x="Région",y="Revenue mensuel")+
  guides(fill=guide_legend(title="Code de région"))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 14,face = "bold"))

barplot4

barplot5<-ggplot(data = df2,mapping=aes(x=df2$`Code région`,y=df2$`Type de migration`))+
  geom_bar(stat = "identity",aes(fill=df2$`Type de migration`),col="transparent")+
  labs(subtitle = "Type de migration par région",x="Région",y="Type de migration")+
  guides(fill=guide_legend(title="Type de migration"))+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 14,face = "bold"))
barplot5

#Les cartograohies
#Import "Map shape" file
options(scipen = 999)
mymap<-st_read("gadm36_MLI_shp/gadm36_MLI_4.shp",stringsAsFactors=FALSE)

#Joindre Map aux données
names(mymap)[4]<-"Code région"
names(mymap)[6]<-"Cercle"
map_and_data<-inner_join(mymap,df2)

#Les graphiques
sf1<-ggplot(map_and_data)+
  geom_sf(aes(fill=map_and_data$`Revenue total mensuel`))+
  scale_fill_gradient(low = "#56B1F7",high = "#132B43")
sf1
map1<-tm_shape(map_and_data)+
  tm_polygons("Revenue total mensuel",id="Code région",palette="Blues")
map1
tmap_mode("view")
tmap_last()

sf2<-ggplot(map_and_data)+
  geom_sf(aes(fill=map_and_data$`Transferts de migrant 12 derniers mois`))+
  scale_fill_gradient(low = "#56B1F7",high = "#132B43")
sf2
map2<-tm_shape(map_and_data)+
  tm_polygons("Transferts de migrant 12 derniers mois",id="Code région",palette="Blues")
map2
tmap_mode("view")
tmap_last()

sf3<-ggplot(map_and_data)+
  geom_sf(aes(fill=map_and_data$`Dépense mensuelle pour éducation`))+
  scale_fill_gradient(low = "#56B1F7",high = "#132B43")
sf3
map3<-tm_shape(map_and_data)+
  tm_polygons("Dépense mensuelle pour éducation",id="Code région",palette="Blues")
map3
tmap_mode("view")
tmap_last()

sf4<-ggplot(map_and_data)+
  geom_sf(aes(fill=map_and_data$`Dépense mensuelle pour santé`))+
  scale_fill_gradient(low = "#56B1F7",high = "#132B43")
sf4
map4<-tm_shape(map_and_data)+
  tm_polygons("Dépense mensuelle pour santé",id="Code région",palette="Blues")
map4
tmap_mode("view")
tmap_last()

sf5<-ggplot(map_and_data)+
  geom_sf(aes(fill=map_and_data$`Part de dépense total pour éducation`))+
  scale_fill_gradient(low = "#56B1F7",high = "#132B43")
sf5
map5<-tm_shape(map_and_data)+
  tm_polygons("Part de dépense total pour éducation",id="Code région",palette="Oranges")
map5
tmap_mode("view")
tmap_last()

sf6<-ggplot(map_and_data)+
  geom_sf(aes(fill=map_and_data$`Garçon à école`))+
  scale_fill_gradient(low = "#56B1F7",high = "#132B43")
sf6
map6<-tm_shape(map_and_data)+
  tm_polygons("Garçon à école",id="Code région",palette="Greens")
map6
tmap_mode("view")
tmap_last()

sf7<-ggplot(map_and_data)+
  geom_sf(aes(fill=map_and_data$`Fille à école`))+
  scale_fill_gradient(low = "#56B1F7",high = "#132B43")
sf7
map7<-tm_shape(map_and_data)+
  tm_polygons("Fille à école",id="Code région",palette="Reds")
map7
tmap_mode("view")
tmap_last()