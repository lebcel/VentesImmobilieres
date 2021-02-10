# -----------------------------------------------------
# Librairies
# -----------------------------------------------------
library(data.table)
library(readr)
library(tidyverse)
library(readxl)
library(purrr)
library(funModeling)
library(plotly)
library(ggplot2)
library(lubridate)
library(magrittr)
library(R.utils)

options( "digits"=1, "scipen"=100)

# -----------------------------------------------------
# Import donnees
# -----------------------------------------------------
don <-
  list.files("./",
             full.names = TRUE) %>% 
  tolower() %>% 
  .[stringr::str_detect(., "valeursfoncieres")] %>% 
  purrr::map(~fread(.x, encoding = "UTF-8", check.names = T, drop = 1:7, dec = ",")) %>% 
  rbindlist()

str(don)

# -----------------------------------------------------
# Filtre pour alléger la table
don<-don[don$Nature.mutation=="Vente" & don$Type.local %in% c("Maison","Appartement") & !is.na(don$Valeur.fonciere) & don$Valeur.fonciere>0,,]
# Suppression des doublons
don<-don[,param:=duplicated(paste(Date.mutation,Valeur.fonciere,Code.commune, sep = "", collapse = NULL))] 
don<-don[param==FALSE,]

nrow(don)
# 3852389

# -----------------------------------------------------
# Formatage des variables

don[, Code.commune := stringr::str_pad(Code.commune, width = 3, side = "left", pad = "0")]
don[, CODGEO := paste(ifelse(don$Code.departement %in% c("971","972","973","974"),don$Code.departement,str_sub(don$Code.departement,1,2)),
                      ifelse(don$Code.departement %in% c("971","972","973","974"),str_sub(don$Code.commune,2),don$Code.commune),
                      sep = "", collapse = NULL)]
# Ici correction dept 97 ex commune St Martin

don<-don[,DTE_MOIS:=str_sub(str_sub(don$Date.mutation,4),1,2),]
don<-don[,DTE_ANNEE:=str_sub(str_sub(don$Date.mutation,4),-4),]
don<-don[,DTE:=paste(DTE_ANNEE,DTE_MOIS, sep = "-", collapse = NULL),]
don<-don[,TRIM:=cut(as.numeric(don$DTE_MOIS),breaks=c(0,3,6,9,12)),]
levels(don$TRIM)<- paste0("T",1:4)
don<-don[,DTE_TRIM:=paste(DTE_ANNEE,TRIM, sep = "-", collapse = NULL),]

nrow(don)
# 3852389

# -----------------------------------------------------
# Ajout données complémentaires 
# -----------------------------------------------------

# -----------------------------------------------------
# Dynamique des entreprises
ficdynvilles<- read_xls("./MDB-INSEE-V2.xls") %>% as.data.table()
setindex(don, CODGEO)
setindex(ficdynvilles, CODGEO)
sum(duplicated(ficdynvilles$CODGEO)) # Verifie s'il y a des doublons
don <- ficdynvilles[don, on = list(CODGEO)]

nrow(don)
# 3852389

# -----------------------------------------------------
# Equipements
ficEquip<- read_delim("./20180110_RES_FichesEquipements.csv",delim = ";", quote = "'") %>% as.data.table()
ficEquip[,EquiCode:=paste("Equ_",EquipementTypeCode,sep="",collapse = NULL)]
ficEquip<- dcast(ficEquip,ComInsee~EquiCode,value.var="EquAnneeService")
# !!!! Ici il faudrait mettre l'année minimum dans les valeurs du tableau au lieu du nombre mais je ne sais pas faire j'ai donc laissé le nombre
setnames(ficEquip,"ComInsee","CODGEO")
setindex(ficEquip, CODGEO)
sum(duplicated(ficEquip$CODGEO)) # Verifie s'il y a des doublons
don <- ficEquip[don, on = list(CODGEO)]

nrow(don)
# 3852389

# -----------------------------------------------------
# Activites
ficEquipActe<- read_delim("./20180110_RES_FichesEquipementsActivites.csv",delim = ";", quote = "'") %>% as.data.table()
ficEquipActe[,ActCode:=paste("Act_",ActCode,sep="",collapse = NULL)]
ficEquipActe<- dcast(ficEquipActe,ComInsee~ActCode,value.var="EquNbEquIdentique")
# !!!! Ici il faudrait mettre l'année minimum dans les valeurs du tableau au lieu du nombre mais je ne sais pas faire j'ai donc laissé le nombre
setnames(ficEquipActe,"ComInsee","CODGEO")
setindex(ficEquipActe, CODGEO)
sum(duplicated(ficEquipActe$CODGEO)) # Verifie s'il y a des doublons
don <- ficEquipActe[don, on = list(CODGEO)]

nrow(don)
# 3852389

# -----------------------------------------------------
# Installations
ficInstal<- read_delim("./20180110_RES_FichesInstallations.csv",delim = ";", quote = "'") %>% as.data.table()
ficInstal[,LibInstal:=case_when(
  InsPartLibelle=="Non"~"Inst_Non",
  InsPartLibelle=="Complexe sportif"~"Inst_ComplSport",
  InsPartLibelle=="Etablissement scolaire"~"Inst_EtabScol",
  InsPartLibelle=="Piscine"~"Inst_Pisc",
  InsPartLibelle=="A\xe9rodrome/a\xe9roport"~"Inst_Aerop",
  InsPartLibelle=="Installation militaire"~"Inst_Milit",
  InsPartLibelle=="Base de plein air et/ou de loisirs"~"Inst_BaseLois",
  InsPartLibelle=="Domaine de ski"~"Inst_DomSki",
  InsPartLibelle=="Etablissement p\xe9nitentiaire"~"Inst_Pris",
  InsPartLibelle=="CREPS ou Ecole nationale"~"Inst_EcolNat",
  TRUE~"NA"
)]

ficInstal<- dcast(ficInstal,ComInsee~LibInstal,value.var="InsDateCreation")
# !!!! Ici il faudrait mettre l'année minimum dans les valeurs du tableau au lieu du nombre mais je ne sais pas faire j'ai donc laissé le nombre
# => Je n'ai pas intégré les transports autours des installations. Est ce que ça aurait une influence?
setnames(ficInstal,"ComInsee","CODGEO")
sum(duplicated(ficInstal$CODGEO))
don <- ficInstal[don, on = list(CODGEO)]

nrow(don)
# 3852389

# Tests
# temp<-ficInstal[ficInstal$ComInsee==78117,,]
# temp<-don[don$CODGEO==78117,,]

# -----------------------------------------------------
# Intégration des données économiques

# Consommation des ménages mensuel
ficConsoMenaMens<- read_delim("./DataEco/Conso menages - mens.csv",delim = ";", quote = "'") %>% as.data.table()
don <- ficConsoMenaMens[don, on = list(DTE)]

# Emplois salariés trimestriel
ficEmploiSalTrim<- read_delim("./DataEco/Emplois salaries - trim.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficEmploiSalTrim,"DTE","DTE_TRIM")
don <- ficEmploiSalTrim[don, on = list(DTE_TRIM)]

# Indice cout de la construction trimestriel
ficICCTrim<- read_delim("./DataEco/ICC - trim.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficICCTrim,"DTE","DTE_TRIM")
don <- ficICCTrim[don, on = list(DTE_TRIM)]

# Indice climat des affaires mensuel
ficClimAffMens<- read_delim("./DataEco/Ind Climat Affaires - mois.csv",delim = ";", quote = "'") %>% as.data.table()
don <- ficClimAffMens[don, on = list(DTE)]

# Indice du climat de l'emploi mensuel
ficClimEmplMens<- read_delim("./DataEco/Ind Climat Emploi - mois.csv",delim = ";", quote = "'") %>% as.data.table()
don <- ficClimEmplMens[don, on = list(DTE)]

# Indice de confiance des ménages mensuel
ficConfMenaMoisPays<- read_delim("./DataEco/Indice confiance conso menages - mois - pays.csv",delim = ";", quote = "'") %>% as.data.table()
don <- ficConfMenaMoisPays[don, on = list(DTE)]

# Indice de confiance des ménages mensuel
ficConfMenaMois<- read_delim("./DataEco/Indice confiance conso menages - mois.csv",delim = ";", quote = "'") %>% as.data.table()
ficConfMenaMois<-ficConfMenaMois[,-"CodEnqMenagesMens",]
don <- ficConfMenaMois[don, on = list(DTE)]

# Indice des prix des logements trimestriel
ficPrixLogTrim<- read_delim("./DataEco/Indice prix logements - trim.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficPrixLogTrim,"DTE","DTE_TRIM")
ficPrixLogTrim<-ficPrixLogTrim[,-"CodPrixLogemB2015Trim",]
don <- ficPrixLogTrim[don, on = list(DTE_TRIM)]

# Indice du volume des ventes mensuel
ficVolVentesMens<- read_delim("./DataEco/Indice volume ventes - mens.csv",delim = ";", quote = "'") %>% as.data.table()
ficVolVentesMens<-ficVolVentesMens[,-"CodVolVentesMens",]
don <- ficVolVentesMens[don, on = list(DTE)]

# Inflation mensuel
ficInflationMens<- read_delim("./DataEco/Inflation - Mens.csv",delim = ";", quote = "'") %>% as.data.table()
ficInflationMens<-ficInflationMens[,-"CodInflaB2015Mens",]
don <- ficInflationMens[don, on = list(DTE)]

# Indice des prix à la consommation mensuel
ficIPCMens<- read_delim("./DataEco/IPC - Mens.csv",delim = ";", quote = "'") %>% as.data.table()
ficIPCMens<-ficIPCMens[,-"CodPxConsoB2015Mens",]
don <- ficIPCMens[don, on = list(DTE)]

# Indice de reference des loyers trimestriel
ficIRLTrim<- read_delim("./DataEco/IRL - trim.csv",delim = ";", quote = "'") %>% as.data.table()
ficIRLTrim<-ficIRLTrim[,-"CodRefLoyerB100Trim",]
setnames(ficIRLTrim,"DTE","DTE_TRIM")
don <- ficIRLTrim[don, on = list(DTE_TRIM)]

# PIB annuel
ficPIBAnn<- read_delim("./DataEco/PIB - annee - Base 2014.csv",delim = ";", quote = "'") %>% as.data.table()
ficPIBAnn<-ficPIBAnn[,-"CodPIBMtB2014An",]
ficPIBAnn<-ficPIBAnn[,DTE_ANNEE:=as.character(DTE),]
ficPIBAnn<-ficPIBAnn[,-"DTE",]
don <- ficPIBAnn[don, on = list(DTE_ANNEE)]

# PIB trimestriel
ficPIBTrim<- read_delim("./DataEco/PIB - trim.csv",delim = ";", quote = "'") %>% as.data.table()
ficPIBTrim<-ficPIBTrim[,-"CodPIBB2014Trim",]
setnames(ficPIBTrim,"Période","DTE_TRIM")
don <- ficPIBTrim[don, on = list(DTE_TRIM)]

# Taux de chomage annuel + sexe
ficTxChoAnnSex<- read_delim("./DataEco/Taux chomage - annee - sex.csv",delim = ";", quote = "'") %>% as.data.table()
ficTxChoAnnSex<-ficTxChoAnnSex[,DTE_ANNEE:=as.character(DTE),]
ficTxChoAnnSex<-ficTxChoAnnSex[,-"DTE",]
don <- ficTxChoAnnSex[don, on = list(DTE_ANNEE)]

# Taux de chomage trimestriel
ficTxChoTrim<- read_delim("./DataEco/Taux chomage - trim.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficTxChoTrim,"DTE","DTE_TRIM")
don <- ficTxChoTrim[don, on = list(DTE_TRIM)]

# Depots / Emprunts / Refinancement journalier
ficWebStat1<- read_delim("./DataEco/Webstat_Export_20200309 1.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficWebStat1,"DTE","Date.mutation")
don <- ficWebStat1[don, on = list(Date.mutation)]

# Taux Euribor journalier
ficWebStat2<- read_delim("./DataEco/Webstat_Export_20200309 2.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficWebStat2,"DTE","Date.mutation")
don <- ficWebStat2[don, on = list(Date.mutation)]

# Euribor journalier
ficEuribor<- read_excel("./DataEco/EURIBOR.xlsx") %>% as.data.table()
setnames(ficEuribor,"DTE","Date.mutation")
don <- ficEuribor[don, on = list(Date.mutation)]

# Cours Or journalier
ficWebStat3<- read_delim("./DataEco/Webstat_Export_20200309 3.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficWebStat3,"DTE","Date.mutation")
don <- ficWebStat3[don, on = list(Date.mutation)]

# Taux moyens mensuel
ficWebStat4<- read_delim("./DataEco/Webstat_Export_20200309 4.csv",delim = ";", quote = "'") %>% as.data.table()
don <- ficWebStat4[don, on = list(DTE)]

# Taux avances sur titres / journalier
ficWebStat5<- read_delim("./DataEco/Webstat_Export_20200309 5.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficWebStat5,"DTE","Date.mutation")
don <- ficWebStat5[don, on = list(Date.mutation)]

# Emprunt Phare 10 ans / Taux bons du tresor / taux OAT journalier
ficWebStat6<- read_delim("./DataEco/Webstat_Export_20200309 6.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficWebStat6,"DTE","Date.mutation")
don <- ficWebStat6[don, on = list(Date.mutation)]

# Taux de l'Echéance Constante journalier
ficWebStat7<- read_delim("./DataEco/Webstat_Export_20200309 7.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficWebStat7,"DTE","Date.mutation")
don <- ficWebStat7[don, on = list(Date.mutation)]

# Indice taux des avances sur titres journalier
ficWebStat8<- read_delim("./DataEco/Webstat_Export_20200309 8.csv",delim = ";", quote = "'") %>% as.data.table()
setnames(ficWebStat8,"DTE","Date.mutation")
don <- ficWebStat8[don, on = list(Date.mutation)]

# Indices conditions marches financiers : Inflation / Tx change / Tx interets / Incertitudes mensuel
ficCondMarFin<- read_excel("./DataEco/Indice cond marches fin.xlsx") %>% as.data.table()
don <- ficCondMarFin[don, on = list(DTE)]

# Taux directeurs mensuel
ficTxDirecteur<- read_excel("./DataEco/Taux directeurs.xlsx") %>% as.data.table()
don <- ficTxDirecteur[don, on = list(DTE)]

# Taux Interet Legal trimestriel
ficTxIntLeg<- read_excel("./DataEco/Taux interet legal.xlsx") %>% as.data.table()
setnames(ficTxIntLeg,"DTE","DTE_TRIM")
don <- ficTxIntLeg[don, on = list(DTE_TRIM)]

# Taux Usure trimestriel
ficTxUsure<- read_excel("./DataEco/Taux usure.xlsx") %>% as.data.table()
setnames(ficTxUsure,"DTE","DTE_TRIM")
don <- ficTxUsure[don, on = list(DTE_TRIM)]

# Taux chomage par ville
ficTxChoAnnVille<- read_excel("./DataEco/Taux chomage - trim - ville.xls") %>% as.data.table()
# !!!!! Non integré car incomplet et je ne sais pas comment le lier
# temp<-ficTxChoAnnVille[LIBZE2010=="Dreux",,]


# -----------------------------------------------------
# Suppression des tables importees
remove(list=ls(pat="^fic"))

# -----------------------------------------------------
# Sauvegarde de la base
save(don,file="don.RData") 
