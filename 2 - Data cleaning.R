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
# Chargement de la base
# -----------------------------------------------------
system.time(load("./Data/don.RData"))

# -----------------------------------------------------
# Analyses
# -----------------------------------------------------

# Detection des NA par variable 
stats <- as.data.frame(cbind(sapply(don,function(x) sum(is.na(x))),
                             sapply(don,function(x) sum(is.na(x))/nrow(don)*100),
                             sapply(don,function(x) nrow(don)-sum(is.na(x)))))
stats <- stats[order(-stats$V2),]

stats<-don %>% group_by(Code.type.local,Surface.terrain) %>% summarise(n=n())

don$Surface.terrain[is.na(don$Surface.terrain)] <- 0
summary(don$Nombre.de.lots)
summary(don$Code.type.local)
summary(don$Surface.reelle.bati)
summary(don$Nombre.pieces.principales)
summary(don$Surface.terrain)

don$Surface.terrain2<-don$Surface.terrain
don$Surface.terrain2[is.na(don$Surface.terrain2)] <- 0
summary(don$Surface.terrain2)

temp<-don %>% filter(is.na(Equ_1001)) %>% group_by(CODGEO,Code.postal,Commune) %>% summarise(n=n(),moy=mean(Dyn_Popu))

stats<-don %>% group_by(Code.type.local,Type.local) %>% summarise(n=n(),pour=n()/nrow(don),moy=mean(Valeur.fonciere))

Q1<-quantile(x=don$Surface.reelle.bati,probs=0.25)
Q3<-quantile(x=don$Surface.reelle.bati,probs=0.75)
BoxPlotMin=Q1-1.5*(Q3-Q1)
BoxPlotMax=Q3+1.5*(Q3-Q1)

# -----------------------------------------------------
# Analyse variable Y - Valeur.fonciere 
# -----------------------------------------------------

# Statistiques de base
stats_ori <- don  %>% summarise(n=n(),moy=mean(Valeur.fonciere),
                                Min=min(Valeur.fonciere),
                                Q1=quantile(Valeur.fonciere,0.25),
                                Q2=quantile(Valeur.fonciere,0.5),
                                Q3=quantile(Valeur.fonciere,0.75),
                                Max=max(Valeur.fonciere),
                                SD=sd(Valeur.fonciere))

# Analyse 1er percentile pour determiner la borne min
Perc01 <- quantile(x=don$Valeur.fonciere,probs=0.01)
temp <- don[don$Valeur.fonciere<=Perc01,,]
a <- quantile(x=temp$Valeur.fonciere,probs=seq(0,1,0.01))

b<-as.data.frame(a)
b$abs<-seq(0,1,0.01)
b$ord<-a
b$Var<-(b$ord-lag(b$ord))/lag(b$ord)*100
is.na(b$Var)<-0

# Graphiques
p <- ggplot(data=b, aes(x=abs, y=Var))+ geom_point() + geom_line(size=0.5) +ylim(0,30) + geom_vline(aes(xintercept=0.67), color="red", linetype="dashed", size=1)
p
p <- ggplot(data=b, aes(x=abs, y=ord))+ geom_point() + geom_line(size=0.5) + geom_hline(aes(yintercept=10500), color="red", linetype="dashed", size=1)
p
# Proposition : On coupe à 10500

# Analyse 99eme percentile pour determiner la borne max
Perc99 <- quantile(x=don$Valeur.fonciere,probs=0.99)
temp <- don[Perc99<=don$Valeur.fonciere,,]
a <- quantile(x=temp$Valeur.fonciere,probs=seq(0,1,0.01))

b<-as.data.frame(a)
b$abs<-seq(0,1,0.01)
b$ord<-a
b$Var<-(b$ord-lag(b$ord))/lag(b$ord)*100
is.na(b$Var)<-0

# Graphiques
p <- ggplot(data=b, aes(x=abs, y=Var))+ geom_point() + geom_line(size=0.5) +ylim(0,30) + geom_vline(aes(xintercept=0.91), color="red", linetype="dashed", size=1)
p
p <- ggplot(data=b, aes(x=abs, y=ord))+ geom_point() + geom_line(size=0.5) +ylim(0,7000000) + geom_hline(aes(yintercept=4674998), color="red", linetype="dashed", size=1)
p
# Proposition : On coupe à 4674998

# Valeur.fonciere : Selection finale
don <- don[don$Valeur.fonciere>10500 & don$Valeur.fonciere<4674998,,]
save(don,file="./Data/don.RData") 
nrow(don)
# 3820313


# Stats decriptives finales
stats <- don  %>% summarise(n=n(),moy=mean(Valeur.fonciere),
                            Min=min(Valeur.fonciere),
                            Q1=quantile(Valeur.fonciere,0.25),
                            Q2=quantile(Valeur.fonciere,0.5),
                            Q3=quantile(Valeur.fonciere,0.75),
                            Max=max(Valeur.fonciere),
                            SD=sd(Valeur.fonciere))

p <- ggplot(data=don, aes(y=Valeur.fonciere)) + geom_boxplot() + ylim(0,500000)
p

describe(don$Valeur.fonciere)

# Test de normalité 
# Test Anderson-Darling
install.packages("nortest")
library(nortest)
ad.test(don$Valeur.fonciere)
# Ici on rejete H0

# Méthode graphique 
library("dplyr")
library("ggpubr")
# Diagramme de densité
ggdensity(don$Valeur.fonciere, fill = "lightgray")
# QQ plot
ggqqplot(don$Valeur.fonciere)

# skewness : le coefficient d'asymétrie g1
library(e1071)                
skewness(don$Valeur.fonciere)
# concl : assymetrie à droite

# kurtosis : le coefficient d'applatissement g2
kurtosis(don$Valeur.fonciere)
# concl : non plate

# Conclusion : Distibution non normale, assymetrie à droite, non plate - Pas de manquants + Filtre [10500;4674998]


# -----------------------------------------------------
# Analyse variables X
# -----------------------------------------------------

# -----------------------------------------------------
# Creation variables Surface et nb lots

don$Surface.Carrez <- rowSums(don[,c("Surface.Carrez.du.1er.lot","Surface.Carrez.du.2eme.lot","Surface.Carrez.du.3eme.lot","Surface.Carrez.du.4eme.lot","Surface.Carrez.du.5eme.lot")],na.rm=TRUE)

don$X1er.lot <- as.numeric(don$X1er.lot)
don$X2eme.lot <- as.numeric(don$X2eme.lot)
don$X3eme.lot <- as.numeric(don$X3eme.lot)
don$X4eme.lot <- as.numeric(don$X4eme.lot)
don$X5eme.lot <- as.numeric(don$X5eme.lot)

don$nb.lots <- ifelse(is.na(don$X1er.lot),0,1)+ifelse(is.na(don$X2eme.lot),0,1)+ifelse(is.na(don$X3eme.lot),0,1)+ifelse(is.na(don$X4eme.lot),0,1)+ifelse(is.na(don$X5eme.lot),0,1)

# -----------------------------------------------------
# Transformation des variables

don$IndPourcEuri1M_corr<-str_length(don$IndPourcEuri1M)
don$IndPourcEuri1M_corr<-don[don$IndPourcEuri1M=="-",]

liste<-c("IndTxUsurePartM10ansTrim", "IndTxUsurePartM10a20ansTrim", "IndTxUsurePartM20ansTrim", "IndTxUsurePartVarTrim", 
         "IndTxIntLegalPart", "IndTxIntLegalAutres", "IndFacDepotMens", "IndFacPretMens", "IndTxRefinMens", "IndMaFinFactCreditMens", 
         "IndMaFinFactActionsMens", "IndMaFinFactTxChangeMens", "IndMaFinFactInflationMens", "IndMaFinFactTxInteretMens", 
         "IndMaFinFactIncertitudeMens", "IndPourcEuri1M", "IndPourcEuri1S", "IndPourcEuri12M", "IndPourcEuri3M", "IndPourcEuri6M", 
         "IndTxAvSurTitreWSJour", "IndTxEch1anJour", "IndTxEch10anJour", "IndTxEch15anJour", "IndTxEch2anJour", "IndTxEch20anJour", 
         "IndTxEch25anJour", "IndTxEch3anJour", "IndTxEch30anJour", "IndTxEch5anJour", "IndTxEch7anJour", "IndEmp10ansJour", 
         "IndTxBonsTres1MJour", "IndTxBonsTres12MJour", "IndTxOAT2ansJour", "IndTxOAT30ansJour", "IndTxBonsTres3MJour", 
         "IndTxOAT5ansJour", "IndTxBonsTres6MJour", "IndTxBonsTres9MJour", "IndTxAvTitresWSJour", "IndTMBWSMois", "IndTMEWSMois", 
         "IndTMOWSMois", "IndTAMWSMois", "IndCoursOrEuWSJour", "IndCoursOrDoWSJour", "IndTxEoniaWSJour", "IndTxEuribor1MWSJour", 
         "IndTxEuribor1SWSJour", "IndTxEuribor12MWSJour", "IndTxEuribor3MWSJour", "IndTxEuribor6MWSJour", "IndFacDepotWSJour", 
         "IndFacEmpruntWSJour", "IndOpRefiWSJour", "IndFacDepotWSDebMois", "IndFacEmpruntWSDebMois", "IndOpRefiWSDebMois")

for(i in liste){
  print(i)
  don[,i]<-ifelse(don[,i]=="-","0",don[,i])
  don[,i]<-as.numeric(as.character(don[,i]))
  class(don[,i])
}

unique(don$Dyn_IndFiscPart)
class(don[,"IndTxBonsTres6MJour"])

# -----------------------------------------------------
# Analyse du type des variables + corrections
temp <- as.data.frame(sapply(don[,select_if(select(don,starts_with("Ind")), is_character),], function(x) as.numeric(sub(",",".",x)))) 

# Convertit en num + change la decimale des variables char qui commencent par Ind
don<-cbind(don[,colnames(temp):=NULL],temp) # Remplace les colonnes converties

# Affichage des variables numériques et des variables caractere dans 2 tables distinctes
VarNum <- don[, .SD, .SDcols = sapply(don, is.numeric)]
View(head(VarNum))

VarChar <- don[, .SD, .SDcols = sapply(don, is.character)]
View(head(VarChar))

# -----------------------------------------------------
# Analyse des NA + corrections

# Detecter les NA
stats <- as.data.frame(cbind(sapply(don,function(x) sum(is.na(x))),sapply(don,function(x) sum(is.na(x))/nrow(don)*100),sapply(don,function(x) nrow(don)-sum(is.na(x)))))
stats <- stats[order(-stats$V2),]

colvides<-stats[stats$V2>9,]
View(stats)

# Suppression des variables vides : 
don$Prefixe.de.section <- NULL
don$IndOpRefiWSDebMois <- NULL
don$IndFacDepotWSDebMois <- NULL
don$IndTxUsurePartM10ansTrim <- NULL
don$IndTxUsurePartM10a20ansTrim <- NULL
don$IndTxUsurePartM20ansTrim <- NULL
don$IndFacPretMens<-NULL
don$IndTxRefinMens<-NULL
don$IndFacEmpruntWSJour<-NULL
don$IndOpRefiWSJour<-NULL
don$IndFacEmpruntWSDebMois<-NULL
don$IndOpRefiWSDebMois<-NULL

# Surface terrain
temp <- don[is.na(Surface.terrain),,]
don$F.Surface.terrain[!is.na(don$Surface.terrain)] <- 1
don$F.Surface.terrain[is.na(don$Surface.terrain)] <- 0

stats <- don  %>% group_by(Type.local) %>% summarise(ntot=n(),nSurTer=sum(F.Surface.terrain,rm.na=T),pourc=nSurTer/ntot*100) 
stats <- don  %>% group_by(Type.local,Code.departement) %>% summarise(ntot=n(),nSurTer=sum(F.Surface.terrain,rm.na=T),pourc=nSurTer/ntot*100) 
don <- don[,-don$F.Surface.terrain,]

don$Surface.terrain[is.na(don$Surface.terrain)] <- 0

# Concl : Surface terrain absente pour 94% des appartements et 5% des maisons. 
# On suppose que les manquants correspondent à des lots sans terrain. On recode les NA par 0. 

# -----------------------------------------------------
# Sauvegarde de la base
don3 <- don
save(don3,file="./Data/don3.RData") 
