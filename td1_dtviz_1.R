###############################################
################ TD1 - DATAVIZ ################
############### TUAN LINH DAO #################
############### GROUPE B2 #####################
###############################################

########### IMPORTER LES DONNÉES ##############
DATA <- read.table(file= file.choose() , header=TRUE,sep=";",dec=",")
View(DATA)
summary(DATA)
dim(DATA)
options(max.print = 82170) 



######### CONVERTIR LE FORMAT DU JOUR #########
date <- DATA$jour
date

date_formatted <- as.Date(date, format = "%Y-%m-%d")
date_formatted

month <- format(date_formatted, "%m")
month

class(date_formatted)
month_col <- substr(DATA$jour,start = 6, stop = 7)
View(month_col)
class(month_col)




######### DÉCOUPER L'ÂGE EN CLASSE #########

# Choisir une variable à analyser dans la table DATA:
nom_var<-"cl_age90"
var<-DATA[,nom_var]

# Découper en 3 tranches d'âge :
cl_age90_cut <- cut(DATA$cl_age90, breaks = c(0,29,59,90),
labels = c("jeune","adulte","agée"), include.lowest = TRUE)
View(cl_age90_cut)
class(cl_age90_cut)

# Nouvelle table avec les 3 tranches d'âge :
DATA_1 <- data.frame(DATA$reg, cl_age90_cut, DATA$jour, DATA$hosp, 
DATA$rea, DATA$HospConv, DATA$SSR_USLD, DATA$autres, DATA$rad, DATA$dc)
View(DATA_1)






########## TRAITER LES DONNÉES DU MOIS MARS #########

# La table M_3 (toutes les données du MARS)
DATA_1$mois3 <- substr(DATA$jour, start = 6, stop = 7)
DATA_1$mois3
M_3 <- DATA_1[DATA_1$mois3 == "03",  ]
M_3
View(M_3)
dim(M_3)
M_3$DATA.dc <- as.numeric(as.character(M_3$DATA.dc))
class(M_3$DATA.dc)

# Nombre de région possible dans la table M_3 :
length(unique(M_3[,"DATA.reg"]))

# Lister toutes les régions possibles dans la table M_3:
unique(M_3[,"DATA.reg"])

# Choisir une variable à analyser dans la table M_3 :
nom_var<-"cl_age90_cut"
var3<-M_3[,nom_var]

# Effectifs des tranches d'âge dans la table M_3 :
Eff1 <- table(M_3$cl_age90_cut)
Eff1

# Le décompte des données valides :
Nval1 <- sum(!is.na(var3))
Nval1

# Fréquence des classes d'âges :
freq1 <- Eff1/Nval1
freq1

# Fréquence cumulée des classes d'âges :
freqcum1 <- cumsum(freq1)
freqcum1

# Data frame résumé pour M_3:
df1 <- data.frame(Effectif = Eff1, Fréquence = freq1, Fréquence_cumulée = freqcum1)
View(df1)

# NB DE DÉCÈS PAR TRANCHE D'ÂGE SELON LA RÉGION DANS LA TABLE M_3:
# RÉGION 1
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==1)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==1)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==1)
dc_ag
# data frame résumé :
data1 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data1) <- c("Jeune","Adulte","Âgée")
View(data1)


# RÉGION 2
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==2)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==2)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==2)
dc_ag
# data frame résumé :
data2 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data2) <- c("Jeune","Adulte","Âgée")
View(data2)

# RÉGION 3
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==3)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==3)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==3)
dc_ag
# data frame résumé :
data3 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data3) <- c("Jeune","Adulte","Âgée")
View(data3)

# RÉGION 4
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==4)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==4)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==4)
dc_ag
# data frame résumé :
data4 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data4) <- c("Jeune","Adulte","Âgée")
View(data4)

# RÉGION 6
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==6)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==6)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==6)
dc_ag
# data frame résumé :
d_6 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_6) <- c("Jeune","Adulte","Âgée")
View(d_6)

# RÉGION 11
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==11)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==6)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==6)
dc_ag
# data frame résumé :
d_11 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_11) <- c("Jeune","Adulte","Âgée")
View(d_11)

# RÉGION 24
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==24)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==24)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==24)
dc_ag
# data frame résumé :
d_24 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_24) <- c("Jeune","Adulte","Âgée")
View(d_24)

# RÉGION 27
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==27)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==27)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==27)
dc_ag
# data frame résumé :
d_27 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_27) <- c("Jeune","Adulte","Âgée")
View(d_27)

# RÉGION 28
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==28)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==28)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==28)
dc_ag
# data frame résumé :
d_28 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_28) <- c("Jeune","Adulte","Âgée")
View(d_28)

# RÉGION 32
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==32)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==32)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==32)
dc_ag
# data frame résumé :
d_32 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_32) <- c("Jeune","Adulte","Âgée")
View(d_32)

# RÉGION 44
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==44)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==44)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==44)
dc_ag
# data frame résumé :
d_44 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_44) <- c("Jeune","Adulte","Âgée")
View(d_44)

# RÉGION 52
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==52)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==52)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==52)
dc_ag
# data frame résumé :
d_52 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_52) <- c("Jeune","Adulte","Âgée")
View(d_52)

# RÉGION 53
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==53)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==53)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==53)
dc_ag
# data frame résumé :
d_53 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_53) <- c("Jeune","Adulte","Âgée")
View(d_53)

# RÉGION 75
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==75)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==75)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==75)
dc_ag
# data frame résumé :
d_75 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_75) <- c("Jeune","Adulte","Âgée")
View(d_75)

# RÉGION 76
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==76)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==76)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==76)
dc_ag
# data frame résumé :
d_76 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_76) <- c("Jeune","Adulte","Âgée")
View(d_76)

# RÉGION 84
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==84)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==84)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==84)
dc_ag
# data frame résumé :
d_84 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_84) <- c("Jeune","Adulte","Âgée")
View(d_84)

# RÉGION 93
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==93)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==93)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==93)
dc_ag
# data frame résumé :
d_93 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_93) <- c("Jeune","Adulte","Âgée")
View(d_93)

# RÉGION 94
# Pour "jeune" :
dc_j <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "jeune" & M_3$DATA.reg==94)
dc_j

# Pour "adulte" :
dc_a <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "adulte" & M_3$DATA.reg==94)
dc_a

# Pour "agée" :
dc_ag <- sum(M_3$DATA.dc & M_3$cl_age90_cut == "agée" & M_3$DATA.reg==94)
dc_ag
# data frame résumé :
d_94 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_94) <- c("Jeune","Adulte","Âgée")
View(d_94)

# TABLE RÉSUMÉ :
x <- c(1,2,3,4,6,11,24,27,28,32,44,52,53,75,76,84,93,94)
df3 <- rbind(data1,data2,data3,data4,d_6,d_11,d_24,d_27,d_28,d_32,d_44
             ,d_52,d_53,d_75,d_76,d_84,d_93,d_94)
# Pour ajouter la colonne des régions 
#df3$Région <- x
#head(df3)
View(df3)

# Graphique :
summary(df3)
jeune <- sum(df3$Jeune)
jeune

adulte <- sum(df3$Adulte)
adulte

agée <- sum(df3$Âgée)
agée

sum <- sum(jeune,adulte,agée)
sum


table <- data.frame(jeune/sum,adulte/sum,agée/sum)
View(table)

DF.TOTAL<-data.frame(Effectifs=1)
View(DF.TOTAL)

table$Total <- DF.TOTAL
View(table)

boxplot(df3,main="Boxplot par tranche d'âge (Mars)", cex.main = 2
, ylab="Nb de décès",xlab ="Tranches d'âge", cex.lab = 1.5, ylim=c(40,250)
,las=1)
pie(table$jeune.sum,table$adulte.sum,table$agée.sum)
barplot()

########## TRAITER LES DONNÉES DU MOIS AVRIL #########

# La table M_4 (toutes les données du AVRIL)
DATA_1$mois4 <- substr(DATA$jour, start = 6, stop = 7)
DATA_1$mois4
M_4 <- DATA_1[DATA_1$mois3 == "04",  ]
M_4
View(M_4)
dim(M_4)
M_4$DATA.dc <- as.numeric(as.character(M_4$DATA.dc))

# Nombre de région possible dans la table M_4 :
length(unique(M_4[,"DATA.reg"]))

# Lister toutes les régions possibles dans la table M_4:
unique(M_4[,"DATA.reg"])

# Choisir une variable à analyser dans la table M_4 :
nom_var<-"cl_age90_cut"
var4<-M_4[,nom_var]

# Effectifs des tranches d'âge dans la table M_4 :
Eff2 <- table(M_4$cl_age90_cut)
Eff2

# Le décompte des données valides :
Nval2 <- sum(!is.na(var4))
Nval2

# Fréquence des classes d'âges :
freq2 <- Eff2/Nval2
freq2

# Fréquence cumulée des classes d'âges :
freqcum2 <- cumsum(freq2)
freqcum2

# Data frame résumé:
df4 <- data.frame(Effectif = Eff2, Fréquence = freq2, Fréquence_cumulée = freqcum2)
View(df4)

# NB DE DÉCÈS PAR TRANCHE D'ÂGE SELON LA RÉGION DANS LA TABLE M_4:
# RÉGION 1
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==1)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==1)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==1)
dc_ag
# data frame résumé :
data1 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data1) <- c("Jeune","Adulte","Âgée")
View(data1)


# RÉGION 2
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==2)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==2)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==2)
dc_ag
# data frame résumé :
data2 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data2) <- c("Jeune","Adulte","Âgée")
View(data2)

# RÉGION 3
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==3)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==3)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==3)
dc_ag
# data frame résumé :
data3 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data3) <- c("Jeune","Adulte","Âgée")
View(data3)

# RÉGION 4
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==4)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==4)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==4)
dc_ag
# data frame résumé :
data4 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data4) <- c("Jeune","Adulte","Âgée")
View(data4)

# RÉGION 6
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==6)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==6)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==6)
dc_ag
# data frame résumé :
data6 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data6) <- c("Jeune","Adulte","Âgée")
View(data6)

# RÉGION 11
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==11)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==11)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==11)
dc_ag
# data frame résumé :
d_11 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_11) <- c("Jeune","Adulte","Âgée")
View(d_11)

# RÉGION 24
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==24)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==24)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==24)
dc_ag
# data frame résumé :
d_242 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_24) <- c("Jeune","Adulte","Âgée")
View(d_24)

# RÉGION 27
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==27)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==27)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==27)
dc_ag
# data frame résumé :
d_27 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_27) <- c("Jeune","Adulte","Âgée")
View(d_27)

# RÉGION 28
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==28)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==28)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==28)
dc_ag
# data frame résumé :
d_28 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_28) <- c("Jeune","Adulte","Âgée")
View(d_28)

# RÉGION 32
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==32)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==32)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==32)
dc_ag
# data frame résumé :
d_32 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_32) <- c("Jeune","Adulte","Âgée")
View(d_32)

# RÉGION 44
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==44)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==44)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==44)
dc_ag
# data frame résumé :
d_44 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_44) <- c("Jeune","Adulte","Âgée")
View(d_44)

# RÉGION 52
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==52)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==52)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==52)
dc_ag
# data frame résumé :
d_52 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_52) <- c("Jeune","Adulte","Âgée")
View(d_52)

# RÉGION 53
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==53)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==53)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==53)
dc_ag
# data frame résumé :
d_53 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_53) <- c("Jeune","Adulte","Âgée")
View(d_53)

# RÉGION 75
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==75)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==75)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==75)
dc_ag
# data frame résumé :
d_75 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_75) <- c("Jeune","Adulte","Âgée")
View(d_75)

# RÉGION 76
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==76)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==76)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==76)
dc_ag
# data frame résumé :
d_76 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_76) <- c("Jeune","Adulte","Âgée")
View(d_76)

# RÉGION 84
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==84)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==84)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==84)
dc_ag
# data frame résumé :
d_84 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_84) <- c("Jeune","Adulte","Âgée")
View(d_84)

# RÉGION 93
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==93)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==93)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==93)
dc_ag
# data frame résumé :
d_93 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_93) <- c("Jeune","Adulte","Âgée")
View(d_93)

# RÉGION 94
# Pour "jeune" :
dc_j <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "jeune" & M_4$DATA.reg==94)
dc_j

# Pour "adulte" :
dc_a <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "adulte" & M_4$DATA.reg==94)
dc_a

# Pour "agée" :
dc_ag <- sum(M_4$DATA.dc & M_4$cl_age90_cut == "agée" & M_4$DATA.reg==94)
dc_ag
# data frame résumé :
d_94 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_94) <- c("Jeune","Adulte","Âgée")
View(d_94)

# TABLE RÉSUMÉE :
x <- c(1,2,3,4,6,11,24,27,28,32,44,52,53,75,76,84,93,94)
df5 <- rbind(data1,data2,data3,data4,data6,d_11,d_24,d_27,d_28,d_32,d_44
             ,d_52,d_53,d_75,d_76,d_84,d_93,d_94)
# Pour ajouter la colonne des régions 
#df5$Région <- x
#head(df5)
View(df5)

# Graphique :
summary(df5)
#boxplot(df3$Jeune,df3$Adulte,df3$Âgée,main="Boxplot par tranche d'âge", cex.main = 2, ylab="Nb de décès",xlab ="Tranches d'âge", cex.lab = 1.5, ylim(0:160),las=1)
boxplot(df5,main="Boxplot par tranche d'âge (Avril)", cex.main = 2
        , ylab="Nb de décès",xlab ="Tranches d'âge", cex.lab = 1.5, ylim=c(50,280),las=1)







########## TRAITER LES DONNÉES DU MOIS MAI #########

# La table M_5 (toutes les données du MAI)
DATA_1$mois5 <- substr(DATA$jour, start = 6, stop = 7)
DATA_1$mois5
M_5 <- DATA_1[DATA_1$mois3 == "05",  ]
M_5
View(M_5)
class(M_5$DATA.dc)
dim(M_5)

# Nombre de région possible dans la table M_5 :
length(unique(M_5[,"DATA.reg"]))

# Lister toutes les régions possibles dans la table M_5:
unique(M_5[,"DATA.reg"])

# Choisir une variable à analyser dans la table M_5:
nom_var<-"cl_age90_cut"
var5<-M_5[,nom_var]

# Effectifs des tranches d'âge dans la table M_5:
Eff3 <- table(M_5$cl_age90_cut)
Eff3

# Le décompte des données valides :
Nval3 <- sum(!is.na(var5))
Nval3

# Fréquence des classes d'âges :
freq3 <- Eff3/Nval3
freq3

# Fréquence cumulée des classes d'âges :
freqcum3 <- cumsum(freq3)
freqcum3

# Data frame résumé:
df6 <- data.frame(Effectif = Eff3, Fréquence = freq3, Fréquence_cumulée = freqcum3)
View(df6)

# NB DE DÉCÈS PAR TRANCHE D'ÂGE SELON LA RÉGION DANS LA TABLE M_5:
# RÉGION 1
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==1)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==1)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==1)
dc_ag
# data frame résumé :
data1 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data1) <- c("Jeune","Adulte","Âgée")
View(data1)


# RÉGION 2
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==2)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==2)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==2)
dc_ag
# data frame résumé :
data2 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data2) <- c("Jeune","Adulte","Âgée")
View(data2)

# RÉGION 3
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==3)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==3)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==3)
dc_ag
# data frame résumé :
data3 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data3) <- c("Jeune","Adulte","Âgée")
View(data3)

# RÉGION 4
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==4)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==4)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==4)
dc_ag
# data frame résumé :
data4 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data4) <- c("Jeune","Adulte","Âgée")
View(data4)

# RÉGION 6
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==6)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==6)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==6)
dc_ag
# data frame résumé :
data6 <- data.frame(dc_j,dc_a,dc_ag)
colnames(data6) <- c("Jeune","Adulte","Âgée")
View(data6)

# RÉGION 11
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==11)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==11)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==11)
dc_ag
# data frame résumé :
d_11 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_11) <- c("Jeune","Adulte","Âgée")
View(d_11)

# RÉGION 24
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==24)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==24)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==24)
dc_ag
# data frame résumé :
d_24 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_24) <- c("Jeune","Adulte","Âgée")
View(d_24)

# RÉGION 27
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==27)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==27)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==27)
dc_ag
# data frame résumé :
d_27 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_27) <- c("Jeune","Adulte","Âgée")
View(d_27)

# RÉGION 28
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==28)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==28)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==28)
dc_ag
# data frame résumé :
d_28 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_28) <- c("Jeune","Adulte","Âgée")
View(d_28)

# RÉGION 32
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==32)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==32)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==32)
dc_ag
# data frame résumé :
d_32 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_32) <- c("Jeune","Adulte","Âgée")
View(d_32)

# RÉGION 44
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==44)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==44)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==44)
dc_ag
# data frame résumé :
d_44 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_44) <- c("Jeune","Adulte","Âgée")
View(d_44)

# RÉGION 52
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==52)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==52)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==52)
dc_ag
# data frame résumé :
d_52 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_52) <- c("Jeune","Adulte","Âgée")
View(d_52)

# RÉGION 53
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==53)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==53)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==53)
dc_ag
# data frame résumé :
d_53 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_53) <- c("Jeune","Adulte","Âgée")
View(d_53)

# RÉGION 75
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==75)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==75)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==75)
dc_ag
# data frame résumé :
d_75 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_75) <- c("Jeune","Adulte","Âgée")
View(d_75)

# RÉGION 76
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==76)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==76)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==76)
dc_ag
# data frame résumé :
d_76 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_76) <- c("Jeune","Adulte","Âgée")
View(d_76)

# RÉGION 84
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==84)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==84)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==84)
dc_ag
# data frame résumé :
d_84 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_84) <- c("Jeune","Adulte","Âgée")
View(d_84)

# RÉGION 93
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==93)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==93)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==93)
dc_ag
# data frame résumé :
d_93 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_93) <- c("Jeune","Adulte","Âgée")
View(d_93)

# RÉGION 94
# Pour "jeune" :
dc_j <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "jeune" & M_5$DATA.reg==94)
dc_j

# Pour "adulte" :
dc_a <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "adulte" & M_5$DATA.reg==94)
dc_a

# Pour "agée" :
dc_ag <- sum(M_5$DATA.dc & M_5$cl_age90_cut == "agée" & M_5$DATA.reg==94)
dc_ag
# data frame résumé :
d_94 <- data.frame(dc_j,dc_a,dc_ag)
colnames(d_94) <- c("Jeune","Adulte","Âgée")
View(d_94)

# TABLE RÉSUMÉE:
x <- c(1,2,3,4,6,11,24,27,28,32,44,52,53,75,76,84,93,94)
df7 <- rbind(data1,data2,data3,data4,data6,d_11,d_24,d_27,d_28,d_32,d_44
             ,d_52,d_53,d_75,d_76,d_84,d_93,d_94)
# Pour ajouter la colonne des régions 
df7$Région <- x
head(df7)
View(df7)

# Graphique :
summary(df7)
boxplot(df7,main="Boxplot par tranche d'âge (Mai)", cex.main = 2
, ylab="Nb de décès",xlab ="Tranches d'âge", cex.lab = 1.5, ylim=c(30,150)
,las=1)


# Boxplot pour 3 mois :
TAB <- data.frame(df3,df5,df7)
TAB
View(TAB)

boxplot(TAB,main="Boxplot par tranche d'âge",cex.main = 2
, ylab="Nb de décès",xlab ="Tranches d'âge", cex.lab = 1.5, ylim=c(30,250)
,las=1, names = c("jM3","aM3","agM3"
              ,"jM4","aM4","agM4"
              ,"jM5","aM5","agM5"))

# Explication des notations :
# jM3 : les jeunes de mois MARS
# aM3 : les adultes de mois MARS
# agM3 : les agées de mois MARS
# jM4 : les jeunes de mois AVRIL
# aM4 : les adultes de mois AVRIL
# agM4 : les agées de mois AVRIL
# jM5 : les jeunes de mois MAI
# aM5 : les adultes de mois MAI
# agM5 : les agées de mois MAI
