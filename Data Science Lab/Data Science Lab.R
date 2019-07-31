# Libraries
library(plyr)
library(dplyr)
library(caret)
library(funModeling)
library(VIM)
library(PerformanceAnalytics)
library(rpart)
library(rpart.plot)
library(MASS)
library(corrplot)
library(klaR)

options(warn=-1)

# wd and datast
setwd('C:\\path')
abSportivo = read.csv("DatasetNoMissing.csv", sep = ";", quote = "", header = TRUE)

# Rename variables
names(abSportivo)
nm = c("respondent_id", "Sesso", "Anno", "Prov", "Naz", "StudioLavoro", "Infortuni", "VicinanzaScuola", "KmScuola", 
       "MaiFattoSport", "VivevanoAllenavano", "Arrampicata", "ArtiMarziali", "Atletica", "BalliDaSala", "Baseball", 
       "Basket", "Calcio", "Canottaggio", "Ciclismo", "Danza", "Equitazione", "FootballAmericano", "Ginnastica",
       "Golf", "Hockey", "Nuoto", "Pallavolo", "Pattinaggio","PingPong", "Rubgy", "Scherma", "Sci", "Skateboard", 
       "SportDaCombattimento", "Tennis", "TiroConArco", "Tuffi", "Yoga", "Altro", "SportSignificativo", "Crocetta", 
       "Agonistico", "GiorniSettimana", "ExtraSport")
nm2 = paste0("FattoriContinuare", 1:34)
nm = append(nm, nm2)
nm3 = c("ImpegnoScuolaSport", "OreStudio", "MammaLavora", "ProfessioneMamma", "OreLavoroMamma", "PapaLavora",
        "ProfessionePapa", "OreLavoroPapa", "FratelliSport", "InfluenzaScelta", "PortareAllenamento",
        "SpigonoContinuare", "ConvincereAllenarsi", "ComportamentoGenitore", "DivertimentoAllenamento")
nm4 = paste0("SportEra", 1:11)
nm = append(nm, nm3)
nm = append(nm, nm4)

names(abSportivo) = nm
abSportivo = abSportivo[, -106] # Remove counter NAs variable



# Check for (quasi)-null variance variables
varCheck = nearZeroVar(abSportivo, saveMetrics = TRUE)
varCheck = cbind(varCheck, nm)
sortVarCheck = varCheck[order(varCheck$percentUnique, decreasing = FALSE), ]
sortVarCheck = sortVarCheck[which(sortVarCheck$nzv == FALSE), ]

# Remove null variance variables
abSportivo = abSportivo[, -which(!(names(abSportivo) %in% sortVarCheck[, 5]))]



#class.tree = rpart(Crocetta ~ ., data = mower.df, method = "class")

# Distribution analysis and creation of a binary target
table(abSportivo$Crocetta)
abSportivo$Crocetta[abSportivo$Crocetta == 1] = 1
abSportivo$Crocetta[abSportivo$Crocetta == 2] = 1
abSportivo$Crocetta[abSportivo$Crocetta == 3] = 0


# Variables type
str(abSportivo)


## Switching to factor or integer
abSportivo$Crocetta = as.factor(abSportivo$Crocetta)
abSportivo$VicinanzaScuola = as.factor(abSportivo$VicinanzaScuola)
abSportivo$VivevanoAllenavano = as.factor(abSportivo$VivevanoAllenavano)

for (i in 9:24) {
  abSportivo[, i] = as.factor(abSportivo[, i])
}

abSportivo$Crocetta = as.factor(abSportivo$Crocetta)
abSportivo$Agonistico = as.factor(abSportivo$Agonistico)

for (i in 31:42) {
  abSportivo[, i] = as.factor(abSportivo[, i])
}

abSportivo$ImpegnoScuolaSport = as.factor(abSportivo$ImpegnoScuolaSport)

for (i in 45:49) {
  abSportivo[, i] = as.factor(abSportivo[, i])
}

for (i in 51:66) {
  abSportivo[, i] = as.factor(abSportivo[, i])
}
str(abSportivo)


abSportivo$Prov = revalue(abSportivo$Prov, c("Aquila"="L'Aquila"))
abSportivo$Prov = revalue(abSportivo$Prov, c("Bari "="Bari"))
abSportivo$Prov = revalue(abSportivo$Prov, c("La spezia "="La Spezia"))
abSportivo$Prov = revalue(abSportivo$Prov, c("salerno"="Salerno"))


provNordEst = c("Bologna", "Ferrara", "Forlì-Cesena", "Gorizia", "Padova", "Parma", "Pordenone",  "Ravenna", 
                "Reggio Emilia", "Rimini", "Rovigo", "Trento", "Trieste", "Udine", "Venezia", "Verona")
provNordOvest = c("Alessandria", "Aosta", "Asti", "Bergamo", "Biella", "Brescia", "Cuneo", "Genova", "La Spezia ",
                  "Milano", "Pavia", "Savona", "Torino", "Varese", "Vecelli")
provCentro = c("Ancona", "Arezzo", "Fermo", "Firenze", "Grosseto", "Lucca", "Macerata", "Perugia",
               "Pesaro e Urbino", "Pistoia", "Prato", "Rieti", "Roma", "Siena", "Viterbo")
provSudIsole = c("Agrigento", "L'Aquila", "Bari", "Benevento", "Cagliari", "Caltanissetta", "Campobasso", "Caserta", 
                 "Catania", "Foggia", "Isernia", "Matera", "Messina", "Napoli", "Oristano", "Palermo", "Potenza",
                 "Ragusa", "Reggio Calabria", "salerno", "Siracusa", "Taranto", "Trapani")
for (i in provNordEst) {
  print(i)
  abSportivo$Prov[abSportivo$Prov == i] = "Nord-Est"
}
abSportivo$Prov[abSportivo$Prov == "Agrigento"] = "Nord-Ovest"

# New dataframe with factor only
isfactor = sapply(abSportivo, function(x) is.factor(x))
abSportivoFactor <- abSportivo[, isfactor]
plot_num(abSportivo[, 2:66] , bins = 5)

# New dataframe with integer only
isint = sapply(abSportivo, function(x) is.integer(x))
abSportivoInt <- abSportivo[, isint]


naive = NaiveBayes(Crocetta ~ ., data = abSportivoFactor)
naive$apriori 
naive$tables$STUDIO  
naive$tables  
naive$tables$Q  
pred = predict(naive, abSportivoFactor, type="class")
confusionMatrix(pred$class, abSportivoFactor$Crocetta)
naive2 <- NaiveBayes(Crocetta ~ ., data = abSportivoFactor, laplace = 50)