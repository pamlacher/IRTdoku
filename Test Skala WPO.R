### Skala WPO: Wissenstest a posteriori ---------------------

# Variablendeklaration Daten
#
# datWPO              Rohdaten aus dem remark
# datWPOheader        Header-Daten (TN-ID, Lehrperson etc.)
# datWPOonly          Nur Item-Daten (mit unterschiedlicher Max. Punktzahl pro Item)
# datWPOnorm          Auf 1 normierte Item-Daten
# datWPOclean         normierte Daten ohne Namen (Zeilennr)
# datWPO_GRM          Für GRM aufbereitete Daten (alle Itemstufen beginnen bei 1 und haben keine Lücke)
# datWPO_FA           Für FA aufbereitete Daten (momentan identisch mit datWPOonly)


#-------------- Initialisierungsblock ----------------------------
# Allgemeine Libraries laden
library(tidyverse)
library(psych)
library(rio)

#-------------- Datenimport und Bereinigung ----------------
datWPO <- rio::import("data/raw/Vorstudie_WPO_alle.sav")

# Datenheader erstellen: Spalten mit Originalzeilennummer OrgRow und Zeilennummer der (durch na.omit) bereinigten Daten CleanRow
# Wird am Ende gebraucht, um die PersonenScores und Noten wieder mit den korrekten TN zu verbinden
datWPOheader <- datWPO %>% na.omit() %>% 
                rownames_to_column(var = "OrgRow") %>% 
                remove_rownames(.) %>% 
                rownames_to_column(var = "CleanRow") %>% 
                select("CleanRow","OrgRow","Lehrperson","TN_ID")

datWPOonly <- datWPO %>% dplyr::select(starts_with("WPO")) %>% na.omit()
# Falls nur eine Lehrperson gewünscht, diese Zeile verwenden:
#datWPOonly <- datWPO %>% filter(Lehrperson=="MAR-EVE") %>% dplyr::select(starts_with("WPO")) %>% na.omit()


# Ändern: Daten nicht "zuschneiden", sondern alles in der Tabelle lassen, damit alles auf TN-ID zurückführbar bleibt
# Berechnungen danach so durchführen:
# testalpha <- datWPO %>% dplyr::select(starts_with("WPO")) %>% na.omit() %>% psych::alpha(check.keys = TRUE)
#

datWPOnorm <- datWPOonly %>% transmute(
  WPO_1_norm = WPO_1 / 6, # Normiere WPO-1, max = 6
  WPO_2_norm = WPO_2 / 1, # Normiere WPO-2, max = 1
  WPO_3_norm = WPO_3 / 2, # Normiere WPO-3, max = 2
  WPO_4_norm = WPO_4 / 12, # Normiere WPO-4, max = 12
  WPO_5_norm = WPO_5 / 2, # Normiere WPO-5, max = 2
  WPO_6_norm = WPO_6 / 3, # Normiere WPO-6, max = 3
  WPO_7_norm = WPO_7 / 1, # Normiere WPO-7, max = 1
  WPO_8_norm = WPO_8 / 1, # Normiere WPO-8, max = 1
  WPO_9_norm = WPO_9 / 2, # Normiere WPO-9, max = 2
  WPO_10_norm = WPO_10 / 1, # Normiere WPO-10, max = 1
  WPO_11_norm = WPO_11 / 2, # Normiere WPO-11, max = 2
  WPO_12_norm = WPO_12 / 2, # Normiere WPO-12, max = 2
  WPO_13_norm = WPO_13 / 2, # Normiere WPO-13, max = 2
)

WPOcalpha <- psych::alpha(datWPOonly,check.keys = TRUE)
WPOnorm_calpha <- psych::alpha(datWPOnorm,check.keys = TRUE)

#Schöne Tabelle darstellen mit formattable
# library(formattable)
# library(knitr)

# formattable(WPOcalpha$total,digits=2)
# formattable(WPOcalpha$alpha.drop,digits=2)
# formattable(WPOcalpha$item.stats,digits=2)
# 
# formattable(WPOnorm_calpha$total,digits=2)
# formattable(WPOnorm_calpha$alpha.drop,digits=2)
# formattable(WPOnorm_calpha$item.stats,digits=2)

#----------- Eindimensionalität testen --------------
# very simple structure und MAP testen
WPOvss <- vss(datWPOnorm)
WPOvss

# Parallelenanalyse
WPOparallel <- fa.parallel(datWPOnorm, fa="both")

# CFA mit Lavaan auf 1 Faktor
library(lavaan)

# Rohdaten
WPO.model <- ' wpo =~ WPO_1 + WPO_2 + WPO_3 + WPO_4 + WPO_5 + 
                      WPO_6 + WPO_7 + WPO_8 + WPO_9 + WPO_10 + 
                      WPO_11 + WPO_12 + WPO_13'

WPO.fit <- cfa(WPO.model,data=datWPOonly)
summary(WPO.fit, fit.measures=TRUE)

# Normierte Daten (Spolier: Kein Unterschied im fit)
WPOnorm.model <- ' wpo =~ WPO_1_norm + WPO_2_norm + WPO_3_norm + WPO_4_norm + WPO_5_norm + 
                      WPO_6_norm + WPO_7_norm + WPO_8_norm + WPO_9_norm + WPO_10_norm + 
                      WPO_11_norm + WPO_12_norm + WPO_13_norm'

WPOnorm.fit <- cfa(WPOnorm.model,data=datWPOnorm)
summary(WPOnorm.fit, fit.measures=TRUE)

# Test auf multivariate Normalverteilung
library(MVN)

MVN::mvn(datWPOnorm, mvnTest = "mardia")
# Resultat: Keine multivariate Normalverteilung!
# $multivariateNormality
# Test        Statistic              p value Result
# 1 Mardia Skewness 985.713291615665 3.09141411002369e-41     NO
# 2 Mardia Kurtosis 4.09695070779561   4.186279983176e-05     NO
# 3             MVN             <NA>                 <NA>     NO
# 
# Deshalb

WPO.model <- ' wpo =~ WPO_1 + WPO_2 + WPO_3 + WPO_4 + WPO_5 + 
                      WPO_6 + WPO_7 + WPO_8 + WPO_9 + WPO_10 + 
                      WPO_11 + WPO_12 + WPO_13'

WPO.fit.mlr <- cfa(WPO.model,data=datWPOonly, estimator = "MLR" )
summary(WPO.fit.mlr, fit.measures=TRUE)
# Resultat: cfa auch nicht erfüllt

# Test mit reduzierten Itemstufen
#
WPOvss <- vss(datWPO_GRM)
WPOvss

# Parallelenanalyse
WPOparallel <- fa.parallel(datWPO_GRM, fa="both")

# CFA mit Lavaan auf 1 Faktor
library(lavaan)

# Rohdaten
WPO.model <- ' wpo =~ WPO_1 + WPO_2 + WPO_3 + WPO_4 + WPO_5 + 
                      WPO_6 + WPO_7 + WPO_8 + WPO_9 + WPO_10 + 
                      WPO_11 + WPO_12 + WPO_13'

WPO.fit <- cfa(WPO.model,data=datWPO_GRM)
summary(WPO.fit, fit.measures=TRUE)

# Normierte Daten (Spolier: Kein Unterschied im fit)
WPOnorm.model <- ' wpo =~ WPO_1_norm + WPO_2_norm + WPO_3_norm + WPO_4_norm + WPO_5_norm + 
                      WPO_6_norm + WPO_7_norm + WPO_8_norm + WPO_9_norm + WPO_10_norm + 
                      WPO_11_norm + WPO_12_norm + WPO_13_norm'

WPOnorm.fit <- cfa(WPOnorm.model,data=datWPOnorm)
summary(WPOnorm.fit, fit.measures=TRUE)

# Test auf multivariate Normalverteilung
library(MVN)

MVN::mvn(datWPOnorm, mvnTest = "mardia")






#----------- GRM (Graded Response Model, mit ltm berechnet) --------------
library(ltm)

datWPOclean <- remove_rownames(datWPOonly)

# Rekodierung der Tabelle datWPOclean so, dass alle Levels bei 1 beginnen und aufsteigend ohne Lücke nummeriert sind
# (Weil ltm::grm nur solche Daten verarbeiten kann)
# siehe:
# https://github.com/drizopoulos/ltm/issues/3
# https://stats.stackexchange.com/questions/49189/error-message-from-grm-in-ltm-subscript-out-of-bounds
#

# Gehe durch alle Items hindurch
for(i in 1:length(datWPOclean)) {
  # Erstelle eine Tabelle aller vorkommenden Levels (Item-Werte)
  lvls_tbl <- table(datWPOclean[i],dnn = "lvls") # table speichert die verschiedenen Levelwerte als Namen und die Anzahl als Wert in diesem Vektor
  Item_lvls <- as.numeric(names(lvls_tbl)) # Nur die Levelwerte, nicht deren Frequenz speichern

  Item_values <- datWPOclean[[i]]       # Item-Werte aus Datentabelle extrahieren
  level_key <- c(1:length(Item_lvls))   # Neue Levels erstellen, Vektor von 1 bis Anzahl alte Levels (length)
  
  Item_recoded <- plyr::mapvalues(Item_values,from = Item_lvls, to = level_key) # Alte Level-Werte durch neue Levelwerte ersetzen
  if(i==1) datWPOord <- tibble(Item_recoded) #Beim ersten Item wird das neue Tibble erstellt
  else datWPOord <- datWPOord %>% bind_cols(Item_recoded) #bei allen weiteren Items die Spalte anhängen
}

# Reduzierung der Itemsstufen nach Ergebnissen eines GRM über das komplette Datenset
#
# Item 1: Nur Stufen 1 und 6 verwenden -> Stufen 2 bis 5 werden zu Stufe 1
# Item 3: Nur Stufen 1 und 3 verwenden -> Stufe 2 wird zu Stufe 1
# Item 4: Nur Stufen 1, 2, 4, 6 und 12 verwenden -> Stufe 3 wird zu Stufe 2, Stufe 5 zu 4, Stufen 7-11 zu 6 
# Item 6: Nur Stufen 1 und 4 verwenden -> Stufen 2 und 3 werden zu Stufe 1
# Item 12: Nur Stufen 1 und 3 verwenden -> Stufen 2 wird zu Stufe 1

names(datWPOord) <- names(datWPOclean) # Namen im neuen Tibble sind identisch mit den alten Namen

datWPO_GRM <- datWPOord %>% 
  mutate(WPO_1 = plyr::mapvalues(datWPOord$WPO_1, c(1, 2, 3, 4, 5, 6), c(1, 1, 1, 1, 1, 2)),
            WPO_3 = plyr::mapvalues(datWPOord$WPO_3, c(1, 2, 3), c(1, 1, 2)),
            WPO_4 = plyr::mapvalues(datWPOord$WPO_4, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), c(1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 5)),
            WPO_6 = plyr::mapvalues(datWPOord$WPO_6, c(1, 2, 3, 4), c(1, 1, 1, 2)),
            WPO_12 = plyr::mapvalues(datWPOord$WPO_12, c(1, 2, 3), c(1, 1, 2))
            )

# GRM mit und ohne constrained ICC (parallele Steigung) durchführen
GRMout <- grm(datWPO_GRM)
GRMout2 <- grm(datWPO_GRM, constrained = TRUE)

ltm::anova.grm(GRMout2,GRMout) # beide Modelle vergleichen
# Personenwerte berechnen
GRMfactorvalues <- ltm::factor.scores(GRMout, resp.patterns=datWPO_GRM,method=c("EB"))
# Personenwerte extrahieren und standardisieren
GRMScores <- scale(GRMfactorvalues$score.dat$z1)
# Daraus die Notenwerte berechnen, wobei gilt: Mittelwert = 4.5, Max = 6, halbe Noten
GRMmarks <- round((4.5 + GRMScores*(1.5/max(GRMScores)))*2,digits = 0)/2

# Verwandle GRMSmarks in einen Faktor mit den Levels von halben Noten (notwendig, damit table() ALLE Notenwerte auflistet, auch solche, die 0 sind)
GRMmarks <- factor(GRMmarks, levels = c(seq(1,6,0.5)))

GRMmarkstbl <- as.data.frame(table(GRMmarks)) # Notenwerte zählen und daraus ein Dataframe machen

plotGRMmarks <- ggplot(GRMmarkstbl, aes(x=GRMmarks, y=Freq, fill=GRMmarks)) + 
  geom_bar(stat="identity") + 
  ylim(0,32) +
  geom_text(aes(label=Freq), vjust=-0.5, size=3.5) + 
  theme(legend.position="none")+
  xlab("Noten")+ylab("Anzahl")+ggtitle("Notenverteilung nach Graded Response Model")

plotGRMmarks

# Polytome ICC zeichnen
# "Drücke Eingabetaste" ausschalten (damit Script komplett durchläuft) -> geht nicht, siehe unten!
par(ask=FALSE)
devAskNewPage(ask = FALSE)
# Alle auf einer Seite, 4x4 Matrix, nach Zeile geordnet
par(mfrow = c(4, 4))
plot(GRMout, items = c(1:13), lwd = 1, cex = 0.8, legend = FALSE, xlab = "Latent Trait")
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt

# Jedes einzeln
# "Drücke Eingabetaste" ausschalten (damit Script komplett durchläuft)
par(ask=FALSE)
par(mfrow = c(1, 1))
devAskNewPage(ask = FALSE)
plot(GRMout)
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 13 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt

# Item Information Curves
# "Drücke Eingabetaste" ausschalten (damit Script komplett durchläuft)
par(ask=FALSE)
devAskNewPage(ask = FALSE)
plot(GRMout, type = "IIC", legend = FALSE, cx = "topright", lwd = 2, cex = 1.4)
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt

# Test Information Function
# "Drücke Eingabetaste" ausschalten (damit Script komplett durchläuft)
par(ask=FALSE)
plot(GRMout, type = "IIC", items = 0, lwd = 2)
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt

#---------- CFA reduzierte Itemstufen ---------------------

# CFA mit Lavaan auf 1 Faktor
library(lavaan)

# Rohdaten
WPO.model <- ' wpo =~ WPO_1 + WPO_2 + WPO_3 + WPO_4 + WPO_5 + 
                      WPO_6 + WPO_7 + WPO_8 + WPO_9 + WPO_10 + 
                      WPO_11 + WPO_12 + WPO_13'

WPO.fit <- cfa(WPO.model,data=datWPO_GRM)
summary(WPO.fit, fit.measures=TRUE)



#------------- Gewichtete Summe -----------------
# Ermittelung der Notenwerte der SuS durch die gewichtete Summe (=nachvollziehbar)
# Die Gewichtungen wurden empirisch ermittelt, in der Excelmappe WPOscores, Tabelle GRM Gewichtung ausprobieren
# 3	1	2	4	2	1.5	1	1	1	1	1	1	1

datWPOgewSumme <- datWPOnorm %>% transmute(
  WPO_1_g = WPO_1_norm * 3, # gewichtetes WPO-1, max = 3
  WPO_2_g = WPO_2_norm * 1, # gewichtetes WPO-2, max = 3
  WPO_3_g = WPO_3_norm * 2, # gewichtetes WPO-3, max = 3
  WPO_4_g = WPO_4_norm * 4, # gewichtetes WPO-4, max = 3
  WPO_5_g = WPO_5_norm * 2, # gewichtetes WPO-5, max = 3
  WPO_6_g = WPO_6_norm * 1.5, # gewichtetes WPO-6, max = 3
  WPO_7_g = WPO_7_norm * 1, # gewichtetes WPO-7, max = 3
  WPO_8_g = WPO_8_norm * 1, # gewichtetes WPO-8, max = 3
  WPO_9_g = WPO_9_norm * 1, # gewichtetes WPO-9, max = 3
  WPO_10_g = WPO_10_norm * 1, # gewichtetes WPO-10, max = 3
  WPO_11_g = WPO_11_norm * 1, # gewichtetes WPO-11, max = 3
  WPO_12_g = WPO_12_norm * 1, # gewichtetes WPO-12, max = 3
  WPO_13_g = WPO_13_norm * 1 # gewichtetes WPO-13, max = 3
)

# Summe aller gewichteten Items bilden
WPOgewSumme <- rowSums(datWPOgewSumme,na.rm = FALSE) 

# Noten ausrechnen, 6er=18.5 Punkte, halbe Noten runden. 
# Die Notenskala wir durch den Exponenten 0.8 leicht nach oben verbogen (siehe Geogebra-Modellierung dazu)
# Dadurch kommt sie näher an die via ltm::grm ermittelte Notenverteilung heran
GewSmarks <- round((1 + WPOgewSumme^0.8/18.5^0.8*5)*2,digits = 0)/2
GewSmarks <- ifelse(GewSmarks > 6,6,GewSmarks) # Noten >6 auf 6 setzen

# Verwandle GewSmarks in einen Faktor mit den Levels von halben Noten (notwendig, damit table() ALLE Notenwerte auflistet, auch solche, die 0 sind)
GewSmarks <- factor(GewSmarks, levels = c(seq(1,6,0.5)))

GewSmarkstbl <- as.data.frame(table(GewSmarks)) # Notenwerte zählen und daraus ein Dataframe machen

# Darstellung der Notenwerte als Säulendiagramm
# Farbpalette für Farbenblinde (zu wenig Werte, deshalb evtl. spätere Implementation)
# cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plotGewSmarks <- ggplot(GewSmarkstbl, aes(x=GewSmarks, y=Freq, fill=GewSmarks)) + 
  geom_bar(stat="identity") + 
  ylim(0,35) +
  geom_text(aes(label=Freq), vjust=-0.5, size=3.5) + 
  theme(legend.position="none")+
  xlab("Noten")+ylab("Anzahl")+ggtitle("Notenverteilung alle Lernenden nach Punkten und Formel")
#+ scale_fill_manual(values=cbPalette)  #für Farbenblinde

plotGewSmarks

# Gewichtete Punktzahlen, Summe und Noten mit Headerdaten (TN_ID und Lehrperson) vereinen
datWPOgewSumme <- datWPOgewSumme %>% bind_cols(tibble(WPOgewSumme)) # Summe zu den Daten hinzufügen

# rownames_to_column wandelt die interne Zeilennummer der berechneten Noten in eine "offizielle" Spalte um, die der Spalte "CleanRow" in datWPOHeader entspricht
datSuSmarks <- datWPOheader %>% 
  left_join(rownames_to_column(as.data.frame(GewSmarks),var="CleanRow"),by="CleanRow") %>% 
  left_join(rownames_to_column(datWPOgewSumme,var="CleanRow"),by="CleanRow") 
  
# Für jede Klasse eine eigene Exceldatei mit den Noten speichern
# Schleife durch alle Lehrpersonen (ermittelt via count auf die Notendaten)
for (i in 1:nrow(datSuSmarks %>% count(Lehrperson))) {
  # Kürzel der aktuellen Lehrperson ermitteln
  lp <- (datSuSmarks %>% count(Lehrperson))[i, "Lehrperson"]
  # Daten nach dieser Lehrperson filtern und die Spalten Lehrperson, TN_ID und GewSmark (=Noten) und alle Punktzahlen in einem Tabellenblatt, das mit 'Klasse LP' angeschrieben ist, in einer Exceldatei speichern
  rio::export(x=datSuSmarks %>% filter(Lehrperson==lp) %>% dplyr::select(Lehrperson, TN_ID, GewSmarks,starts_with("WPO")),
              file=paste(sep="","Noten ",lp,".xlsx"),
              which = paste("Klasse ",lp)
             )
  # In dieselbe Exceldatei ein zweites Tabellenblatt mit allen Noten (ohne LP) speichern
  rio::export(x=dplyr::select(datSuSmarks, TN_ID, GewSmarks),
              file=paste(sep="","Noten ",lp,".xlsx"),
              which = "Alle Klassen"
             )
}

# Alternative Notenberechnung: Min = 1, Max = 6, halbe Noten
# Falls jemand danach fragt
#
# GRMmarks2 <- round((1 + (GRMScores-min(GRMScores))/(max(GRMScores)-min(GRMScores))*5)*2,digits = 0)/2


#------------ Faktoranalyse ----------------
#Vorbereitung
#Parallelenanalyse, gibt Anzahl Faktoren zurück

items.parallel <- fa.parallel(datWPOonly, fa="both")
#Bartlett-Test, darf man PCA überhaupt anwenden?

cortest.bartlett(datWPOonly,n=100)

#KMO, darf man PCA anwenden? Welche Faktoren ausschliessen?

kmo <- KMO(datWPOonly)
#Items nach Measure of Sampling Adequacy ordnen
kmo$MSAi[order(kmo$MSAi)]

datWPO_FA <- datWPOonly # Momentan kein Ausschluss von Items. %>% select(-"SR1_1",-"SR1_2") #Aufgrund der KMO-Analyse Items mit MSAi<0.6 ausschliessen (WPO_3 wäre 0.56, wird momentan noch drin gelassen)

#Eigentliche PCA
# Gemäss Statistikerin besser fa als PCA durchführen
# hauptkomp <- psych::principal(datWPO_PCA, rotate='none', nfactors=1)
# summary and Chi^2 model test
# print.psych(hauptkomp, cut=0.2, sort=F)

#Faktoranalyse mit 1 Faktor mit fa und max likelihood
WPOfa <- psych::fa(datWPO_FA,nfactors = 1, n.obs = 100, rotate = "none", fm = "ml")
print.psych(WPOfa, cut=0.2, sort=F)

# Faktorwerte berechnen
FAscorevalues <- psych::factor.scores(datWPO_FA,WPOfa,method = "Thurstone")

# Personenwerte extrahieren und standardisieren
FAscores <- scale(FAscorevalues$scores)
# Daraus die Notenwerte berechnen, wobei gilt: Mittelwert = 4.5, Max = 6, halbe Noten
FAmarks <- round((4.5 + FAscores*(1.5/max(FAscores)))*2,digits = 0)/2

# Verwandle GRMSmarks in einen Faktor mit den Levels von halben Noten (notwendig, damit table() ALLE Notenwerte auflistet, auch solche, die 0 sind)
FAmarks <- factor(FAmarks, levels = c(seq(1,6,0.5)))

FAmarkstbl <- as.data.frame(table(FAmarks)) # Notenwerte zählen und daraus ein Dataframe machen

plotFAmarks <- ggplot(FAmarkstbl, aes(x=FAmarks, y=Freq, fill=FAmarks)) + 
  geom_bar(stat="identity") + 
  ylim(0,35) +
  geom_text(aes(label=Freq), vjust=-0.5, size=3.5) + 
  theme(legend.position="none")
plotFAmarks

# Code unten liefert dieselben Werte für alle Methoden, deshalb obsolet
#
# alle Methoden ausprobieren und nebeneinanderstellen
# WPOscoresThurstone <- factor.scores(datWPO_PCA,WPOfa,method = "Thurstone")
# WPOscorestenBerge <- factor.scores(datWPO_PCA,WPOfa,method = "tenBerge")
# WPOscoresAnderson <- factor.scores(datWPO_PCA,WPOfa,method = "Anderson")
# WPOscoresBartlett <- factor.scores(datWPO_PCA,WPOfa,method = "Bartlett")
# WPOscoresHarman <- factor.scores(datWPO_PCA,WPOfa,method = "Harman")
# WPOscorescompontents <- factor.scores(datWPO_PCA,WPOfa,method = "components")
# 
# WPOscoresAll <- bind_cols(WPOscoresThurstone[["scores"]],WPOscorestenBerge[["scores"]],WPOscoresAnderson[["scores"]],WPOscoresBartlett[["scores"]],WPOscoresHarman[["scores"]],WPOscorescompontents[["scores"]])


#------------- GRM mit MIRT berechnen ----------------------
library(mirt)

# MIRTmod <- mirt(datWPO_GRM, 1, itemtype = "graded")

# mirt kodiert selbstständig die Daten zu ordinalen um
MIRTmod <- mirt(datWPOonly, 1, itemtype = "graded")
MIRTscorevalues <- fscores(MIRTmod, method ="ML")


# Noten berechnen
MIRTscores <- scale(MIRTscorevalues) #standardisierte scores nach MIRT-Berechnung

MIRTmarks <- round((4.5 + MIRTscores*(1.5/max(MIRTscores)))*2,digits = 0)/2 #Notenberechnung aus MIRT-Scores


# Verwandle GRMSmarks in einen Faktor mit den Levels von halben Noten (notwendig, damit table() ALLE Notenwerte auflistet, auch solche, die 0 sind)
MIRTmarks <- factor(MIRTmarks, levels = c(seq(1,6,0.5)))

MIRTmarkstbl <- as.data.frame(table(MIRTmarks)) # Notenwerte zählen und daraus ein Dataframe machen

# Diagramm der Notenverteilung aus MIRT-Scores
plotMIRTmarks <- ggplot(MIRTmarkstbl, aes(x=MIRTmarks, y=Freq, fill=MIRTmarks)) + 
  geom_bar(stat="identity") + 
  ylim(0,55) +
  geom_text(aes(label=Freq), vjust=-0.5, size=3.5) + 
  theme(legend.position="none")
plotMIRTmarks

#----------- Alle Plots --------------
# Zeichne alle Plots auf einmal
library(gridExtra)
grid.arrange(plotFAmarks,plotGewSmarks,plotGRMmarks,plotMIRTmarks,nrow=2,ncol=2)

# Streudiagramm für ltm und mirt GRM-Daten zeichnen
datGRMboth <- tibble(GRMScores,MIRTscores)
ggplot(datGRMboth, aes(x=GRMScores, y=MIRTscores)) + 
  geom_point(size=3) +
  geom_smooth(method='lm') +
  xlim(-3.5, 3.5) + ylim(-3.5,3.5)

GRMplots <- list()  # new empty list
MIRTplots <- list()  # new empty list

for(i in 1:13) {

  GRMplots[[i]] <- plot(GRMout, items = i, lwd = 1, cex = 0.8, legend = FALSE, xlab = "Latent Trait")
  dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
  MIRTplots[[i]] <- mirt::itemplot(MIRTmod,i)

}

grid.arrange(GRMplots[1],MIRTplots[1],nrow=1,ncol=2)

#----------- CRM Model Test --------------
# Nicht mehr verwendet

# library(EstCRM)
# 
# ##Define the vectors "max.item" and "min.item". The maximum possible
# ##score was 112 and the minimum possible score was 0 for all items
# 
# max.item <- c(1,1,1,1,1,1,1,1,1,1,1,1,1)
# min.item <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
# 
# ##The maximum number of EM Cycle and the convergence criteria can be
# ##specified
# 
# max.EMCycle=200
# converge=.01
# 
# ##Estimate the item parameters
# CRM <- EstCRMitem(datWPOnorm, max.item, min.item, max.EMCycle, converge)
# CRM
# 
# ##Other details
# 
# CRM$descriptive
# CRM$param
# # CRM$iterations
# # CRM$dif
# 
# par <- CRM$param
# 
# ##Estimate the person parameters
# 
# CRMthetas <- EstCRMperson(datWPOnorm,par,min.item,max.item)
# theta.par <- CRMthetas$thetas
# theta.par
# 
# #------- Test nicht normalisierte Daten
# 
# library(EstCRM)
# 
# ## Maximum und Minimum für jedes Item definieren (13 Items = 13 Werte in je Vektor mx.item und min.item)
# 
# max.item <- c(6,1,2,12,2,3,1,1,2,1,2,2,2)
# min.item <- c(0,0,0,0,0,0,0,0,0,0,0,0,0)
# 
# ##The maximum number of EM Cycle and the convergence criteria
# max.EMCycle=200
# converge=.01
# 
# ## Item-Parameter schätzen
# #  Achtung: Dieser Output wird nur verwendet für die Schätzung der Personenparameter und ist enthält KEINE direkte
# #  Gewichtung der Items (da diese nicht einfach linear kombiniert werden, vgl. Dokumentation EstCRM)
# CRM <- EstCRMitem(datWPOonly, max.item, min.item, max.EMCycle, converge)
# CRM
# 
# ## Deskritive Parameter der CRM
# 
# CRM$descriptive
# CRM$param
# 
# ## Testscore pro Person berechnen
# par <- CRM$param
# 
# CRMthetas <- EstCRMperson(datWPOonly,par,min.item,max.item)
# theta.par <- CRMthetas$thetas
# theta.par
# 
# # Testscore mit den ursprünglichen Daten vereinen
# datWPOscore <- rownames_to_column(datWPO,"Zeilennr")
# datTestscoreZeilennr <- bind_cols(rownames_to_column(datWPOonly,"Zeilennr"),as_tibble(theta.par)) %>%  
#   dplyr::select(Zeilennr,`Theta Est.`)
# datWPOscore <- left_join(datWPOscore,datTestscoreZeilennr,by="Zeilennr")
# #rio::export(datWPOscore,"WPOscore.xlsx")


