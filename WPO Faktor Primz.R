### Skala WPO: Wissenstest a posteriori ---------------------
# 
# 7.12.20 lam
#
# In dieser Datei werden die WPO-Daten nach der Bereinigung gemäss GRM-Analyse in den Itemstufen reduziert
# und auf die Items WPO-7 bis WPO-13 reduziert (Faktor Primzahl)
# Danach werden die Faktorscores mit unterschiedlichen Methoden berechnet und verglichen
#
### ----------------------------------------------------------

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
# Alle Libraries laden
library(psych)
library(rio)
library(MVN)
library(ltm)
library(tidyverse)

#-------------- Datenimport und Bereinigung ----------------
datWPO <- rio::import("data/raw/Vorstudie_WPO_alle.sav")

# Datenheader erstellen: Spalten mit Originalzeilennummer OrgRow und Zeilennummer der (durch na.omit) bereinigten Daten CleanRow
# Wird am Ende gebraucht, um die PersonenScores und Noten wieder mit den korrekten TN zu verbinden
datWPOheader <- datWPO %>% na.omit() %>% 
                rownames_to_column(var = "OrgRow") %>% 
                remove_rownames(.) %>% 
                rownames_to_column(var = "CleanRow") %>% 
                dplyr::select("CleanRow","OrgRow","Lehrperson","TN_ID")

datWPOonly <- datWPO %>% 
              dplyr::select("WPO_7","WPO_8","WPO_9","WPO_7","WPO_10","WPO_11","WPO_12","WPO_13") %>% 
              na.omit()

# Falls nur eine Lehrperson gewünscht, diese Zeile verwenden:
#datWPOonly <- datWPOonly %>% filter(Lehrperson=="MAR-EVE")

datWPOnorm <- datWPOonly %>% transmute(
  WPO_7_norm = WPO_7 / 1, # Normiere WPO-7, max = 1
  WPO_8_norm = WPO_8 / 1, # Normiere WPO-8, max = 1
  WPO_9_norm = WPO_9 / 2, # Normiere WPO-9, max = 2
  WPO_10_norm = WPO_10 / 1, # Normiere WPO-10, max = 1
  WPO_11_norm = WPO_11 / 2, # Normiere WPO-11, max = 2
  WPO_12_norm = WPO_12 / 2, # Normiere WPO-12, max = 2
  WPO_13_norm = WPO_13 / 2, # Normiere WPO-13, max = 2
)

# Daten für GRM vorbereiten: Tabelle rekodieren und Stufen reduzieren

# Rekodierung der Tabelle datWPOclean so, dass alle Levels bei 1 beginnen und aufsteigend ohne Lücke nummeriert sind
# (Weil ltm::grm nur solche Daten verarbeiten kann)
# siehe:
# https://github.com/drizopoulos/ltm/issues/3
# https://stats.stackexchange.com/questions/49189/error-message-from-grm-in-ltm-subscript-out-of-bounds

datWPOclean <- remove_rownames(datWPOonly)

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
# Item 12: Nur Stufen 1 und 3 verwenden -> Stufen 2 wird zu Stufe 1

names(datWPOord) <- names(datWPOclean) # Namen im neuen Tibble sind identisch mit den alten Namen

datWPO_GRM <- datWPOord %>% 
  mutate(
          WPO_12 = plyr::mapvalues(datWPOord$WPO_12, c(1, 2, 3), c(1, 1, 2))
        )

#----------- Eindimensionalität testen --------------

# Cronbachs Alpha
WPOcalpha <- psych::alpha(datWPO_GRM,check.keys = TRUE)

#Schöne Tabelle darstellen mit formattable
# library(formattable)
# library(knitr)

# formattable(WPOcalpha$total,digits=2)
# formattable(WPOcalpha$alpha.drop,digits=2)
# formattable(WPOcalpha$item.stats,digits=2)

# very simple structure und MAP testen
WPOvss <- vss(datWPO_GRM)
WPOvss

# Parallelenanalyse
WPOparallel <- fa.parallel(datWPO_GRM, fa="both")

# CFA mit Lavaan auf 1 Faktor
library(lavaan)

# Rohdaten
WPO.model <- ' wpo =~ WPO_7 + WPO_8 + WPO_9 + WPO_10 + WPO_11 + WPO_12 + WPO_13'

WPO.fit <- cfa(WPO.model,data=datWPO_GRM)
summary(WPO.fit, fit.measures=TRUE)

# Test auf multivariate Normalverteilung
library(MVN)

MVN::mvn(datWPOnorm, mvnTest = "mardia")
# Resultat: Keine multivariate Normalverteilung!
# $multivariateNormality
# Test          Statistic              p value Result
# 1 Mardia Skewness   203.206346130144 7.12694480575304e-12     NO
# 2 Mardia Kurtosis -0.792606067774516    0.428007372397103    YES
# 3             MVN               <NA>                 <NA>     NO
# 
# Deshalb

WPO.fit.mlr <- cfa(WPO.model,data=datWPOonly, estimator = "MLR" )
summary(WPO.fit.mlr, fit.measures=TRUE)


#----------- GRM (Graded Response Model, mit ltm berechnet) --------------

# GRM mit und ohne constrained ICC (parallele Steigung) durchführen
GRMout <- grm(datWPO_GRM)
GRMout2 <- grm(datWPO_GRM, constrained = TRUE)

ltm::anova.grm(GRMout2,GRMout) # beide Modelle vergleichen
# Personenwerte berechnen
GRMfactorvalues <- ltm::factor.scores(GRMout, resp.patterns=datWPO_GRM,method=c("EB"))
# Personenwerte extrahieren und standardisieren
GRMScores <- scale(GRMfactorvalues$score.dat$z1)

# Polytome ICC zeichnen
# "Drücke Eingabetaste" ausschalten (damit Script komplett durchläuft) -> geht nicht, siehe unten!
par(ask=FALSE)
devAskNewPage(ask = FALSE)
# Alle auf einer Seite, 4x4 Matrix, nach Zeile geordnet
par(mfrow = c(4, 2))
plot(GRMout, items = c(1:7), lwd = 1, cex = 0.8, legend = FALSE, xlab = "Latent Trait")
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt

# Jedes einzeln
# "Drücke Eingabetaste" ausschalten (damit Script komplett durchläuft)
par(ask=FALSE)
par(mfrow = c(1, 1))
devAskNewPage(ask = FALSE)
plot(GRMout)
dummy <- 1 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 2 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 3 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 4 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 5 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 6 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt
dummy <- 7 # Notwendig, weil plot nach Eingabetaste fragt und sich das einfach nicht abstellen lässt

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


#------------- GRM mit MIRT berechnen ----------------------
library(mirt)

MIRTmod <- mirt(datWPO_GRM, 1, itemtype = "graded")
MIRTscorevalues <- fscores(MIRTmod)
#MIRTscorevalues <- fscores(MIRTmod, method ="ML")

MIRTscores <- scale(MIRTscorevalues) #standardisierte scores nach MIRT-Berechnung


#------------ Faktoranalyse ----------------
#Vorbereitung

cortest.bartlett(datWPO_GRM,n=100)

#KMO, darf man PCA anwenden? Welche Faktoren ausschliessen?

kmo <- KMO(datWPO_GRM)
#Items nach Measure of Sampling Adequacy ordnen
kmo$MSAi[order(kmo$MSAi)]

datWPO_FA <- datWPO_GRM # Momentan kein Ausschluss von Items

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

#----------- Plots --------------
# Zeichne alle Plots auf einmal

# library(gridExtra)
# grid.arrange(plotFAmarks,plotGewSmarks,plotGRMmarks,plotMIRTmarks,nrow=2,ncol=2)

# Streudiagramm für GRM-Daten (ltm) und FA zeichnen
datGRMboth <- tibble(GRMScores,FAscores, MIRTscores)
ggplot(datGRMboth, aes(x=GRMScores, y=FAscores)) + 
  geom_point(size=3) +
  geom_smooth(method='lm') +
  xlim(-3.5, 3.5) + ylim(-3.5,3.5)

ggplot(datGRMboth, aes(x=GRMScores, y=MIRTscores)) + 
  geom_point(size=3) +
  geom_smooth(method='lm') +
  xlim(-3.5, 3.5) + ylim(-3.5,3.5)

ggplot(datGRMboth, aes(x=MIRTscores, y=FAscores)) + 
  geom_point(size=3) +
  geom_smooth(method='lm') +
  xlim(-3.5, 3.5) + ylim(-3.5,3.5)
