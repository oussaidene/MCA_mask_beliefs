##       Library's utilisees          ##

library(tidyverse) 
library("FactoMineR")
library("factoextra")
library("corrplot")
library(explor)

##        Lecture des donnees        ##

# na.strings=c("") : remplacer les chaines de caracteres vides avec NA
data <- read.csv("MaskBeliefs.csv", header = TRUE, na.strings=c("")) 

# Suppression des colones qui ne representent pas des variables qualitatifs
data$Timestamp<- NULL
data$Age<- NULL
data$Restaurant <- NULL

# Affichage des noms de colonnes retenues
colnames(data)

# Affichage du nombre de valeurs manquantes
sum_na <- sum(is.na(data))
print(paste("Le nombre des NAs dans notre dataset est :", sum_na))

# Suppression des NAs
data = na.omit(data) 

# Reaffichage du nombre de valeurs manquantes pour verefication
sum_na_apres <- sum(is.na(data))
print(paste("Le nombre des NAs dans notre dataset est :", sum_na_apres))

##        Etude statistique        ##

# Afficher le tableau des statistiques de la variable Boarding
Boardingstat <- fct_count(data$Boarding , prop = TRUE, sort = TRUE) %>%
  rename("Boarding" = "f", "freq" = "p", "nb" = "n") %>%
  mutate(freq = round(freq, 2))
Boardingstat

# Plotter la distribution de la variable Boarding
ggplot(data.frame(data), aes(x = Boarding)) +
  geom_bar(colour = "black", fill = "#6082B6")+ 
  labs(title = "La distribution de la variable 'Boarding' ") +
  theme(plot.title = element_text(hjust = 0.5, size=17), axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),  axis.title=element_text(size=16))

# Plotter le diagramme de pie chart de la variable Boarding
ggplot(Boardingstat , aes(x="", y= nb, fill= Boarding)) +
  scale_fill_brewer("Blues") + 
  geom_bar(stat="identity", width=1, , color = "black") +
  labs(title = "La distribution de la variable 'Boarding' ") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size=17))


# Afficher le tableau des statistiques de la variable Gender
fct_count(data$Gender , prop = TRUE, sort = TRUE) %>%
  rename("Gender" = "f", "freq" = "p", "nb" = "n") %>%
  mutate(freq = round(freq, 2))


# Plotter la distribution de la variable Gender
ggplot(data.frame(data), aes(x = Gender)) +
  geom_bar(colour = "black", fill = "#6082B6")+ 
  labs(title = "La distribution de la variable 'Gender' ") +
  theme(plot.title = element_text(hjust = 0.5, size=17), axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),  axis.title=element_text(size=16))


# Afficher le tableau des statistiques de la variable ResidentialElder
fct_count(data$ResidentialElder , prop = TRUE, sort = TRUE) %>%
  rename("ResidentialElder" = "f", "freq" = "p", "nb" = "n") %>%
  mutate(freq = round(freq, 2))


# Plotter la distribution de la variable ResidentialElder
ggplot(data.frame(data), aes(x = ResidentialElder)) +
  geom_bar(colour = "black", fill = "#6082B6")+ 
  labs(title = "La distribution de la variable 'ResidentialElder'") +
  theme(plot.title = element_text(hjust = 0.5, size=17), axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),  axis.title=element_text(size=16))


# Afficher le tableau des statistiques de la variable InteractedElder
fct_count(data$InteractedElder , prop = TRUE, sort = TRUE) %>%
  rename("InteractedElder" = "f", "freq" = "p", "nb" = "n") %>%
  mutate(freq = round(freq, 2))


# Plotter la distribution de la variable InteractedElder
ggplot(data.frame(data), aes(x = InteractedElder)) +
  geom_bar(colour = "black", fill = "#6082B6")+ 
  labs(title = "La distribution de la variable 'InteractedElder'") +
  theme(plot.title = element_text(hjust = 0.5, size=17), axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),  axis.title=element_text(size=16))


# Afficher le tableau des statistiques de la variable PreventSpread
fct_count(data$PreventSpread , prop = TRUE, sort = TRUE) %>%
  rename("PreventSpread" = "f", "freq" = "p", "nb" = "n") %>%
  mutate(freq = round(freq, 2))


# Plotter la distribution de la variable PreventSpread
ggplot(data.frame(data), aes(x = PreventSpread)) +
  geom_bar(colour = "black", fill = "#6082B6")+ 
  labs(title = "La distribution de la variable 'PreventSpread'") +
  theme(plot.title = element_text(hjust = 0.5, size=17), axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),  axis.title=element_text(size=16))


# Afficher le tableau des statistiques de la variable Reason
Reasonstat <- fct_count(data$Reason , prop = TRUE, sort = TRUE) %>%
  rename("Reason" = "f", "freq" = "p", "nb" = "n") %>%
  mutate(freq = round(freq, 2))
Reasonstat 

# Plotter la distribution de la variable Reason
ggplot(data.frame(data), aes(x = Reason)) +
  geom_bar(colour = "black", fill = "#6082B6")+ 
  labs(title = "La distribution de la variable 'Reason' ") +
  theme(plot.title = element_text(hjust = 0.5, size=17), axis.text.x= element_text(angle = 60, hjust = 1, size=10),
        axis.text.y= element_text(size=14),  axis.title=element_text(size=16))

# Plotter le diagramme de pie chart de la variable Reason
ggplot(Reasonstat , aes(x="", y= nb, fill= Reason)) +
  scale_fill_brewer("Blues") + 
  geom_bar(stat="identity", width=1, , color = "black") +
  labs(title = "La distribution de la variable 'Reason' ") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size=17))


# Afficher le tableau des statistiques de la variable Public
fct_count(data$Public , prop = TRUE, sort = TRUE) %>%
  rename("Public" = "f", "freq" = "p", "nb" = "n") %>%
  mutate(freq = round(freq, 2))


# Plotter la distribution de la variable Public
ggplot(data.frame(data), aes(x = Public)) +
  geom_bar(colour = "black", fill = "#6082B6")+ 
  labs(title = "La distribution de la variable 'Public'") +
  theme(plot.title = element_text(hjust = 0.5, size=17), axis.text.x= element_text(size=14),
        axis.text.y= element_text(size=14),  axis.title=element_text(size=16))


##    Le tableau disjonctif complet   ##

# Rendre le type des colonnes comme factor
data<- data %>%
  mutate(Boarding = factor(Boarding),
         Gender = factor(Gender),
         ResidentialElder = factor(ResidentialElder),
         InteractedElder = factor(InteractedElder),
         PreventSpread = factor(PreventSpread),
         Reason = factor(Reason),
         Public = factor(Public))




# Tableau disjonctif complet de data
Z <- tab.disjonctif(data)
head(Z)
colnames(Z)

##       Application de l'AFCM       ##


# Appliquer l'AFCM sur le tableau de donnee (data)
afcm <- MCA(data)

# Afficher le tableau des valeurs propres issue de notre AFCM
afcm$eig


# Representation de l'eboulis des valeurs propores
fviz_eig(afcm)

# Tableau de valeurs propres corrigÃ©
p = ncol(data) # Le nombre de questions 

# Correction des valeurs propres
eigs_corr = subset(afcm$eig, afcm$eig[,"eigenvalue"]> 1/p)
eigs_corr[,"eigenvalue"] = ((p/(p-1))*(eigs_corr[,"eigenvalue"] - (1/p)))**2

# Calcul de l'inertie des nouvelle valeur propore
eigs_corr[,"percentage of variance"] = (eigs_corr[,"eigenvalue"] / sum(eigs_corr[,"eigenvalue"]) * 100)

# Calcul de l'inertie cummulÃ©e
eigs_corr[1, "cumulative percentage of variance"] = eigs_corr[1, "percentage of variance"]
for(i in 2:nrow(eigs_corr)) {
  eigs_corr[i, "cumulative percentage of variance"] = eigs_corr[i-1, "cumulative percentage of variance"] + eigs_corr[i, "percentage of variance"]
}

# Affichage de l'eboulie des valeurs propres corrigÃ©es
eig.val <- eigs_corr
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

# Illustration des modalites dans ler plan factoriel 
plot(afcm, 
     invisible = c("ind", "quali.sup", "quanti.sup"),
     cex = 0.8,
     autoLab = "yes")

# Illustration du biplot individus-modalites
fviz_mca_biplot(afcm, repel = TRUE,
                ggtheme = theme_minimal())


# Afficher le tableau des projections des modalitÃ©s issue de notre AFCM
afcm$var$coord 

# Afficher le tableau des contributions issue de notre AFCM
afcm$var$contrib

# Afficher les poids des modalites
afcm$call$marge.col

# Afficher le tableau des contributions relatives des modalitÃ©s issue de notre AFCM 
afcm$var$cos2

# Afficher le tableau des projections des modalités issue de notre AFCM 
afcm$var$eta2

# Visialiser les individus coloré par rapport la variable public 
explor(afcm)



##          Visualisations            ##

# Illustration de la qualitÃ© de representation des modalitÃ©s
fviz_mca_var(afcm, col.var = "cos2",
             gradient.cols = c("#00AFBB", "blue", "red"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

# Illustration de la contribution absolue des modalitÃ©s
fviz_mca_var(afcm, col.var = "contrib",
             gradient.cols = c("#00AFBB", "blue", "red"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

# Illustration de a qualitÃ© de representation des individus
fviz_mca_ind(afcm, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "blue", "red"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

# Illustration de la contribution absolue des individus
fviz_mca_ind(afcm, col.ind = "contrib", 
             gradient.cols = c("#00AFBB", "blue", "red"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

# Illustration de la diffÃ©rence entre les contribution absolues et les poids des modalitÃ©s

corrplot(afcm$var$contrib - 100*afcm$call$marge.col, is.corr=FALSE, method = 'color', tl.col="black", col = COL2('RdBu', 200) )

# Illustration des contribution relatives des modalitÃ©s
corrplot(afcm$var$cos2, is.corr=FALSE, method = 'color', tl.col="black" )


# Tableau de contingence entre la variable Public et Reason
N = t(Z[, 12:16]) %*%  Z[, 17:18] 
N

# Calcul de la distance de khi 2 entre Public et Reason
khisq <- chisq.test(N)
khisq

# Application de l'AFC sur le tableau de contigence
afc <- CA(N)

# Tableau des valeurs propres
afc$eig

# Tableaux des projections

# Tableau des projections de profils lignes
afc$row$coord

# Tableau des projections de profils colonnes
afc$col$coord


# Tableaux des contributions

# Tableau des contributions de profils lignes
afc$row$contrib

# Tableau des contributions de profils colonnes
afc$col$contrib


# Poids

# Poids des profils lignes 
afc$call$marge.row

# Poids des profils colonnes
afc$call$marge.col



# ************************* #
# Reprepsentation graphique de l'AFC
Z2 = Z[, 17:18] 
Z2 <- data.frame(Z2)
# Comme la somme est lineairement depandante elle ne va pas influer les resultats
Z2$` ` = (Z2$No + Z2$Yes)
N =  t(Z[, 12:16]) %*% as.matrix(Z2)

# Application de l'AFC sur le nouveau tableau de contigence
afc <- CA(N)
afc$col$coord = -afc$col$coord
afc$row$coord = -afc$row$coord
fviz_ca_biplot(afc) + ylim (-3, 3) +  xlim(-1, 0.3)
fviz_ca(afc, repel = TRUE) + ylim (-3, 3) +  xlim(-1.5, 0.3)




#         AFCM ajustee      #


# Afficher qui a choisit la modalitee « To protect others but also because I'm required to »
data[data$Reason == "To protect others but also because I'm required to",]

# Supprimer la donnee aberante 
data <- subset(data, data$Reason != "To protect others but also because I'm required to")

# Verfier que la donnee aberante a etait supprime
data[data$Reason == "To protect others but also because I'm required to",]


# Appliquer l'AFCM sur le nouveau tableau de donnée
afcm_ajustee <- MCA(data)

# Afficher le tableau des valeurs propres issue de notre AFCM
afcm_ajustee$eig

# Representation de l'eboulis des valeurs propores
fviz_eig(afcm_ajustee)

# Tableau de valeurs propres corrigees
p = ncol(data) # Le nombre de questions 

# Correction des valeurs propres
eigs_corr = subset(afcm_ajustee$eig, afcm_ajustee$eig[,"eigenvalue"]> 1/p)
eigs_corr[,"eigenvalue"] = ((p/(p-1))*(eigs_corr[,"eigenvalue"] - (1/p)))**2

# Calcul de l'inertie des nouvelle valeur propore
eigs_corr[,"percentage of variance"] = (eigs_corr[,"eigenvalue"] / sum(eigs_corr[,"eigenvalue"]) * 100)

# Calcul de l'inertie cummulÃ©e
eigs_corr[1, "cumulative percentage of variance"] = eigs_corr[1, "percentage of variance"]
for(i in 2:nrow(eigs_corr)) {
  eigs_corr[i, "cumulative percentage of variance"] = eigs_corr[i-1, "cumulative percentage of variance"] + eigs_corr[i, "percentage of variance"]
}

eigs_corr

# Affichage de l'eboulie des valeurs propres corrigÃ©es
eig.val <- eigs_corr
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

# Illustration des modalites dans ler plan factoriel 
plot(afcm_ajustee, 
     invisible = c("ind", "quali.sup", "quanti.sup"),
     cex = 0.8,
     autoLab = "yes")

# Illustration du biplot individus-modalites
fviz_mca_biplot(afcm_ajustee, repel = TRUE,
                ggtheme = theme_minimal())


# Afficher le tableau des projections des modalitees issue de notre AFCM
afcm_ajustee$var$coord 

# Afficher le tableau des contributions issue de notre AFCM
afcm_ajustee$var$contrib

# Afficher les poids des modalites
afcm_ajustee$call$marge.col

# Afficher le tableau des contributions relatives des modalitÃ©s issue de notre AFCM 
afcm_ajustee$var$cos2

