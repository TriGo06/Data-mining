

install.packages(setdiff(c("ggplot2", "MASS", "scatterplot3d", "dplyr", "tree", "factoextra", "FSelectorRcpp", "cluster", "missForest", "dbscan", "tsne", "NbClust", "dendextend", "gridExtra", "caret", "rpart", "C50", "randomForest", "kknn", "ROCR", "e1071", "naivebayes", "nnet", "stats", "smotefamily", "pROC", "MLmetrics", "brnn", "RSNNS", "corrplot", "LiblineaR", "kernlab", "monmlp", "neuralnet"), installed.packages()[, "Package"]))


lapply(c("ggplot2", "MASS", "scatterplot3d", "dplyr", "tree", "factoextra", "FSelectorRcpp", "cluster", "missForest", "dbscan", "tsne", "NbClust", "dendextend", "gridExtra", "caret", "rpart", "C50", "randomForest", "kknn", "ROCR", "e1071", "naivebayes", "nnet", "stats", "smotefamily", "pROC", "MLmetrics", "brnn", "RSNNS", "corrplot", "LiblineaR", "kernlab", "monmlp", "neuralnet"), library, character.only = TRUE)


####################
library(ggplot2)
library(MASS)
library(scatterplot3d)
library(dplyr)
library(tree)
library(factoextra)
library(FSelectorRcpp)
library(cluster)
library(missForest)
library(dbscan)
library(tsne)
library(NbClust)
library(dendextend)
library(gridExtra)
library(caret)
library(rpart)
library(C50)
library(randomForest)
library(kknn)
library(ROCR)
library(e1071)
library(naivebayes)
library(nnet)
library(stats)
library(smotefamily)
library(pROC)
library(MLmetrics)
library(brnn)    
library(RSNNS)
####################


#Tableau génerer par IA attention aux erreurs /!\
# ================================
# Description des variables
# ================================
# Variable       | Type      | Description                              | Domaine de valeurs                   | Valeurs manquantes
# ------------------------------------------------------------------------------------------------------------
# client         | Entier    | Numéro d'identification du client        | [1201, 8500]                         | -
# age            | Entier    | Âge en nombre d'années                   | [18, 999]                            | 999
# education      | Ordinal   | Niveau d'éducation relativement au bac   | Niveau bac, Bac+2, Bac+3, Bac+4,     | -
#               |           |                                           | Bac+5 et plus                        |
# emploi         | Entier    | Nombre d'années avec l'employeur actuel  | [0, 63]                              | -
# categorie      | Entier    | Catégorie bancaire                       | [12, 12]                             | -
# adresse        | Entier    | Nombre d'années à l'adresse actuelle     | [0, 999]                             | 999
# revenus        | Réel      | Revenus du foyer en milliers de $        | [12.3, 2461.7]                       | -
# debcred        | Réel      | Ratio Débit/Crédit (x100)                | [0.08, 44.62]                        | -
# debcarte       | Réel      | Débit carte de crédit en milliers de $   | [0.005, 139.580]                     | -
# autres         | Réel      | Autres dettes en milliers de $           | [0.009, 416.517]                     | -
# default        | Booléen   | Un défaut de paiement a-t-il eu lieu ?   | Oui, Non                             | -

# ================================
# Fichiers de données
# ================================
# Fichier            | Nombre d'instances | Classe ? | Remarques
# --------------------------------------------------------------
# projet.csv         | 6000               | Oui      | Instances dont la classe réelle est connue
# projet_new.csv     | 500                | Non      | Instances à prédire
# ================================
#classe positive : défaut = Oui 
#classe négative : défaut = Non
# ================================



# ================================
#premier traitement des donées
# ================================
data <- read.csv("projet.csv",dec =".", sep = ",", header = TRUE, stringsAsFactors = T)
data <- data[,-1] # On supprime la colonne client inutile pour la prédiction
data$defaut <- as.factor(data$defaut)
table(data$categorie) # 1 seule catégorie 12 donc on peut supprimer cette colonne inutile pour la prédiction
data <- data[,-4]

table(data$adresse)
table(data$age)
str(data)

data$defaut <- as.factor(data$defaut)
table(data$defaut)
pie(table(data$defaut)) # environ 38% de défaut ce qui n'est pas très déséquilibré par rapport au problème étudié
#supprimer une valeur aberrante
data <- data[data$revenus != 2461.7,] # cette ligne peut etre interprété incorrectement par les algorithmes, tout les revenus extreme pourrait etre considéré comme défaut = Oui.

#gestion des valeurs manquantes
data$age[data$age == 999] <- NA
data$adresse[data$adresse == 999] <- NA

data$education <- ifelse(data$education == "Niveau bac", 1,
                         ifelse(data$education == "Bac+2", 2,
                                ifelse(data$education == "Bac+3", 3,
                                       ifelse(data$education == "Bac+4", 4,
                                              ifelse(data$education == "Bac+5 et plus", 5, NA)))))

summary(data)


# ================================
#Analyse exploratoire des données
# ================================
qplot(education,debcred,data=data,color=defaut) + geom_jitter(width=0.3) +geom_point(alpha=0.5)
#on remarque que plus le niveau de debcred est élévé plus la proportion de points defaut oui augmente mais qu'il qu'il n'y a
#pas a prirori de lien entre education et défaut

ggplot(data, aes(x=education, fill=defaut)) + geom_bar(position = "fill")
#on remarque que la proportion de défaut selon l'age augmente avec le niveau d'éducation jusqu'a bac+4 puis diminue a un niveau equivalent a bac+3
ggplot(data, aes(x=age, fill=defaut)) + geom_bar(position = "fill")

qplot(emploi,age,data=data,color=defaut) + geom_jitter(height=0.5) +geom_point(alpha=0.5)
qplot(adresse,age,data=data,color=defaut) + geom_jitter(height=0.5) +geom_point(alpha=0.5)
#premiere remarque logique plus on est agé plus on a un emploi stable et également une adresse stable
#la proportion de point defaut oui diminue avec l'age , adresse et emploi
#ces trois variables semblent corélés et nous donne des informations similaires


scatterplot3d(x = data$debcred,y = data$debcarte,z = data$education,pch=20,angle = 40,grid = TRUE, color = ifelse(data$defaut == "Oui", "green", "red"))




set.seed(12345)
imputé_mf <- missForest(data)

data_mf <- imputé_mf$ximp

imputé_mf$OOBerror
str(data_mf)


importance <- function(data, title = "") {
  # Calcul de l'Information Gain
  info_gain <- information_gain(defaut ~ ., data = data)
  info_gain <- info_gain[order(info_gain$importance), ]
  
  # Calcul du Gain Ratio
  gain_ratio <- information_gain(defaut ~ ., data = data, type = "gainratio")
  gain_ratio <- gain_ratio[order(gain_ratio$importance), ]
  
  # Calcul de Symmetrical Uncertainty
  symm_uncertainty <- information_gain(defaut ~ ., data = data, type = "symuncert")
  symm_uncertainty <- symm_uncertainty[order(symm_uncertainty$importance), ]
  
  # Calcul avec Relief
  relief_scores <- relief(defaut ~ ., data = data)
  relief_scores <- relief_scores[order(relief_scores$importance), ]
  
  plot_importance <- function(df, title) {
    ggplot(df, aes(x = reorder(attributes, importance), y = importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = title, x = "Attributs", y = "Importance") +
      theme_minimal()
  }
  
  list(
    info_gain = plot_importance(info_gain, paste(title, "Information Gain")),
    gain_ratio = plot_importance(gain_ratio, paste(title, "Gain Ratio")),
    symm_uncertainty = plot_importance(symm_uncertainty, paste(title, "Symmetrical Uncertainty")),
    relief_scores = plot_importance(relief_scores, paste(title, "Relief Scores"))
  )
}




data_median <- data
data_median$age <- ifelse(is.na(data_median$age), median(data_median$age, na.rm = TRUE), data_median$age)
data_median$adresse <- ifelse(is.na(data_median$adresse), median(data_median$adresse, na.rm = TRUE), data_median$adresse)

set.seed(12345)
plots_original <- importance(data, "Données originales -")
plots_mf <- importance(data_mf, "Données imputées avec missForest -")
plots_median <- importance(data_median, "Données imputées avec la médiane -")

grid.arrange(plots_original$info_gain, plots_original$gain_ratio, plots_original$symm_uncertainty, plots_original$relief_scores, ncol = 2)
grid.arrange(plots_mf$info_gain, plots_mf$gain_ratio, plots_mf$symm_uncertainty, plots_mf$relief_scores, ncol = 2)
grid.arrange(plots_median$info_gain, plots_median$gain_ratio, plots_median$symm_uncertainty, plots_median$relief_scores, ncol = 2)


#pour chacune des metriques et chacun des datasets on remaque que les 2 variables les moins importante sont éducation et revenus à part pour le relief score de éducation des données imputé avec la médiane
#cela est en contradiction avec les données brutes et miss forest et nos observations des nuages précédents
#on va donc garder dans un premier temps nos données imputé par miss forest, puis essayer de détecter si la variable education peut nous servir à la prediction tout comme revenus.


#regardons dans un modele d'arbre de décision si notre modele perd en capacité de prediction en retirant éducation avec algo C5.0

# Sans éducation
sanseducation <- data_mf[, -2]
# idx <- createDataPartition(sanseducation$defaut, p = 0.5, list = FALSE)
# train_data <- sanseducation[idx, ]
# test_data <- sanseducation[-idx, ]
# treesansed <- C5.0(defaut ~ ., data = train_data)
# prob_predictions_sans <- predict(treesansed, test_data, type = "prob")[, 2]
# perf_sans <- performance(prediction(prob_predictions_sans, test_data$defaut), measure = "tpr", x.measure = "fpr")
# auc_sans <- performance(prediction(prob_predictions_sans, test_data$defaut), measure = "auc")@y.values[[1]]

# Avec éducation
# set.seed(12345)
# idx <- createDataPartition(data_mf$defaut, p = 0.5, list = FALSE)
# train_data <- data_mf[idx, ]
# test_data <- data_mf[-idx, ]
# treeaveced <- C5.0(defaut ~ ., data = train_data)
# prob_predictions_avec <- predict(treeaveced, test_data, type = "prob")[, 2]
# perf_avec <- performance(prediction(prob_predictions_avec, test_data$defaut), measure = "tpr", x.measure = "fpr")
# auc_avec <- performance(prediction(prob_predictions_avec, test_data$defaut), measure = "auc")@y.values[[1]]
# 
# 
# plot(perf_sans, col = "blue", lwd = 2, main = "Courbes ROC comparatives",
#      xlab = "False Positive Rate", ylab = "True Positive Rate")
# lines(perf_avec@x.values[[1]], perf_avec@y.values[[1]], col = "red", lwd = 2)
# 
# # Ajout de la diagonale
# abline(a = 0, b = 1, lty = 2, col = "gray")
# 
# # Légende
# legend("bottomright", legend = c(paste("Sans éducation (AUC =", round(auc_sans, 4), ")"),
#                                  paste("Avec éducation (AUC =", round(auc_avec, 4), ")")),
#        col = c("blue", "red"), lwd = 2)
# 
# plot(treeaveced, main = "Arbre de décision avec éducation")
# plot(treesansed, main = "Arbre de décision sans éducation")

#Conclusion : en testant avec plusieurs seed le modèle reste stable et la variable education n'apporte pas vraiment d'information supplémentaire pour la prédiction
#on va garder un jeu de donnée sans education noté sanseducation pour la suite de l'analyse

importance(sanseducation, "Données sans éducation -")

qplot(revenus,education,data=data_mf,color=defaut) + geom_jitter(height = 0.4) + geom_point(alpha=0.5)
#on remarque que revenus est une combinaison de debcred, debcarte et autres

qplot(emploi,age,data=data_mf,color=defaut)+geom_point(alpha=0.5) + geom_jitter(height = 0.3, width = 0.3)
qplot(adresse,age,data=data_mf,color=defaut)+geom_point(alpha=0.5) + geom_jitter(height = 0.3, width = 0.3)

scatterplot3d(x = data_mf$emploi,y = data_mf$age,z = data_mf$adresse,pch=20,angle = 80,grid = TRUE, color = ifelse(data$defaut == "Oui", "green", "red"))
scatterplot3d(x = data_mf$emploi,y = data_mf$adresse,z = data_mf$education,pch=20,angle = 40,grid = TRUE, color = ifelse(data$defaut == "Oui", "green", "red"))




nuage_de_points = function(x, y, datau = data,jw = 0,jh = 0) {
  qplot(.data[[x]], .data[[y]], data = data,color = defaut, 
        main = paste("Nuage de points de", x, "et", y), 
        xlab = paste("Valeur de", x), 
        ylab = paste("Valeur de", y)
  )+geom_jitter(width= jw ,height=jh)
}

qplot(revenus/100,debcred,data=data_mf,color=defaut,xlim=c(0,4)) +geom_point(alpha=0.5)+geom_jitter(width=0.5,height=0.5)
nuage_de_points("revenus", "debcred") 
nuage_de_points("revenus", "debcarte")
nuage_de_points("revenus", "autres")
nuage_de_points("revenus", "adresse")
nuage_de_points("revenus", "emploi")
nuage_de_points("revenus", "age",jw=0.3,jh=0.3)
nuage_de_points("revenus", "education",jh=0.3)

nuage_de_points("debcred", "debcarte")
nuage_de_points("debcred", "autres")
nuage_de_points("debcred", "adresse")
nuage_de_points("debcred", "emploi")
nuage_de_points("debcred", "age",jh=0.3,jw=0.3)
nuage_de_points("debcred", "education",jh=0.3,jw=0.3)

nuage_de_points("debcarte", "autres")
nuage_de_points("debcarte", "adresse")
nuage_de_points("debcarte", "emploi",jh=0.3,jw=1)
nuage_de_points("debcarte", "age")
nuage_de_points("debcarte", "education",jh=0.3,jw=0.5)

nuage_de_points("autres", "adresse")
nuage_de_points("autres", "emploi",jh=0.3,jw=0.3)
nuage_de_points("autres", "age")
nuage_de_points("autres", "education",jh=0.3,jw=0.3)

nuage_de_points("adresse", "emploi",jh=0.3,jw=0.3)
nuage_de_points("adresse", "age",jh=0.3,jw=0.3)
nuage_de_points("adresse", "education",jh=0.3,jw=0.3)

nuage_de_points("emploi", "age",jh=0.3,jw=0.3)
nuage_de_points("emploi", "education",jh=0.3,jw=0.3)

nuage_de_points("age", "education",jh=0.3,jw=0.3)
qplot((adresse+emploi),education,data=data_mf,color=defaut) + geom_jitter(width=0.3,height = 0.3) +geom_point(alpha=0.5)


# Matrice de corrélation
cor_matrix <- cor(data_mf[, sapply(data, is.numeric)])
print(cor_matrix)

# Matrice de covariance
cov_matrix <- cov(data_mf[, sapply(data, is.numeric)])
print(cov_matrix)

# Visualisation de la matrice de corrélation
library(corrplot)
corrplot(cor_matrix, method = "square", type = "lower", tl.col = "black", tl.srt = 45,diag = TRUE,bg = "black")


##############
# Clustering #
##############

#methode du coude(elbow) pour déterminer le nombre optimal de clusters
data_num <- data_mf %>%
  select(age, revenus, autres,adresse)
#conseillé de scalé les variables pour le kmeans ??
data_scaled <- scale(data_num)

# Méthode 1 : Elbow (within sum of squares)
fviz_nbclust(data_scaled, kmeans, method = "wss") +
  ggplot2::labs(title = "Elbow wss- Nombre optimal de clusters")

# Méthode 2 : Silhouette moyenne
fviz_nbclust(data_scaled, kmeans, method = "silhouette") +
  ggplot2::labs(title = "Silhouette - Nombre optimal de clusters")

# Méthode 3 : Gap Statistic
#/!\ un peu long 
set.seed(123) 
fviz_nbclust(data_scaled, kmeans, method = "gap_stat", nstart = 25, nboot = 50) +
  ggplot2::labs(title = "Gap Statistic - Nombre optimal de clusters")

k_opt <- 4   #on ne sait pas vraiment...
set.seed(123)
km_res <- kmeans(data_scaled, centers = k_opt, nstart = 25)
dataclustcoude <- data_mf
dataclustcoude$cluster <- as.factor(km_res$cluster)
#Visualisation globale des clusters (selon 2 axes principaux PCA par exemple)
fviz_cluster(km_res, 
             data = data_scaled,
             ellipse.type = "euclid", # on entour par une ellipse les clusters
             geom = "point", 
             show.clust.cent = TRUE, 
             main = "K-means clustering sur data_mf")

#Visualisation de la silhouette
sil <- silhouette(km_res$cluster, dist(data_scaled))
fviz_silhouette(sil) + 
  ggplot2::labs(title = paste("Silhouette plot pour K =", k_opt))
cat("Silhouette moyenne =", mean(sil[, "sil_width"]), "\n")


qplot(age,cluster,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(revenus,cluster,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(autres,cluster,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(adresse,cluster,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(education,cluster,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(emploi,cluster,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)

qplot(cluster,age,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(cluster,revenus,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(cluster,adresse,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(cluster,debcred,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.4) +geom_point(alpha=0.5)
qplot(cluster,adresse,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(cluster,debcarte,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)
qplot(cluster,autres,data=dataclustcoude,color=defaut) + geom_jitter(height = 0.3,width = 0.3) +geom_point(alpha=0.5)

table(dataclustcoude$cluster,dataclustcoude$defaut)

stats_par_cluster <- dataclustcoude %>%
  group_by(cluster) %>% 
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    mean_revenus = mean(revenus, na.rm = TRUE),
    sd_revenus = sd(revenus, na.rm = TRUE),
    mean_emploi= mean(emploi, na.rm = TRUE),
    sd_emploi = sd(emploi, na.rm = TRUE),
    mean_adresse = mean(adresse, na.rm = TRUE),
    sd_adresse = sd(adresse, na.rm = TRUE),
    mean_debcred = mean(debcred, na.rm = TRUE),
    sd_debcred = sd(debcred, na.rm = TRUE),
    mean_debcarte = mean(debcarte, na.rm = TRUE),
    sd_debcarte = sd(debcarte, na.rm = TRUE),
    mean_autres = mean(autres, na.rm = TRUE),
    sd_autres = sd(autres, na.rm = TRUE),
    count = n()  # Nombre d'observations par cluster
  )


print(stats_par_cluster)

#densité
dmatrix <- daisy(data_mf)

dbs <- dbscan(dmatrix, eps = 0.1, minPts = 2)
table(dbs$cluster)
print(qplot(as.factor(dbs$cluster), data=data_mf, fill=defaut))

#eps = 0,025 m =15 et 9clusters
# for (eps in seq(0.025,0.04,by=0.001)){
#   dbs <- dbscan(dmatrix, eps, minPts = 15)
#   print(table(dbs$cluster, data_mf$defaut))
#   print(qplot(as.factor(dbs$cluster), data=data_mf, fill=defaut))
#   cat("Tapez Entree")
#   scan()
# }  
dbs <- dbscan(dmatrix, eps = 0.025, minPts = 15)

datacluster <- data_mf
datacluster$cluster <- dbs$cluster
print(qplot(as.factor(dbs$cluster), data=data_mf, fill=defaut))

qplot(age,cluster,data=datacluster,color=defaut) + geom_jitter(width=0.3) +geom_point(alpha=0.5)
qplot(education,cluster,data=datacluster,color=defaut) + geom_jitter(width=0.3) +geom_point(alpha=0.5)
qplot(emploi,cluster,data=datacluster,color=defaut) + geom_jitter(width=0.3) +geom_point(alpha=0.5)
qplot(adresse,cluster,data=datacluster,color=defaut) + geom_jitter(width=0.3) +geom_point(alpha=0.5)
qplot(revenus,cluster,data=datacluster,color=defaut) + geom_jitter(width=0.3) +geom_point(alpha=0.5)
qplot(debcred,cluster,data=datacluster,color=defaut) + geom_jitter(width=0.3) +geom_point(alpha=0.5)
qplot(debcarte,cluster,data=datacluster,color=defaut) + geom_jitter(width=0.3) +geom_point(alpha=0.5)
qplot(autres,cluster,data=datacluster,color=defaut) + geom_jitter(width=0.3) +geom_point(alpha=0.5)

qplot(debcarte,emploi,data=datacluster,color=defaut) + geom_jitter(height=0.5,width = 0.5) +geom_point(alpha=0.5)
scatterplot3d(x = datacluster$debcarte,
              y = datacluster$age,
              z = datacluster$cluster,
              pch=20,angle = 40,grid = TRUE,
              color = ifelse(datacluster$defaut == "Oui", "green", "red"))




library(dplyr)
stats_par_cluster <- datacluster %>%
  group_by(cluster) %>% 
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    mean_revenus = mean(revenus, na.rm = TRUE),
    sd_revenus = sd(revenus, na.rm = TRUE),
    mean_emploi= mean(emploi, na.rm = TRUE),
    sd_emploi = sd(emploi, na.rm = TRUE),
    mean_adresse = mean(adresse, na.rm = TRUE),
    sd_adresse = sd(adresse, na.rm = TRUE),
    mean_debcred = mean(debcred, na.rm = TRUE),
    sd_debcred = sd(debcred, na.rm = TRUE),
    mean_debcarte = mean(debcarte, na.rm = TRUE),
    sd_debcarte = sd(debcarte, na.rm = TRUE),
    mean_autres = mean(autres, na.rm = TRUE),
    sd_autres = sd(autres, na.rm = TRUE),
    count = n()  # Nombre d'observations par cluster
  )


print(stats_par_cluster)


#hierarchique (dendogramme)
agn <- agnes(dmatrix)
plot(agn, which.plots = 2)
rect.hclust(agn, k=4, border="red")

agn9 <- cutree(agn, k=4)

qplot(age,agn9,data=data_mf,color=defaut) + geom_jitter(height=0.3) +geom_point(alpha=0.5)
qplot(education,agn9,data=data_mf,color=defaut) + geom_jitter(height=0.3) +geom_point(alpha=0.5)
qplot(debcred,agn9,data=data_mf,color=defaut) + geom_jitter(height=0.3) +geom_point(alpha=0.5)
qplot(debcarte,agn9,data=data_mf,color=defaut) + geom_jitter(height=0.3) +geom_point(alpha=0.5)
qplot(autres,agn9,data=data_mf,color=defaut) + geom_jitter(height=0.3) +geom_point(alpha=0.5)
qplot(adresse,agn9,data=data_mf,color=defaut) + geom_jitter(height=0.3) +geom_point(alpha=0.5)
qplot(emploi,agn9,data=data_mf,color=defaut) + geom_jitter(height=0.3) +geom_point(alpha=0.5)


dataQmf <- data_mf
dataQmf$DE <- ifelse(dataQmf$emploi==0,dataQmf$debcarte,dataQmf$debcarte/dataQmf$emploi)


#cette partie du code est commenté car ce sont des tests préliminaire pour mieux comprendre comment le package caret fonctionne
# ######################
# # Arbres de décision #
# ######################
# 
# best_tree <- function(algo = c("rpart", "C5.0"),
#                       data,
#                       prop = 0.66,
#                       seed = 12345,
#                       positive_class = "Oui",
#                       smote = FALSE) {
#   set.seed(seed)
#   
#   algo <- match.arg(algo)
#   
#   trainIndex <- createDataPartition(data$defaut, p = prop, list = FALSE)
#   dataEA <- data[ trainIndex, ]
#   dataET <- data[-trainIndex, ]
#   
#   #Si on veut utiliser smote ou pas
#   if (smote==TRUE){
#     ctrl <- trainControl(
#       method = "cv",
#       number = 10,               # 10Cross-Validation
#       summaryFunction = twoClassSummary,
#       classProbs = TRUE,         # indispensable pour calculer l'AUC
#       savePredictions = TRUE,
#       sampling = "smote"
#     )}
#   else{
#   ctrl <- trainControl(
#     method = "cv",
#     number = 10,               
#     summaryFunction = twoClassSummary,
#     classProbs = TRUE,         
#     savePredictions = TRUE,
#   )}
#   
#   fit <- train(
#     defaut ~ .,                
#     data = dataEA,
#     method = algo,
#     metric = "ROC",            # on optimise l’AUC, la sensi bilité, la sepciﬁcité
#     trControl = ctrl,
#     maximize = TRUE
#   )
#   
#   pred_class <- predict(fit, newdata = dataET)
#   
#   conf_mat <- confusionMatrix(
#     data = pred_class,
#     reference = dataET$defaut,
#     positive = positive_class
#   )
#   
#   precision <- conf_mat$byClass["Pos Pred Value"]
#   
#   pred_prob <- predict(fit, newdata = dataET, type = "prob")[, positive_class]
#   roc_obj <- roc(response = dataET$defaut, 
#                  predictor = pred_prob, 
#                  levels = rev(levels(dataET$defaut)))  
#   model_auc <- as.numeric(auc(roc_obj))
#   
#   #Retourne tout ce qui nous intéresse
#   return(
#     list(
#       model       = fit,          # le modèle entraîné (caret)
#       conf_matrix = conf_mat,     # matrice de confusion
#       precision   = precision,    # précision sur le test
#       roc_curve   = roc_obj,      # objet ROC (pROC) qu'on peut tracer
#       AUC         = model_auc     # AUC du modèle
#     )
#   )
# }
# 
# 
# dataQsansed <- dataQmf[,-2]
# 
# model_rpartmf <- best_tree("rpart",data_mf,0.66,12345)
# model_C50mf <- best_tree("C5.0",data_mf,0.66,12345)
# model_rpartmed <- best_tree("rpart",data_median,0.66,12345)
# model_C50med <- best_tree("C5.0",data_median,0.66,12345)
# model_rpartsansed <- best_tree("rpart",sanseducation,0.66,12345)
# model_C50sansed <- best_tree("C5.0",sanseducation,0.66,12345)
# model_rpartQmf <- best_tree("rpart",dataQmf,0.66,12345)
# model_C50Qmf <- best_tree("C5.0",dataQmf,0.66,12345)
# model_C50Qmfsansed <- best_tree("C5.0",dataQsansed,0.66,12345)
# model_C50QmfsansedSMOTE <- best_tree("C5.0",dataQsansed,0.66,12345,smote = TRUE)
# model_C50QmfSMOTE <- best_tree("C5.0",dataQmf,0.66,12345,smote = TRUE)
# model_C50mfSMOTE <- best_tree("C5.0",data_mf,0.66,12345,smote = TRUE)
# 
# 
# plot(
#   model_rpartmf$roc_curve,
#   col  = "blue",
#   lwd  = 2,
#   main = "Comparaison de toutes les courbes ROC",
#   xlab = "Taux de faux positifs (FPR)",
#   ylab = "Taux de vrais positifs (TPR)"
# )
# 
# lines(model_C50mf$roc_curve,       col = "red",    lwd = 2)
# lines(model_rpartmed$roc_curve,    col = "orange", lwd = 2)
# lines(model_C50med$roc_curve,      col = "green",  lwd = 2)
# lines(model_rpartsansed$roc_curve, col = "purple", lwd = 2)
# lines(model_C50sansed$roc_curve,   col = "black",  lwd = 2)
# lines(model_rpartQmf$roc_curve,    col = "cyan",   lwd = 2)
# lines(model_C50Qmf$roc_curve,      col = "magenta", lwd = 2)
# lines(model_C50Qmfsansed$roc_curve, col = "brown",  lwd = 2)
# lines(model_C50QmfsansedSMOTE$roc_curve, col = "darkblue", lwd = 2)
# lines(model_C50QmfSMOTE$roc_curve,      col = "gold",    lwd = 2)
# lines(model_C50mfSMOTE$roc_curve,      col = "darkred", lwd = 2)
# 
# legend(
#   "bottomright",
#   legend = c(
#     "rpart MF",
#     "C5.0 MF",
#     "rpart Median",
#     "C5.0 Median",
#     "rpart SansEd",
#     "C5.0 SansEd",
#     "rpart QMF",
#     "C5.0 QMF",
#     "C5.0 QMF SansEd",
#     "C5.0 QMF SansEd SMOTE",
#     "C5.0 QMF SMOTE",
#     "C5.0 MF SMOTE"
#   ),
#   col = c("blue", "red", "orange", "green", "purple", "black", "cyan", "magenta", "brown", "darkblue", "gold", "darkred"),
#   lwd = rep(2, 12)  # Épaisseur des traits
# )
# 
# #il est claire que toute les modèles C50 sans exceptions sont meilleurs que les rpart(CART)
# 
# extract_metrics <- function(model_obj, model_name) {
#   # Récupération de la matrice de confusion
#   cm <- model_obj$conf_matrix
#   
#   # Accuracy
#   accuracy <- cm$overall["Accuracy"]
#   
#   # Dans confusionMatrix, la classe positive est "Oui"
#   FN <- cm$table[2,1]
#   TP <- cm$table[2,2]
#   
#   # Pourcentage de faux négatif (en %) ???
#   fn_percent <- FN/(TP + FN)
#   
#   # AUC
#   auc_val <- model_obj$AUC
#   
#   # On retourne un data frame (1 ligne)
#   data.frame(
#     Model           = model_name,
#     pospredvalue    = as.numeric(cm$byClass["Pos Pred Value"]),
#     Spec            = as.numeric(cm$byClass["Specificity"]),
#     Sens            = as.numeric(cm$byClass["Sensitivity"]),
#     Accuracy        = as.numeric(accuracy),
#     AUC             = as.numeric(auc_val)
#   )
# }
# 
# all_models_summary <- rbind(
#   extract_metrics(model_rpartmf,     "rpart MF"),
#   extract_metrics(model_C50mf,       "C5.0 MF"),
#   extract_metrics(model_rpartmed,    "rpart Median"),
#   extract_metrics(model_C50med,      "C5.0 Median"),
#   extract_metrics(model_rpartsansed, "rpart SansEd"),
#   extract_metrics(model_C50sansed,   "C5.0 SansEd"),
#   extract_metrics(model_rpartQmf,    "rpart QMF"),
#   extract_metrics(model_C50Qmf,      "C5.0 QMF"),
#   extract_metrics(model_C50Qmfsansed, "C5.0 QMF SansEd"),
#   extract_metrics(model_C50QmfsansedSMOTE, "C5.0 QMF SansEd SMOTE"),
#   extract_metrics(model_C50QmfSMOTE, "C5.0 QMF SMOTE"),
#   extract_metrics(model_C50mfSMOTE, "C5.0 MF SMOTE")
# )
# 
# all_models_summary
# 
# 
# #Conclusion: le meilleur modèle est celui avec les données imputé par missforest,avec la nouvelle variable debcarte/emploi avec l'algorithme C5.0(model_C50Qmf) (à verifier ?)
# model_C50Qmf
# 
# 
# 
# ######################
# #    Random Forest   #
# ######################
# 
# 
# 
# 
# best_RF<-function(data, prop) {
#   set.seed(12345)
#   idx <- createDataPartition(data$defaut, p = prop, list = FALSE)
#   dataEA <- data[idx, ]
#   dataET <- data[-idx, ]
#   
#   ntree_seq <- seq(300, 600, by = 100)
#   mtry_seq  <- seq(2, 6, by = 2)         
#   
#   all_roc_points <- data.frame() # on va stocker plus tard les coordonnées ROC
#   all_scores     <- data.frame()  # et ici les scores Accuracy, AUC
#   for (ntree in ntree_seq) {
#     for (mtry in mtry_seq) {
#       
#       model <- randomForest(defaut ~ ., 
#                             data = dataEA, 
#                             ntree = ntree, 
#                             mtry = mtry)
#       
#       
#       pred_prob  <- predict(model, newdata = dataET, type = "prob")[, "Oui"]
#       
#       pred_class <- predict(model, newdata = dataET, type = "class")
#       
#       conf_mat   <- confusionMatrix(pred_class, dataET$defaut, positive = "Oui")
#       accuracy   <- conf_mat$overall["Accuracy"]
#     
#       roc_obj <- roc(
#         response  = dataET$defaut,
#         predictor = pred_prob,
#         levels    = c("Non", "Oui"),  # "Oui"=positif, "Non"=négatif
#         direction = "<"
#       )
#       auc_val <- pROC::auc(roc_obj)
#       
#       # -- Extraire coords ROC pour tracer la courbe --
#       coords_roc <- coords(
#         roc_obj,
#         x = "all",
#         ret = c("threshold", "specificity", "sensitivity"),
#         transpose = FALSE
#       ) %>%
#         mutate(
#           TPR   = sensitivity,
#           FPR   = 1 - specificity,
#           ntree = ntree,
#           mtry  = mtry
#         )
#       
#       # -- Empiler les données ROC --
#       all_roc_points <- bind_rows(all_roc_points, coords_roc)
#       
#       # -- Empiler les scores (Accuracy, AUC) --
#       all_scores <- bind_rows(all_scores, data.frame(
#         ntree    = ntree,
#         mtry     = mtry,
#         Accuracy = as.numeric(accuracy),
#         AUC      = as.numeric(auc_val)
#       ))
#     }
#   }
#   
#   all_roc_points <- all_roc_points %>%
#     mutate(model_label = paste0("ntree=", ntree, ", mtry=", mtry))
#   
#   g <- ggplot(all_roc_points, aes(x = FPR, y = TPR, color = model_label)) +
#     geom_line() +
#     labs(
#       title = "ROC - Tous les modèles Random Forest",
#       x = "Taux de faux positifs (FPR)",
#       y = "Taux de vrais positifs (TPR)",
#       color = "Modèle"
#     ) +
#     theme_minimal()
#   
#   print(g)
# 
#   all_scores <- all_scores %>%
#     arrange(desc(AUC))
#   
#   print(all_scores)
#   
#   return(invisible(list(
#     roc_data   = all_roc_points,  
#     plot       = g,             
#     scores     = all_scores       
#   )))
# }
# 
# 
# 
# best_RF(data_mf,0.5)
# best_RF(data_median,0.5)
# best_RF(sanseducation,0.5)
# best_RF(dataQmf,0.5)
# 
# 
# 
# 
# best_RF <- function(data,
#                     prop = 0.66,
#                     seed = 12345,
#                     positive_class = "Oui",
#                     smote = FALSE,
#                     ntree = 1000) {  # Ajout de ntree comme paramètre réglable
#   set.seed(seed)
#   
#   # Séparation des données en ensemble d'apprentissage et de test
#   trainIndex <- createDataPartition(data$defaut, p = prop, list = FALSE)
#   dataEA <- data[trainIndex, ]
#   dataET <- data[-trainIndex, ]
#   
#   # Contrôle de l'entraînement avec ou sans SMOTE
#   if (smote == TRUE) {
#     ctrl <- trainControl(
#       method = "cv",
#       number = 10,               # 10 Cross-Validation
#       summaryFunction = twoClassSummary,
#       classProbs = TRUE,         # Indispensable pour calculer l'AUC
#       savePredictions = TRUE,
#       sampling = "smote"
#     )
#   } else {
#     ctrl <- trainControl(
#       method = "cv",
#       number = 10,               
#       summaryFunction = twoClassSummary,
#       classProbs = TRUE,         
#       savePredictions = TRUE
#     )
#   }
#   
#   # Définition de la grille des hyperparamètres
#   tuneGrid <- expand.grid(
#     mtry = c(2, 3, 4, 6)  # Nombre de variables sélectionnées à chaque split
#   )
#   
#   # Entraînement du modèle Random Forest
#   fit <- train(
#     defaut ~ .,                
#     data = dataEA,
#     method = "rf",             # Spécifie l'utilisation de Random Forest
#     metric = "ROC",            # Optimisation basée sur l'AUC
#     trControl = ctrl,
#     tuneGrid = tuneGrid,       # Grille des hyperparamètres
#     ntree = ntree              # Spécifie le nombre d'arbres
#   )
#   
#   # Prédictions sur l'ensemble de test
#   pred_class <- predict(fit, newdata = dataET)
#   
#   # Matrice de confusion
#   conf_mat <- confusionMatrix(
#     data = pred_class,
#     reference = dataET$defaut,
#     positive = positive_class
#   )
#   
#   # Précision
#   precision <- conf_mat$byClass["Pos Pred Value"]
#   
#   # Prédictions probabilistes pour l'AUC
#   pred_prob <- predict(fit, newdata = dataET, type = "prob")[, positive_class]
#   roc_obj <- roc(response = dataET$defaut, 
#                  predictor = pred_prob, 
#                  levels = rev(levels(dataET$defaut)))  
#   model_auc <- as.numeric(auc(roc_obj))
#   
#   # Retour des résultats
#   return(
#     list(
#       model       = fit,          # le modèle entraîné (caret)
#       conf_matrix = conf_mat,     # matrice de confusion
#       precision   = precision,    # précision sur le test
#       roc_curve   = roc_obj,      # objet ROC (pROC) qu'on peut tracer
#       AUC         = model_auc     # AUC du modèle
#     )
#   )
# }
# 
# 
# model_rfmf1000 <- best_RF(data_mf,0.5,12345)
# model_rfmf600 <- best_RF(data_mf,0.5,12345,ntree = 600)
# model_rfmf600bool <- best_RF(databool,0.5,12345,ntree = 600)
# model_rangermf <- best_RF(data_mf,0.5,12345)
# model_rangermed <- best_RF(data_median,0.5,12345)
# model_rangersansed <- best_RF(sanseducation,0.5,12345)
# model_rangerQmf <- best_RF(dataQmf,0.5,12345)
# model_rangerQmfsansed <- best_RF(dataQsansed,0.5,12345)
########################################################################################



#####################################
# recherche du meilleur classifieur #
#####################################


best_modelPPV <- function(data,
                          prop = 0.5,
                          seed = 123,
                          positive_class = "Oui",
                          smote = FALSE, # ne sert a rien
                          algo,
                          tuneLength = 10,
                          metric_to_optimize = "ROC",
                          search_type = "random") { # search type = grille si on veut des paretre vectorielle qui se suivent
  set.seed(seed)
  
  # Séparation apprentissage / test
  data_oui <- subset(data, defaut == "Oui") 
  data_non <- subset(data, defaut == "Non")
  min_size <- min(nrow(data_oui), nrow(data_non)) 
  set.seed(seed)
  data_oui_sample <- data_oui[sample(1:nrow(data_oui), min_size), ] 
  data_non_sample <- data_non[sample(1:nrow(data_non), min_size), ] 
  data_balanced <- rbind(data_oui_sample, data_non_sample) 
  set.seed(seed)
  data_balanced <- data_balanced[sample(1:nrow(data_balanced)), ] 
  set.seed(seed)
  idx <- createDataPartition(data_balanced$defaut, p = prop, list = FALSE) 
  dataEA <- data_balanced[idx, ] 
  dataET <- data_balanced[-idx, ] 
  
  # Fonction de résumé pour la PPV
  ppvSummary <- function(data, lev = NULL, model = NULL) {
    cm <- caret::confusionMatrix(data = data$pred, reference = data$obs, positive = positive_class)
    ppv <- cm$byClass["Pos Pred Value"]
    names(ppv) <- "PPV"
    return(ppv)
  }
  
  # Contrôle de train
  if (smote) {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = if (metric_to_optimize == "PPV") ppvSummary else twoClassSummary,
      classProbs = TRUE,
      savePredictions = TRUE,
      sampling = "smote",
      search = search_type
    )
  } else {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = if (metric_to_optimize == "PPV") ppvSummary else twoClassSummary,
      classProbs = TRUE,
      savePredictions = TRUE,
      search = search_type
    )
  }
  
  # Entraînement
  fit <- caret::train(
    form = as.formula("defaut ~ ."), 
    data = dataEA,
    method = algo,             
    metric = if (metric_to_optimize == "PPV") "PPV" else "ROC",  
    trControl = ctrl,
    tuneLength = tuneLength
  )
  
  # Prédictions et métriques
  pred_class <- predict(fit, newdata = dataET)
  conf_mat <- caret::confusionMatrix(
    data = pred_class,
    reference = dataET$defaut,
    positive = positive_class
  )
  precision <- conf_mat$byClass["Pos Pred Value"]
  pred_prob <- predict(fit, newdata = dataET, type = "prob")[, positive_class]
  roc_obj <- roc(response = dataET$defaut, 
                 predictor = pred_prob, 
                 levels = rev(levels(dataET$defaut)))
  model_auc <- as.numeric(auc(roc_obj))
  
  return(list(
    model       = fit,
    conf_matrix = conf_mat,
    precision   = precision,
    roc_curve   = roc_obj,
    AUC         = model_auc
  ))
}

######################
# Test structuré.    #
######################


# Modèles pour les données data_mf
avNNetMF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetMF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetMF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomMF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2MF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyMF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialMF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesMF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbMF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnMF <- best_modelPPV(data_mf, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerMF <- best_modelPPV(data_mf, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristMF <- best_modelPPV(data_mf, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfMF <- best_modelPPV(data_mf, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50MF <- best_modelPPV(data_mf, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartMF <- best_modelPPV(data_mf, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_mf <- c(
  "avNNetMF", "nnetMF", "pcaNNetMF", "multinomMF", 
  "svmLinear2MF", "svmPolyMF", "svmRadialMF", 
  "naive_bayesMF", "nbMF", "kknnMF", 
  "rangerMF", "RboristMF", "rfMF", 
  "C50MF", "rpartMF"
)

# Liste des résultats des modèles
model_list_mf <- list(
  avNNetMF, nnetMF, pcaNNetMF, multinomMF, 
  svmLinear2MF, svmPolyMF, svmRadialMF, 
  naive_bayesMF, nbMF, kknnMF, 
  rangerMF, RboristMF, rfMF, 
  C50MF, rpartMF
)

# Création du tableau récapitulatif
results_df_mf <- data.frame(
  model    = model_names_mf,
  AUC      = sapply(model_list_mf, function(x) x$AUC),
  PPV      = sapply(model_list_mf, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_mf$moyenne <- (results_df_mf$AUC + results_df_mf$PPV) / 2

# Affichage du tableau final
print(results_df_mf)


dataQmf <- data_mf
dataQmf$DE <- ifelse(dataQmf$emploi==0,dataQmf$debcarte,dataQmf$debcarte/dataQmf$emploi)

# Modèles pour les données dataQmf
avNNetQmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetQmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetQmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomQmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2Qmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyQmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialQmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesQmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbQmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnQmf <- best_modelPPV(dataQmf, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerQmf <- best_modelPPV(dataQmf, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristQmf <- best_modelPPV(dataQmf, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfQmf <- best_modelPPV(dataQmf, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50Qmf <- best_modelPPV(dataQmf, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartQmf <- best_modelPPV(dataQmf, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_Qmf <- c(
  "avNNetQmf", "nnetQmf", "pcaNNetQmf", "multinomQmf", 
  "svmLinear2Qmf", "svmPolyQmf", "svmRadialQmf", 
  "naive_bayesQmf", "nbQmf", "kknnQmf", 
  "rangerQmf", "RboristQmf", "rfQmf", 
  "C50Qmf", "rpartQmf"
)

# Liste des résultats des modèles
model_list_Qmf <- list(
  avNNetQmf, nnetQmf, pcaNNetQmf, multinomQmf, 
  svmLinear2Qmf, svmPolyQmf, svmRadialQmf, 
  naive_bayesQmf, nbQmf, kknnQmf, 
  rangerQmf, RboristQmf, rfQmf, 
  C50Qmf, rpartQmf
)

# Création du tableau récapitulatif
results_df_Qmf <- data.frame(
  model    = model_names_Qmf,
  AUC      = sapply(model_list_Qmf, function(x) x$AUC),
  PPV      = sapply(model_list_Qmf, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_Qmf$moyenne <- (results_df_Qmf$AUC + results_df_Qmf$PPV) / 2

# Affichage du tableau final
print(results_df_Qmf)



sansed <- data_mf[,-2]
# Modèles pour les données sansed
avNNetSansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetSansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetSansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomSansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2Sansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolySansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialSansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesSansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbSansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnSansed <- best_modelPPV(sansed, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerSansed <- best_modelPPV(sansed, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristSansed <- best_modelPPV(sansed, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfSansed <- best_modelPPV(sansed, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50Sansed <- best_modelPPV(sansed, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartSansed <- best_modelPPV(sansed, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_sansed <- c(
  "avNNetSansed", "nnetSansed", "pcaNNetSansed", "multinomSansed", 
  "svmLinear2Sansed", "svmPolySansed", "svmRadialSansed", 
  "naive_bayesSansed", "nbSansed", "kknnSansed", 
  "rangerSansed", "RboristSansed", "rfSansed", 
  "C50Sansed", "rpartSansed"
)

# Liste des résultats des modèles
model_list_sansed <- list(
  avNNetSansed, nnetSansed, pcaNNetSansed, multinomSansed, 
  svmLinear2Sansed, svmPolySansed, svmRadialSansed, 
  naive_bayesSansed, nbSansed, kknnSansed, 
  rangerSansed, RboristSansed, rfSansed, 
  C50Sansed, rpartSansed
)

# Création du tableau récapitulatif
results_df_sansed <- data.frame(
  model    = model_names_sansed,
  AUC      = sapply(model_list_sansed, function(x) x$AUC),
  PPV      = sapply(model_list_sansed, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_sansed$moyenne <- (results_df_sansed$AUC + results_df_sansed$PPV) / 2

# Affichage du tableau final
print(results_df_sansed)





sansrev <- data_mf[,-5]
# Modèles pour les données sansrev
avNNetSansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetSansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetSansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomSansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2Sansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolySansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialSansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesSansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbSansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnSansrev <- best_modelPPV(sansrev, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerSansrev <- best_modelPPV(sansrev, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristSansrev <- best_modelPPV(sansrev, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfSansrev <- best_modelPPV(sansrev, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50Sansrev <- best_modelPPV(sansrev, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartSansrev <- best_modelPPV(sansrev, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_sansrev <- c(
  "avNNetSansrev", "nnetSansrev", "pcaNNetSansrev", "multinomSansrev", 
  "svmLinear2Sansrev", "svmPolySansrev", "svmRadialSansrev", 
  "naive_bayesSansrev", "nbSansrev", "kknnSansrev", 
  "rangerSansrev", "RboristSansrev", "rfSansrev", 
  "C50Sansrev", "rpartSansrev"
)

# Liste des résultats des modèles
model_list_sansrev <- list(
  avNNetSansrev, nnetSansrev, pcaNNetSansrev, multinomSansrev, 
  svmLinear2Sansrev, svmPolySansrev, svmRadialSansrev, 
  naive_bayesSansrev, nbSansrev, kknnSansrev, 
  rangerSansrev, RboristSansrev, rfSansrev, 
  C50Sansrev, rpartSansrev
)

# Création du tableau récapitulatif
results_df_sansrev <- data.frame(
  model    = model_names_sansrev,
  AUC      = sapply(model_list_sansrev, function(x) x$AUC),
  PPV      = sapply(model_list_sansrev, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_sansrev$moyenne <- (results_df_sansrev$AUC + results_df_sansrev$PPV) / 2

# Affichage du tableau final
print(results_df_sansrev)





sansedrev <- data_mf[,-c(2,5)]
# Modèles pour les données sansedrev
avNNetSansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetSansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetSansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomSansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2Sansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolySansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialSansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesSansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbSansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnSansedrev <- best_modelPPV(sansedrev, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerSansedrev <- best_modelPPV(sansedrev, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristSansedrev <- best_modelPPV(sansedrev, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfSansedrev <- best_modelPPV(sansedrev, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50Sansedrev <- best_modelPPV(sansedrev, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartSansedrev <- best_modelPPV(sansedrev, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_sansedrev <- c(
  "avNNetSansedrev", "nnetSansedrev", "pcaNNetSansedrev", "multinomSansedrev", 
  "svmLinear2Sansedrev", "svmPolySansedrev", "svmRadialSansedrev", 
  "naive_bayesSansedrev", "nbSansedrev", "kknnSansedrev", 
  "rangerSansedrev", "RboristSansedrev", "rfSansedrev", 
  "C50Sansedrev", "rpartSansedrev"
)

# Liste des résultats des modèles
model_list_sansedrev <- list(
  avNNetSansedrev, nnetSansedrev, pcaNNetSansedrev, multinomSansedrev, 
  svmLinear2Sansedrev, svmPolySansedrev, svmRadialSansedrev, 
  naive_bayesSansedrev, nbSansedrev, kknnSansedrev, 
  rangerSansedrev, RboristSansedrev, rfSansedrev, 
  C50Sansedrev, rpartSansedrev
)

# Création du tableau récapitulatif
results_df_sansedrev <- data.frame(
  model    = model_names_sansedrev,
  AUC      = sapply(model_list_sansedrev, function(x) x$AUC),
  PPV      = sapply(model_list_sansedrev, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_sansedrev$moyenne <- (results_df_sansedrev$AUC + results_df_sansedrev$PPV) / 2

# Affichage du tableau final
print(results_df_sansedrev)


sansedQ <- sansed
sansedQ$DE <- ifelse(sansedQ$emploi==0,sansedQ$debcarte,sansedQ$debcarte/sansedQ$emploi)
# Modèles pour les données sansedQ
avNNetSansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetSansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetSansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomSansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2SansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolySansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialSansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesSansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbSansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnSansedQ <- best_modelPPV(sansedQ, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerSansedQ <- best_modelPPV(sansedQ, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristSansedQ <- best_modelPPV(sansedQ, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfSansedQ <- best_modelPPV(sansedQ, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50SansedQ <- best_modelPPV(sansedQ, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartSansedQ <- best_modelPPV(sansedQ, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_sansedQ <- c(
  "avNNetSansedQ", "nnetSansedQ", "pcaNNetSansedQ", "multinomSansedQ", 
  "svmLinear2SansedQ", "svmPolySansedQ", "svmRadialSansedQ", 
  "naive_bayesSansedQ", "nbSansedQ", "kknnSansedQ", 
  "rangerSansedQ", "RboristSansedQ", "rfSansedQ", 
  "C50SansedQ", "rpartSansedQ"
)

# Liste des résultats des modèles
model_list_sansedQ <- list(
  avNNetSansedQ, nnetSansedQ, pcaNNetSansedQ, multinomSansedQ, 
  svmLinear2SansedQ, svmPolySansedQ, svmRadialSansedQ, 
  naive_bayesSansedQ, nbSansedQ, kknnSansedQ, 
  rangerSansedQ, RboristSansedQ, rfSansedQ, 
  C50SansedQ, rpartSansedQ
)

# Création du tableau récapitulatif
results_df_sansedQ <- data.frame(
  model    = model_names_sansedQ,
  AUC      = sapply(model_list_sansedQ, function(x) x$AUC),
  PPV      = sapply(model_list_sansedQ, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_sansedQ$moyenne <- (results_df_sansedQ$AUC + results_df_sansedQ$PPV) / 2

# Affichage du tableau final
print(results_df_sansedQ)





sansrevQ <- data_mf[,-5]
sansrevQ$DE <- ifelse(sansrevQ$emploi==0,sansrevQ$debcarte,sansrevQ$debcarte/sansrevQ$emploi)

# Modèles pour les données sansrevQ
avNNetSansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetSansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetSansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomSansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2SansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolySansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialSansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesSansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbSansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnSansrevQ <- best_modelPPV(sansrevQ, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerSansrevQ <- best_modelPPV(sansrevQ, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristSansrevQ <- best_modelPPV(sansrevQ, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfSansrevQ <- best_modelPPV(sansrevQ, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50SansrevQ <- best_modelPPV(sansrevQ, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartSansrevQ <- best_modelPPV(sansrevQ, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_sansrevQ <- c(
  "avNNetSansrevQ", "nnetSansrevQ", "pcaNNetSansrevQ", "multinomSansrevQ", 
  "svmLinear2SansrevQ", "svmPolySansrevQ", "svmRadialSansrevQ", 
  "naive_bayesSansrevQ", "nbSansrevQ", "kknnSansrevQ", 
  "rangerSansrevQ", "RboristSansrevQ", "rfSansrevQ", 
  "C50SansrevQ", "rpartSansrevQ"
)

# Liste des résultats des modèles
model_list_sansrevQ <- list(
  avNNetSansrevQ, nnetSansrevQ, pcaNNetSansrevQ, multinomSansrevQ, 
  svmLinear2SansrevQ, svmPolySansrevQ, svmRadialSansrevQ, 
  naive_bayesSansrevQ, nbSansrevQ, kknnSansrevQ, 
  rangerSansrevQ, RboristSansrevQ, rfSansrevQ, 
  C50SansrevQ, rpartSansrevQ
)

# Création du tableau récapitulatif
results_df_sansrevQ <- data.frame(
  model    = model_names_sansrevQ,
  AUC      = sapply(model_list_sansrevQ, function(x) x$AUC),
  PPV      = sapply(model_list_sansrevQ, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_sansrevQ$moyenne <- (results_df_sansrevQ$AUC + results_df_sansrevQ$PPV) / 2

# Affichage du tableau final
print(results_df_sansrevQ)




sansedrevQ <- sansedrev
sansedrevQ$DE <- ifelse(sansedrevQ$emploi==0,sansedrevQ$debcarte,sansedrevQ$debcarte/sansedrevQ$emploi)
# Modèles pour les données sansedrevQ
avNNetSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2SansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolySansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50SansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartSansedrevQ <- best_modelPPV(sansedrevQ, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_sansedrevQ <- c(
  "avNNetSansedrevQ", "nnetSansedrevQ", "pcaNNetSansedrevQ", "multinomSansedrevQ", 
  "svmLinear2SansedrevQ", "svmPolySansedrevQ", "svmRadialSansedrevQ", 
  "naive_bayesSansedrevQ", "nbSansedrevQ", "kknnSansedrevQ", 
  "rangerSansedrevQ", "RboristSansedrevQ", "rfSansedrevQ", 
  "C50SansedrevQ", "rpartSansedrevQ"
)

# Liste des résultats des modèles
model_list_sansedrevQ <- list(
  avNNetSansedrevQ, nnetSansedrevQ, pcaNNetSansedrevQ, multinomSansedrevQ, 
  svmLinear2SansedrevQ, svmPolySansedrevQ, svmRadialSansedrevQ, 
  naive_bayesSansedrevQ, nbSansedrevQ, kknnSansedrevQ, 
  rangerSansedrevQ, RboristSansedrevQ, rfSansedrevQ, 
  C50SansedrevQ, rpartSansedrevQ
)

# Création du tableau récapitulatif
results_df_sansedrevQ <- data.frame(
  model    = model_names_sansedrevQ,
  AUC      = sapply(model_list_sansedrevQ, function(x) x$AUC),
  PPV      = sapply(model_list_sansedrevQ, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_sansedrevQ$moyenne <- (results_df_sansedrevQ$AUC + results_df_sansedrevQ$PPV) / 2

# Affichage du tableau final
print(results_df_sansedrevQ)



databoolmf <- data_mf
databoolmf$age <- ifelse(databoolmf$age>=38,2,1)
databoolmf$adresse <- ifelse(databoolmf$adresse>=10,2,1)
# Modèles pour les données databoolmf
avNNetDataboolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetDataboolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetDataboolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomDataboolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2Databoolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyDataboolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialDataboolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesDataboolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbDataboolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnDataboolmf <- best_modelPPV(databoolmf, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerDataboolmf <- best_modelPPV(databoolmf, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristDataboolmf <- best_modelPPV(databoolmf, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfDataboolmf <- best_modelPPV(databoolmf, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50Databoolmf <- best_modelPPV(databoolmf, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartDataboolmf <- best_modelPPV(databoolmf, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_databoolmf <- c(
  "avNNetDataboolmf", "nnetDataboolmf", "pcaNNetDataboolmf", "multinomDataboolmf", 
  "svmLinear2Databoolmf", "svmPolyDataboolmf", "svmRadialDataboolmf", 
  "naive_bayesDataboolmf", "nbDataboolmf", "kknnDataboolmf", 
  "rangerDataboolmf", "RboristDataboolmf", "rfDataboolmf", 
  "C50Databoolmf", "rpartDataboolmf"
)

# Liste des résultats des modèles
model_list_databoolmf <- list(
  avNNetDataboolmf, nnetDataboolmf, pcaNNetDataboolmf, multinomDataboolmf, 
  svmLinear2Databoolmf, svmPolyDataboolmf, svmRadialDataboolmf, 
  naive_bayesDataboolmf, nbDataboolmf, kknnDataboolmf, 
  rangerDataboolmf, RboristDataboolmf, rfDataboolmf, 
  C50Databoolmf, rpartDataboolmf
)

# Création du tableau récapitulatif
results_df_databoolmf <- data.frame(
  model    = model_names_databoolmf,
  AUC      = sapply(model_list_databoolmf, function(x) x$AUC),
  PPV      = sapply(model_list_databoolmf, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_databoolmf$moyenne <- (results_df_databoolmf$AUC + results_df_databoolmf$PPV) / 2

# Affichage du tableau final
print(results_df_databoolmf)




databoolmfsansrev <- databoolmf[,-5]
# Modèles pour les données databoolmfsansrev
avNNetDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2Databoolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50Databoolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartDataboolmfsansrev <- best_modelPPV(databoolmfsansrev, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_databoolmfsansrev <- c(
  "avNNetDataboolmfsansrev", "nnetDataboolmfsansrev", "pcaNNetDataboolmfsansrev", "multinomDataboolmfsansrev", 
  "svmLinear2Databoolmfsansrev", "svmPolyDataboolmfsansrev", "svmRadialDataboolmfsansrev", 
  "naive_bayesDataboolmfsansrev", "nbDataboolmfsansrev", "kknnDataboolmfsansrev", 
  "rangerDataboolmfsansrev", "RboristDataboolmfsansrev", "rfDataboolmfsansrev", 
  "C50Databoolmfsansrev", "rpartDataboolmfsansrev"
)

# Liste des résultats des modèles
model_list_databoolmfsansrev <- list(
  avNNetDataboolmfsansrev, nnetDataboolmfsansrev, pcaNNetDataboolmfsansrev, multinomDataboolmfsansrev, 
  svmLinear2Databoolmfsansrev, svmPolyDataboolmfsansrev, svmRadialDataboolmfsansrev, 
  naive_bayesDataboolmfsansrev, nbDataboolmfsansrev, kknnDataboolmfsansrev, 
  rangerDataboolmfsansrev, RboristDataboolmfsansrev, rfDataboolmfsansrev, 
  C50Databoolmfsansrev, rpartDataboolmfsansrev
)

# Création du tableau récapitulatif
results_df_databoolmfsansrev <- data.frame(
  model    = model_names_databoolmfsansrev,
  AUC      = sapply(model_list_databoolmfsansrev, function(x) x$AUC),
  PPV      = sapply(model_list_databoolmfsansrev, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_databoolmfsansrev$moyenne <- (results_df_databoolmfsansrev$AUC + results_df_databoolmfsansrev$PPV) / 2

# Affichage du tableau final
print(results_df_databoolmfsansrev)


databoolmfsansed <- databoolmf[,-2]
# Modèles pour les données databoolmfsansed
avNNetDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2Databoolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50Databoolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartDataboolmfsansed <- best_modelPPV(databoolmfsansed, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_databoolmfsansed <- c(
  "avNNetDataboolmfsansed", "nnetDataboolmfsansed", "pcaNNetDataboolmfsansed", "multinomDataboolmfsansed", 
  "svmLinear2Databoolmfsansed", "svmPolyDataboolmfsansed", "svmRadialDataboolmfsansed", 
  "naive_bayesDataboolmfsansed", "nbDataboolmfsansed", "kknnDataboolmfsansed", 
  "rangerDataboolmfsansed", "RboristDataboolmfsansed", "rfDataboolmfsansed", 
  "C50Databoolmfsansed", "rpartDataboolmfsansed"
)

# Liste des résultats des modèles
model_list_databoolmfsansed <- list(
  avNNetDataboolmfsansed, nnetDataboolmfsansed, pcaNNetDataboolmfsansed, multinomDataboolmfsansed, 
  svmLinear2Databoolmfsansed, svmPolyDataboolmfsansed, svmRadialDataboolmfsansed, 
  naive_bayesDataboolmfsansed, nbDataboolmfsansed, kknnDataboolmfsansed, 
  rangerDataboolmfsansed, RboristDataboolmfsansed, rfDataboolmfsansed, 
  C50Databoolmfsansed, rpartDataboolmfsansed
)

# Création du tableau récapitulatif
results_df_databoolmfsansed <- data.frame(
  model    = model_names_databoolmfsansed,
  AUC      = sapply(model_list_databoolmfsansed, function(x) x$AUC),
  PPV      = sapply(model_list_databoolmfsansed, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_databoolmfsansed$moyenne <- (results_df_databoolmfsansed$AUC + results_df_databoolmfsansed$PPV) / 2

# Affichage du tableau final
print(results_df_databoolmfsansed)


databoolmfsansedrev <- databoolmf[,-c(2,5)]
# Modèles pour les données databoolmfsansedrev
avNNetDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2Databoolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50Databoolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartDataboolmfsansedrev <- best_modelPPV(databoolmfsansedrev, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_databoolmfsansedrev <- c(
  "avNNetDataboolmfsansedrev", "nnetDataboolmfsansedrev", "pcaNNetDataboolmfsansedrev", "multinomDataboolmfsansedrev", 
  "svmLinear2Databoolmfsansedrev", "svmPolyDataboolmfsansedrev", "svmRadialDataboolmfsansedrev", 
  "naive_bayesDataboolmfsansedrev", "nbDataboolmfsansedrev", "kknnDataboolmfsansedrev", 
  "rangerDataboolmfsansedrev", "RboristDataboolmfsansedrev", "rfDataboolmfsansedrev", 
  "C50Databoolmfsansedrev", "rpartDataboolmfsansedrev"
)

# Liste des résultats des modèles
model_list_databoolmfsansedrev <- list(
  avNNetDataboolmfsansedrev, nnetDataboolmfsansedrev, pcaNNetDataboolmfsansedrev, multinomDataboolmfsansedrev, 
  svmLinear2Databoolmfsansedrev, svmPolyDataboolmfsansedrev, svmRadialDataboolmfsansedrev, 
  naive_bayesDataboolmfsansedrev, nbDataboolmfsansedrev, kknnDataboolmfsansedrev, 
  rangerDataboolmfsansedrev, RboristDataboolmfsansedrev, rfDataboolmfsansedrev, 
  C50Databoolmfsansedrev, rpartDataboolmfsansedrev
)

# Création du tableau récapitulatif
results_df_databoolmfsansedrev <- data.frame(
  model    = model_names_databoolmfsansedrev,
  AUC      = sapply(model_list_databoolmfsansedrev, function(x) x$AUC),
  PPV      = sapply(model_list_databoolmfsansedrev, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_databoolmfsansedrev$moyenne <- (results_df_databoolmfsansedrev$AUC + results_df_databoolmfsansedrev$PPV) / 2

# Affichage du tableau final
print(results_df_databoolmfsansedrev)


databoolmfsansrevQ <- databoolmfsansrev
databoolmfsansrevQ$DE <- ifelse(databoolmfsansrevQ$emploi==0,databoolmfsansrevQ$debcarte,databoolmfsansrevQ$debcarte/databoolmfsansrevQ$emploi)
# Modèles pour les données databoolmfsansrevQ
avNNetDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2DataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50DataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartDataboolmfsansrevQ <- best_modelPPV(databoolmfsansrevQ, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_databoolmfsansrevQ <- c(
  "avNNetDataboolmfsansrevQ", "nnetDataboolmfsansrevQ", "pcaNNetDataboolmfsansrevQ", "multinomDataboolmfsansrevQ", 
  "svmLinear2DataboolmfsansrevQ", "svmPolyDataboolmfsansrevQ", "svmRadialDataboolmfsansrevQ", 
  "naive_bayesDataboolmfsansrevQ", "nbDataboolmfsansrevQ", "kknnDataboolmfsansrevQ", 
  "rangerDataboolmfsansrevQ", "RboristDataboolmfsansrevQ", "rfDataboolmfsansrevQ", 
  "C50DataboolmfsansrevQ", "rpartDataboolmfsansrevQ"
)

# Liste des résultats des modèles
model_list_databoolmfsansrevQ <- list(
  avNNetDataboolmfsansrevQ, nnetDataboolmfsansrevQ, pcaNNetDataboolmfsansrevQ, multinomDataboolmfsansrevQ, 
  svmLinear2DataboolmfsansrevQ, svmPolyDataboolmfsansrevQ, svmRadialDataboolmfsansrevQ, 
  naive_bayesDataboolmfsansrevQ, nbDataboolmfsansrevQ, kknnDataboolmfsansrevQ, 
  rangerDataboolmfsansrevQ, RboristDataboolmfsansrevQ, rfDataboolmfsansrevQ, 
  C50DataboolmfsansrevQ, rpartDataboolmfsansrevQ
)

# Création du tableau récapitulatif
results_df_databoolmfsansrevQ <- data.frame(
  model    = model_names_databoolmfsansrevQ,
  AUC      = sapply(model_list_databoolmfsansrevQ, function(x) x$AUC),
  PPV      = sapply(model_list_databoolmfsansrevQ, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_databoolmfsansrevQ$moyenne <- (results_df_databoolmfsansrevQ$AUC + results_df_databoolmfsansrevQ$PPV) / 2

# Affichage du tableau final
print(results_df_databoolmfsansrevQ)





databoolmfsansedQ <- databoolmfsansed
databoolmfsansedQ$DE <- ifelse(databoolmfsansedQ$emploi==0,databoolmfsansedQ$debcarte,databoolmfsansedQ$debcarte/databoolmfsansedQ$emploi)
# Modèles pour les données databoolmfsansedQ
avNNetDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2DataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50DataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartDataboolmfsansedQ <- best_modelPPV(databoolmfsansedQ, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_databoolmfsansedQ <- c(
  "avNNetDataboolmfsansedQ", "nnetDataboolmfsansedQ", "pcaNNetDataboolmfsansedQ", "multinomDataboolmfsansedQ", 
  "svmLinear2DataboolmfsansedQ", "svmPolyDataboolmfsansedQ", "svmRadialDataboolmfsansedQ", 
  "naive_bayesDataboolmfsansedQ", "nbDataboolmfsansedQ", "kknnDataboolmfsansedQ", 
  "rangerDataboolmfsansedQ", "RboristDataboolmfsansedQ", "rfDataboolmfsansedQ", 
  "C50DataboolmfsansedQ", "rpartDataboolmfsansedQ"
)

# Liste des résultats des modèles
model_list_databoolmfsansedQ <- list(
  avNNetDataboolmfsansedQ, nnetDataboolmfsansedQ, pcaNNetDataboolmfsansedQ, multinomDataboolmfsansedQ, 
  svmLinear2DataboolmfsansedQ, svmPolyDataboolmfsansedQ, svmRadialDataboolmfsansedQ, 
  naive_bayesDataboolmfsansedQ, nbDataboolmfsansedQ, kknnDataboolmfsansedQ, 
  rangerDataboolmfsansedQ, RboristDataboolmfsansedQ, rfDataboolmfsansedQ, 
  C50DataboolmfsansedQ, rpartDataboolmfsansedQ
)

# Création du tableau récapitulatif
results_df_databoolmfsansedQ <- data.frame(
  model    = model_names_databoolmfsansedQ,
  AUC      = sapply(model_list_databoolmfsansedQ, function(x) x$AUC),
  PPV      = sapply(model_list_databoolmfsansedQ, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_databoolmfsansedQ$moyenne <- (results_df_databoolmfsansedQ$AUC + results_df_databoolmfsansedQ$PPV) / 2

# Affichage du tableau final
print(results_df_databoolmfsansedQ)




databoolmfsansedrevQ <- databoolmfsansedrev
databoolmfsansedrevQ$DE <- ifelse(databoolmfsansedrevQ$emploi==0,databoolmfsansedrevQ$debcarte,databoolmfsansedrevQ$debcarte/databoolmfsansedrevQ$emploi)
# Modèles pour les données databoolmfsansedrevQ
avNNetDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2DataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50DataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartDataboolmfsansedrevQ <- best_modelPPV(databoolmfsansedrevQ, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_databoolmfsansedrevQ <- c(
  "avNNetDataboolmfsansedrevQ", "nnetDataboolmfsansedrevQ", "pcaNNetDataboolmfsansedrevQ", "multinomDataboolmfsansedrevQ", 
  "svmLinear2DataboolmfsansedrevQ", "svmPolyDataboolmfsansedrevQ", "svmRadialDataboolmfsansedrevQ", 
  "naive_bayesDataboolmfsansedrevQ", "nbDataboolmfsansedrevQ", "kknnDataboolmfsansedrevQ", 
  "rangerDataboolmfsansedrevQ", "RboristDataboolmfsansedrevQ", "rfDataboolmfsansedrevQ", 
  "C50DataboolmfsansedrevQ", "rpartDataboolmfsansedrevQ"
)

# Liste des résultats des modèles
model_list_databoolmfsansedrevQ <- list(
  avNNetDataboolmfsansedrevQ, nnetDataboolmfsansedrevQ, pcaNNetDataboolmfsansedrevQ, multinomDataboolmfsansedrevQ, 
  svmLinear2DataboolmfsansedrevQ, svmPolyDataboolmfsansedrevQ, svmRadialDataboolmfsansedrevQ, 
  naive_bayesDataboolmfsansedrevQ, nbDataboolmfsansedrevQ, kknnDataboolmfsansedrevQ, 
  rangerDataboolmfsansedrevQ, RboristDataboolmfsansedrevQ, rfDataboolmfsansedrevQ, 
  C50DataboolmfsansedrevQ, rpartDataboolmfsansedrevQ
)

# Création du tableau récapitulatif
results_df_databoolmfsansedrevQ <- data.frame(
  model    = model_names_databoolmfsansedrevQ,
  AUC      = sapply(model_list_databoolmfsansedrevQ, function(x) x$AUC),
  PPV      = sapply(model_list_databoolmfsansedrevQ, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_databoolmfsansedrevQ$moyenne <- (results_df_databoolmfsansedrevQ$AUC + results_df_databoolmfsansedrevQ$PPV) / 2

# Affichage du tableau final
print(results_df_databoolmfsansedrevQ)





databoolQmf <- databoolmf
databoolQmf$DE <- ifelse(databoolQmf$emploi==0,databoolQmf$debcarte,databoolQmf$debcarte/databoolQmf$emploi)
# Modèles pour les données databoolQmf
avNNetDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "avNNet", tuneLength = 10, metric_to_optimize = "ROC")
nnetDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "nnet", tuneLength = 10, metric_to_optimize = "ROC")
pcaNNetDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "pcaNNet", tuneLength = 10, metric_to_optimize = "ROC")
multinomDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "multinom", tuneLength = 10, metric_to_optimize = "ROC")

svmLinear2DataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "svmLinear2", tuneLength = 3, metric_to_optimize = "ROC")
svmPolyDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "svmPoly", tuneLength = 3, metric_to_optimize = "ROC")
svmRadialDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "svmRadial", tuneLength = 3, metric_to_optimize = "ROC")

naive_bayesDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "naive_bayes", tuneLength = 1000, metric_to_optimize = "ROC")
nbDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "nb", tuneLength = 5, metric_to_optimize = "ROC")

kknnDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.5, seed = 123, algo = "kknn", tuneLength = 2, metric_to_optimize = "ROC")

rangerDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.66, seed = 123, algo = "ranger", tuneLength = 5, metric_to_optimize = "ROC")
RboristDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.66, seed = 123, algo = "Rborist", tuneLength = 4, metric_to_optimize = "ROC")
rfDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.66, seed = 123, algo = "rf", tuneLength = 2, metric_to_optimize = "ROC")

C50DataboolQmf <- best_modelPPV(databoolQmf, prop = 0.66, seed = 123, algo = "C5.0", tuneLength = 5, metric_to_optimize = "ROC")
rpartDataboolQmf <- best_modelPPV(databoolQmf, prop = 0.66, seed = 123, algo = "rpart", tuneLength = 10, metric_to_optimize = "ROC")

# Noms des modèles
model_names_databoolQmf <- c(
  "avNNetDataboolQmf", "nnetDataboolQmf", "pcaNNetDataboolQmf", "multinomDataboolQmf", 
  "svmLinear2DataboolQmf", "svmPolyDataboolQmf", "svmRadialDataboolQmf", 
  "naive_bayesDataboolQmf", "nbDataboolQmf", "kknnDataboolQmf", 
  "rangerDataboolQmf", "RboristDataboolQmf", "rfDataboolQmf", 
  "C50DataboolQmf", "rpartDataboolQmf"
)

# Liste des résultats des modèles
model_list_databoolQmf <- list(
  avNNetDataboolQmf, nnetDataboolQmf, pcaNNetDataboolQmf, multinomDataboolQmf, 
  svmLinear2DataboolQmf, svmPolyDataboolQmf, svmRadialDataboolQmf, 
  naive_bayesDataboolQmf, nbDataboolQmf, kknnDataboolQmf, 
  rangerDataboolQmf, RboristDataboolQmf, rfDataboolQmf, 
  C50DataboolQmf, rpartDataboolQmf
)

# Création du tableau récapitulatif
results_df_databoolQmf <- data.frame(
  model    = model_names_databoolQmf,
  AUC      = sapply(model_list_databoolQmf, function(x) x$AUC),
  PPV      = sapply(model_list_databoolQmf, function(x) x$precision)
)

# Ajout de la colonne moyenne = (AUC + PPV) / 2
results_df_databoolQmf$moyenne <- (results_df_databoolQmf$AUC + results_df_databoolQmf$PPV) / 2

# Affichage du tableau final
print(results_df_databoolQmf)





# install.packages("dplyr") # si ce n'est pas déjà installé
library(dplyr)

# On met toutes les data.frames dans une liste nommée
list_dfs <- list(
  results_df_mf              = results_df_mf,
  results_df_Qmf             = results_df_Qmf,
  results_df_sansed          = results_df_sansed,
  results_df_sansrev         = results_df_sansrev,
  results_df_sansedrev       = results_df_sansedrev,
  results_df_sansedQ         = results_df_sansedQ,
  results_df_sansrevQ        = results_df_sansrevQ,
  results_df_sansedrevQ      = results_df_sansedrevQ,
  results_df_databoolmf      = results_df_databoolmf,
  results_df_databoolmfsansrev     = results_df_databoolmfsansrev,
  results_df_databoolmfsansed      = results_df_databoolmfsansed,
  results_df_databoolmfsansedrev   = results_df_databoolmfsansedrev,
  results_df_databoolmfsansrevQ    = results_df_databoolmfsansrevQ,
  results_df_databoolmfsansedQ     = results_df_databoolmfsansedQ,
  results_df_databoolmfsansedrevQ  = results_df_databoolmfsansedrevQ,
  results_df_databoolQmf           = results_df_databoolQmf
)

# On applique un bind_rows après avoir muté la colonne dataset
final_results_df <- bind_rows(
  lapply(
    names(list_dfs),
    function(nom) {
      list_dfs[[nom]] %>%
        mutate(dataset = nom) # ajoute une colonne pour identifier la source
    }
  )
)

# On regarde le résultat final !
head(final_results_df)

library(readr)

write_csv(final_results_df, "resultats.csv")

#################################
# Aplication du meilleur modèle #
#################################

datatest <- read.csv("projet_new.csv", sep = ",", header = TRUE)
datatest[datatest == 999] <- NA
datatest <- data.frame(lapply(datatest, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)))
datatest <- datatest[,-1]
datatest <- datatest[,-4]

datatest$education <- ifelse(datatest$education == "Niveau bac", 1,
                             ifelse(datatest$education == "Bac+2", 2,
                                    ifelse(datatest$education == "Bac+3", 3,
                                           ifelse(datatest$education == "Bac+4", 4,
                                                  ifelse(datatest$education == "Bac+5 et plus", 5, NA)))))
colSums(is.na(datatest))
datatestsansrevQ <- datatest[,-5]
datatestsansrevQ$DE <- ifelse(datatestsansrevQ$emploi==0,datatestsansrevQ$debcarte,datatestsansrevQ$debcarte/datatestsansrevQ$emploi)



set.seed(123)
data <- data_mf[,-5]
data$DE <- ifelse(data$emploi==0,data$debcarte,data$debcarte/data$emploi)
data_oui <- subset(data, defaut == "Oui") 
data_non <- subset(data, defaut == "Non")
min_size <- min(nrow(data_oui), nrow(data_non)) 
set.seed(123)
data_oui_sample <- data_oui[sample(1:nrow(data_oui), min_size), ] 
data_non_sample <- data_non[sample(1:nrow(data_non), min_size), ] 
data_balanced <- rbind(data_oui_sample, data_non_sample) 
set.seed(123)
data_balanced <- data_balanced[sample(1:nrow(data_balanced)), ] 
set.seed(123)
idx <- createDataPartition(data_balanced$defaut, p = 0.5, list = FALSE) 
dataEA <- data_balanced[idx, ]



#laplace = 0 - usekernel = FALSE - adjust = 1
model <- naive_bayes(defaut~., data=dataEA, laplace = 0 , usekernel = FALSE,adjust = 1) 
predictions <- predict(model, newdata=datatestsansrevQ, type="class")
table(predictions)

datatest$predictions <- predictions

write.csv(datatest, "predictions.csv")


