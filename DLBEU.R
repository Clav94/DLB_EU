## Article MCL EU, Clément Aveneau, 08/2024

#Générer la table
dlb <- read.csv2("DLB_EU.csv")

#Pour supprimer les patients mixtes
dlb <- dlb[!(dlb$debut == 4), ]

#Faire les groupes
dlb$avc <- factor (dlb$avc, levels=c(0,1), labels=c("No", "Yes"))
dlb$etude <- factor (dlb$etude, levels=c(1:4), labels=c("No diploma", "Lower secondary education", "Upper secondary education", "Higher education"))
dlb$sexe <- factor (dlb$sexe, levels=c(1,2), labels=c("Woman", "Man"))
dlb$tabac <- factor (dlb$tabac, levels=c(0:2), labels=c("No", "Weaned", "Active"))
dlb$dyslipidemie <- factor (dlb$dyslipidemie, levels=c(0,1), labels=c("No", "Yes"))
dlb$diabete <- factor (dlb$diabete, levels=c(0,1), labels=c("No", "Yes"))
dlb$saos <- factor (dlb$saos, levels=c(0,1), labels=c("No", "Yes"))
dlb$hta <- factor (dlb$hta, levels=c(0,1), labels=c("No", "Yes"))
dlb$debut <- factor (dlb$debut, levels=c(1:3), labels=c("Cognitive", "Psychiatric", "Motor"))
dlb$perte_auto <- factor (dlb$perte_auto, levels=c(0,1), labels=c("No", "Yes"))
dlb$instit <- factor (dlb$instit, levels=c(0,1), labels=c("No", "Yes"))
dlb$rbd <- factor (dlb$rbd, levels=c(0,1), labels=c("No", "Yes"))
dlb$fluct <- factor (dlb$fluct, levels=c(0,1), labels=c("No", "Yes"))
dlb$psy <- factor (dlb$psy, levels=c(0,1), labels=c("No", "Yes"))
dlb$park <- factor (dlb$park, levels=c(0,1), labels=c("No", "Yes"))
dlb$dysauto <- factor (dlb$dysauto, levels=c(0,1), labels=c("No", "Yes"))
dlb$chute <- factor (dlb$chute, levels=c(0,1), labels=c("No", "Yes"))
dlb$irm <- factor (dlb$irm, levels=c(0,1), labels=c("Non realized", "Realized"))
dlb$pet <- factor (dlb$pet, levels=c(0:2), labels=c("Non realized","Realized normal", "Realized abnormal"))
dlb$dat <- factor (dlb$dat, levels=c(0:2), labels=c("Non realized","Realized normal", "Realized abnormal"))
dlb$eeg <- factor (dlb$eeg, levels=c(0:2), labels=c("Non realized","Realized normal", "Realized abnormal"))
dlb$polysom0 <- factor (dlb$polysom0, levels=c(0:2), labels=c("Non realized","Realized normal", "Realized abnormal"))
dlb$pl <- factor (dlb$pl, levels=c(0,1), labels=c("Non realized", "Realized"))
dlb$centre <- factor (dlb$centre, levels=c(1:5), labels=c("Lyon", "Paris-Lariboisière", "Rouen", "Bobigny", "Paris-FOR"))


#Ajouter une colonne age de début
dlb$age_debut <- dlb$annee_debut - dlb$annee_naissance
dlb$catage_debut <- cut(dlb$age_debut,
                     breaks = c(-Inf, 65, 75, Inf),
                     labels = c("-65 ans", "65-75 ans", ">75 ans"))


#Modèle de LCA 
library(poLCA)
lca_model <- poLCA(cbind(centre, sexe, rbd, psy, park, dysauto, chute, catage_debut) ~ 1, data = dlb, nclass = 3)
print(lca_model)

#HEATMAP
# Fonction pour convertir les résultats poLCA en dataframe utilisable pour la heatmap
create_heatmap_data <- function(lca_model) {
  # Créer une liste vide pour stocker les données
  heatmap_data <- list()
  
  # Boucle sur chaque variable dans l'analyse
  for (var in names(lca_model$probs)) {
    # Récupérer les probabilités conditionnelles pour chaque variable
    prob_matrix <- lca_model$probs[[var]]
    
    # Convertir en dataframe et ajouter des colonnes pour la classe et la variable
    prob_df <- as.data.frame(prob_matrix)
    prob_df$Class <- rownames(prob_df)
    prob_df$Variable <- var
    
    # Réorganiser le dataframe au format long (long format) pour ggplot
    prob_long <- reshape2::melt(prob_df, id.vars = c("Class", "Variable"))
    
    # Ajouter au dataframe final
    heatmap_data[[var]] <- prob_long
  }
  
  # Combiner toutes les données en un seul dataframe
  return(do.call(rbind, heatmap_data))
}

# Créer le dataframe à partir du modèle LCA
heatmap_data <- create_heatmap_data(lca_model)

# Afficher la heatmap avec ggplot
ggplot(heatmap_data, aes(x = Class, y = variable, fill = value)) +
  geom_tile(color = "white") +
  
  # Changer la palette de couleurs (de rouge pour 0 à bleu pour 1)
  scale_fill_gradient2(low = "white", mid = "orange", high = "red", midpoint = 0.5, limits = c(0, 1)) +
  
  # Ajouter des valeurs numériques sur chaque case
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  
  # Paramètres de la mise en page
  facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
  labs(title = "Heatmap des probabilités conditionnelles par classe", x = "Classe", y = "Variable") +
  theme_minimal() +
  
  # Ajuster les labels des axes pour améliorer la lisibilité
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Extraire les classes prédictives pour chaque individu
dlb$predicted_class <- lca_model$predclass
library(survival)
km_global <- survfit(Surv(delai_deces, deces) ~ predicted_class, data = enfants)
km_global