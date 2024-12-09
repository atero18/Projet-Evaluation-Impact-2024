library(dplyr)
library(ggplot2)
library(MatchIt)

# Execution des fichiers nécessaire pour le reste du script

source("src/loading_data.R")
source("src/calcul_propension.R")

str(dataObs)

dataObs = cbind(dataObs, propValues)


# Distribution des scores de propension pour les individus contrôlés
ggplot(data = dataObs %>% filter(group=="Control"), aes(x=propValues, group=group, fill=group)) +
  geom_histogram(alpha=0.5) +
  theme_minimal() +
  xlab(label="Score de propension") +
  ylab(label="") +
  labs(fill="Groupe d'appartenance")

# Distribution des scores pour les individus traités
ggplot(data = dataObs %>% filter(group=="Treated"), aes(x=propValues, group=group, fill=group)) +
  geom_histogram(alpha=0.5) +
  theme_minimal() +
  xlab(label="Score de propension") +
  ylab(label="") +
  labs(fill="Groupe d'appartenance") +
  scale_fill_manual(values = c("cyan3"))

# Comparaison des distribution pour les deux groupes
ggplot(data = dataObs, aes(x=propValues, group=group, fill=group)) +
  geom_histogram(alpha=0.5) +
  theme_minimal() +
  xlab(label="Score de propension") +
  ylab(label="") +
  labs(fill="Groupe d'appartenance")


################### Mise en place du matching pour le distance glm #######################
# En utilisant la distance glm
matching_GLM = matchit(formula = formulaProp, 
        data = dataObs, 
        replace = FALSE, 
        distance = "glm")
summary(matching_GLM)
plot(matching_GLM)
cobalt::love.plot(matching_GLM, drop.distance = TRUE)

# extraire les données 
match.data_GLM <- match.data(matching_GLM) 

# Test de Student de différence de revenu entre paires
pairwise.t.test(match.data_GLM$RE78,match.data_GLM$group,paired=TRUE, p.adjust.method ="bonferroni")


# Estimation de l'ATT sur les données matchées
subclass_GLM = as.data.frame(match.data_GLM$subclass) 
colnames(subclass_GLM)[1] = "subclass"
subclass_control = match.data_GLM %>% select(RE78, group, subclass) %>% filter(group=="Control") %>% select(-group)
subclass_treated = match.data_GLM %>% select(RE78, group, subclass) %>% filter(group=="Treated") %>% select(-group)

subclass_GLM = subclass_GLM %>% as.data.frame() %>% 
  left_join(subclass_control, by=join_by(subclass)) %>%
  left_join(subclass_treated, by=join_by(subclass))

colnames(subclass_GLM)[c(2,3)] = c("RE78_control", "RE78_treated")
summary(subclass_GLM)  

# On a donc un effet moyen du traitement de 

################### Mise en place du matching pour le distance mahalanobis #######################


matching_mahalanobis = matchit(formula = formulaProp, 
                       data = dataObs, 
                       replace = FALSE, 
                       distance = "mahalanobis")
summary(matching_mahalanobis)
plot(matching_mahalanobis)

cobalt::love.plot(matching_mahalanobis, drop.distance = TRUE)
# extraire les données 
match.data_mahalanobis <- match.data(matching_mahalanobis) 
# En utilisant la distance de mahalanobis
pairwise.t.test(match.data_mahalanobis$RE78,match.data_mahalanobis$group,paired=TRUE, p.adjust.method ="bonferroni")


### Estimation de l'effet moyen du traitement sur les individus matchés
subclass_mahalanobis = as.data.frame(match.data_mahalanobis$subclass) 
colnames(subclass_mahalanobis)[1] = "subclass"

subclass_control = match.data_mahalanobis %>% 
  select(RE78, group, subclass) %>% 
  filter(group=="Control") %>% 
  select(-group)

subclass_treated = match.data_mahalanobis %>% 
  select(RE78, group, subclass) %>% 
  filter(group=="Treated") %>% 
  select(-group)

subclass_mahalanobis = subclass_mahalanobis %>% 
  as.data.frame() %>% 
  left_join(subclass_control, by=join_by(subclass)) %>%
  left_join(subclass_treated, by=join_by(subclass))

colnames(subclass_mahalanobis)[c(2,3)] = c("RE78_control", "RE78_treated")
summary(subclass_mahalanobis)  


  
  
  
  