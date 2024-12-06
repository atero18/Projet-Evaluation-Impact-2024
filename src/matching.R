library(dplyr)
library(ggplot2)
library(MatchIt)

str(dataObs)

dataObs = cbind(dataObs, propValues)
dataObs$indice = seq(from=1,to=nrow(dataObs), by=1)


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


### Mise en place du matching
# En utilisant la distance glm
?matchit

matching_GLM = matchit(formula = formulaProp, 
        data = dataObs, 
        replace = FALSE, 
        distance = "glm")
summary(matching_GLM)
plot(matching_GLM)

cobalt::love.plot(matching_GLM, drop.distance = TRUE)
# extraire les données 
match.data <- match.data(matching_GLM) 
# En utilisant la distance de mahalanobis
t.test(match.data[match.data$group==1,]$RE78,match.data[match.data$group==0,]$RE78, paired=TRUE)
pairwise.t.test(match.data$RE78,match.data$group,paired=TRUE, p.adjust.method ="bonferroni")
ATT = mean(match.data[match.data$group=="Treated","RE78"])-mean(match.data[match.data$group=="Control","RE78"])

matching_mahalanobis = matchit(formula = formulaProp, 
                       data = dataObs, 
                       replace = FALSE, 
                       distance = "mahalanobis")
summary(matching_mahalanobis)
plot(matching_mahalanobis)

cobalt::love.plot(matching_mahalanobis, drop.distance = TRUE)
# extraire les données 
match.data <- match.data(matching_mahalanobis) 
# En utilisant la distance de mahalanobis
pairwise.t.test(match.data$RE78,match.data$group,paired=TRUE, p.adjust.method ="bonferroni")
ATT = mean(match.data[match.data$group=="Treated","RE78"])-mean(match.data[match.data$group=="Control","RE78"])






