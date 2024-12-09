order2Terms <- 
  combn(listQuantCovariates, 2L, 
        function(x) paste(x, collapse = ":"), simplify = TRUE)

order2Terms <- c(order2Terms, 
                 paste(listQuantCovariates, "^2", sep = ""))

formulaProp <- paste("group ~ . + ", paste(order2Terms, collapse = " + ")) %>% 
  as.formula()


dataTrain <- dataObs %>% 
  select(-RE78)

# dataTrain[, listQuantCovariates] <-
#   dataTrain[, listQuantCovariates] %>%
#   scale(center = TRUE, scale = TRUE)

modelProp <- 
  glm(formulaProp, data = dataTrain, family = binomial)

# summary(modelProp)

rm(order2Terms)

# modelPropAIC <- step(modelProp, trace = 0L)

# summary(modelPropAIC)

# require(pROC)
# auc(response = dataObs$group, predictor = propValues)

rm(dataTrain)

propValues <- predict.glm(modelProp, type = "response")