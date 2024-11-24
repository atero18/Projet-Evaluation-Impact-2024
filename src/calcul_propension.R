order2Terms <- 
  combn(listQuantCovariates, 2L, 
        function(x) paste(x, collapse = ":"), simplify = TRUE)

order2Terms <- c(order2Terms, 
                 paste(listQuantCovariates, "^2", sep = ""))

formulaProp <- paste("group ~ . + ", paste(order2Terms, collapse = " + ")) %>% 
  as.formula()


modelProp <- dataObs %>% 
  select(-RE78) %>% 
  glm(formulaProp, data = ., family = binomial)

rm(order2Terms, formulaProp)

propValues <- predict.glm(modelProp, type = "response")