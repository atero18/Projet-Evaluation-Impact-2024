library(readr)
library(dplyr)
library(forcats)

readFile <- function(file)
{
  # The variables from left to right are: 
  # treatment indicator (1 if treated, 0 if not treated), 
  # age, education, Black (1 if black, 0 otherwise), 
  # Hispanic (1 if Hispanic, 0 otherwise), married (1 if married, 0 otherwise), 
  # nodegree (1 if no high school degree, 0 otherwise), 
  # RE74 (earnings in 1974), RE75 (earnings in 1975),
  # and RE78 (earnings in 1978)

  colTypes <- cols("T" = "f", 
                   age = "i", education = "i", 
                   Black = readr::col_factor(levels = c("0", "1")), 
                   Hispanic = readr::col_factor(levels = c("0", "1")), 
                   married = readr::col_factor(levels = c("0", "1")),
                   nodegree = readr::col_factor(levels = c("0", "1")), 
                   RE74 = "d", RE75 = "d", RE78 = "d")
  
  data <- readr::read_csv(file, col_names = TRUE, col_types = colTypes)
  
  # Suggestion from Rubin & Imbens (2015)
  # indRE74 (1 if earnings in 1974, 0 otherwise)
  # indRE75 (1 if earnings in 1975, 0 otherwise)
  data %>% 
    mutate(RE74 = RE74 / 1000.0) %>% 
    mutate(indRE74 = (RE74 > 0.0) %>% as.integer() %>% factor(),
           .after = RE74) %>% 
    mutate(RE75 = RE75 / 1000.0) %>% 
    mutate(indRE75 = (RE75 > 0.0) %>% as.integer() %>% factor(), 
           .after = RE75) %>% 
    mutate(RE78 = RE78 / 1000.0)
  
}

# controlExp <- readFile("data/nswre74_Dehejia_Wahha_exp_control.csv")

treatedExp <- readFile("data/nswre74_Dehejia_Wahha_exp_treated.csv")

# dataExp <- rbind(controlExp, treatedExp)

# rm(controlExp)

# summary(dataExp)

controlObs <- readFile("data/cps_nonexp_control.csv")

dataObs <- rbind(controlObs, treatedExp) %>% 
  mutate(group = fct_recode(T, "Control" = "0", "Treated" = "1")) %>% 
  select(-T)

listCovariates <- setdiff(colnames(dataObs), c("group", "RE78"))
listQuantCovariates <- c("age", "education", "RE74", "RE75")

N <- nrow(dataObs)

rm(treatedExp, controlObs)

rm(readFile)

# summary(dataObs)
