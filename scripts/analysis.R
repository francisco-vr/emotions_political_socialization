########################################################################################################
### Analysis Data                                                                                   ####          
### Septiember 2022                                                                                 ####
### Francisco Villarroel                                                                            ####
### df Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la            ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage

packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork","stringr",
              "tidyr","kableExtra","psych", "MASS", "foreign", "data.table","gtools","lubridate","AER",
              "xtable","pBrackets","Hmisc","ri2","ggpubr", "stargazer", "Rmisc","wesanderson", "gridExtra","ggmosaic",
              "vcd", "plyr", "ggannotate","scales", "fastDummies","gt", "MASS", "modelsummary", "nnet", "margins")
ipak(packages)

# load DF

df <-readRDS("data/Final Data/DF-final.RDS")

### SAMPLE DESCRIPTIVES ###

df <-dplyr::select(df, AgeRecod:ideologia,E1, E1Treat, E3Treat:E3Sad, HomoIndex,DigitIndex)


### CREATE BASE VARIABLES


df$GenRecod <-relevel(as.factor(df$GenRecod), ref = "Femenino")
df$AgeRecod <-relevel(as.factor(df$AgeRecod), ref = "18 a 29 años")
df$ideologia <-relevel(as.factor(df$ideologia), ref = "centro")
df$IncomeRecod <-relevel(as.factor(df$IncomeRecod), ref = "Menos de $224.000")
df$E3Treat <-relevel(as.factor(df$E3Treat), ref = "Conocido-Misinfo")
df$E1Treat <-relevel(as.factor(df$E1Treat), ref = "Control")


#Falta educación que ay que reordenar


#df$SexDum <- if_else(df$GenRecod == "Femenino",
                    # true = 1, false = 0)

#df <-fastDummies::dummy_cols(df, select_columns = 'AgeRecod')
#df <-fastDummies::dummy_cols(df, select_columns = 'EducRec')
#df <-fastDummies::dummy_cols(df, select_columns = 'ideologia')
#df <-fastDummies::dummy_cols(df, select_columns = 'IncomeRecod')
#df <-dplyr::select(df, -`AgeRecod_18 a 29 años`, -`EducRec_Sin Estudios`,-ideologia_centro,
#                   -`IncomeRecod_Menos de $224.000`)


# Relevel

df$ideologia<- factor(df$ideologia, levels = c("Ninguno", "centro",  "Derecha", "Izquierda" ))
df$AgeRecod<- factor(df$AgeRecod, levels = c("18 a 29 años", "30 a 40 años", "41 a 65 años", "+66 años"))
df$IncomeRecod<- factor(df$IncomeRecod, levels = c("Menos de $224.000",  "Entre $224.001 - $448.000", "Ente $448.001 y $1.000.000", "Entre $1.000.001 - $3.000.000" ,
                                                   "Más de $3.000.000"))



### TESTING RANDOMIZATION ###

##################
## Experiment 1 ##n
##################

test <-multinom(E3Treat ~ HomoIndex + DigitIndex + GenRecod + AgeRecod + IncomeRecod + ideologia, data = df)

summary(test)

stargazer::stargazer(test,
                     title = "Experiment 1: Balanced test to check",
                     dep.var.caption = "Probability",
                     dep.var.labels = c("Strong Tie/Validated", "Weak Tie/Misinformation", "Weak Tie/Validated"),
                     covariate.labels = c("High Echo Chamber", "High Digital Citizenship", "Men", "Other", "30 to 40 years",
                                          "41 to 65 years", "+65 years", "Low income", "Low-mid",
                                          "Mid-High income","Highest", "Center wing", "Right-Wing","Left-wing",
                                          "Constant"),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     column.sep.width = "10pt",
                     notes.label = "Significance levels",
                     type = "html",
                     out = "Results/Tables/E1_Balanced_test.html")
                     
        
## REGRESSION OF EXPERIMENT 1


#df$E3 <-if_else(df$E3 == "2", true = 1, false = 0) 
df$E3 <-as.numeric(df$E3)


E1M1 <-glm(E3 ~ E3Treat, data = df)

table(df$E3)

summary(E1M1)

E1M1 <-glm(E3 ~ E3Treat, data = df, family = "binomial")
E1M2 <-glm(E3 ~ E3Treat + HomoIndex, data = df, family = "binomial")
E1M3 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat), data = df, family = "binomial")


E1RegBal <-stargazer(E1M1, E1M2, E1M3,
                    title = "Social media Broke ties probabilities",
                    dep.var.caption = "Social media unfollow probabilities",
                    dep.var.labels = c("Model 1", "Model 2", "Model 3"),
                    covariate.labels = c("Strong Tie \\ Arg. Validated", "Weak Tie \\ Misinformation", "Weak Tie \\ Arg. Validated",
                                         "High Eco chamber", "High Digital Citizenship", "Strong Tie \\ Misinfo * High Eco Chamber",
                                         "Strong Tie \\ Validated * High Eco Chamber", "Weak Tie \\ Validated * High EcoChamber",
                                          "Strong Tie\\ Misinfo * High Dig. citizen", "Strong Tie \\Validated * High Dig. Citizen",
                                        "Weak Tie \\ Validated* High Digit. Citizen", "Constante"),
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    column.sep.width = "1pt",
                    notes.label = "Significance levels",
                    type = "html",
                    out = "Results/Tables/E1-balanced.html")


## Marginal effects



## Unbalanced variables

E1M5 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod, data = df, family = "binomial")
E1M6 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod + EducRec, data = df, family = "binomial")
E1M7 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod + EducRec + GenRecod, data = df, family = "binomial")
E1M8 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod + EducRec + GenRecod + ideologia, data = df, family = "binomial")

E1RegNoBal <-stargazer::stargazer(E1M5,E1M6,E1M7,E1M8, out = "Results/Tables/E1_no_balanced.html")


##################
## Experiment 2 ##
##################

# Check variables 

testE2 <-multinom(E1Treat ~ HomoIndex + DigitIndex + GenRecod + AgeRecod + IncomeRecod + ideologia, data = df)


summary(testE2)

stargazer::stargazer(testE2,
                     title = "Experiment 2: Balanced test to check Digital CitizenShip and Echo Chamber membership",
                     dep.var.caption = "Experimental conditions",
                     covariate.labels = c("High Echo Chamber", "High Digital Citizenship", "Men", "Other", "30 to 40 years",
                                          "41 to 65 years", "+65 years", "Low income", "Low-mid",
                                          "Mid-High income","Highest", "Center wing", "Right-Wing","Left-wing",
                                          "Constant"),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     column.sep.width = "10pt",
                     notes.label = "Significance levels",
                     type = "html",
                     out = "Results/Tables/E2_Balanced_test.html")


### Regressiones ###

df$E1 <- as.numeric(df$E1)

lm1  <-lm(E1 ~ E1Treat + E1Treat, data = df)
glm1 <-glm(E1 ~ E1Treat + E1Treat, data = df)
lm2  <-lm(E1 ~ E1Treat + HomoIndex + DigitIndex, data = df)
glm2 <-glm(E1 ~ E1Treat + HomoIndex + DigitIndex, data = df) 
lm3  <-lm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat), data = df)
glm3 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat), data = df)

stargazer::stargazer(lm1, glm1, lm2, glm2, lm3, glm3,
                     title = "Experiment 2: Anger levels by treatment and EchoChamber membership and digital citizenship",
                     dep.var.caption = "Anger levels",
                     covariate.labels = c("T1: Like-Minded", "T2: Opposite", "High Eco Chamber", "High Digital Citizenship", "Like-Minded * High EcoChamber",
                                          "Opposite* High EcoChamber","Like-minded*High Digit. Citizen", "Opposite*High Digit.citizen", "Constant"),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     column.sep.width = "1pt",
                     notes.label = "Significance levels",
                     type = "html",
                     out = "Results/Tables/E2Reg_Balanced.html")
                     

# Regresion with unbalanced covariates

lm4 <-lm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod, data = df)
lm5 <-lm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec, data = df)
lm6 <-lm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod, data = df)
lm7 <-lm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod + ideologia, data = df)

glm4 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod, data = df)
glm5 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec, data = df)
glm6 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod, data = df)
glm7 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod + ideologia, data = df)


E2RegNoBal <-stargazer::stargazer(lm4, glm4, lm5, glm5, lm6, glm6, lm7, glm7,
                                  title = "Experiment 2: Anger levels by treatment with controls",
                                  dep.var.caption = "Anger levels",
                                  covariate.labels = c("T1: Like-Minded", "T2: Opposite", "High Eco Chamber", "High Digital Citizenship",
                                                       "30 to 04 years","41 to 65 years", "+66 Years", "Media", "Postgrado","Sin estudios",
                                                       "Superior", "Masculino", "Otro genero","Center wing", "Right-Wing", "Left Wing",
                                                       "Like-Minded * High EcoChamber","Opposite* High EcoChamber","Like-minded*High Digit. Citizen", 
                                                       "Opposite*High Digit.citizen", "Constant"),
                                  star.cutoffs = c(0.05, 0.01, 0.001),
                                  column.sep.width = "1pt",
                                  notes.label = "Significance levels",
                                  type = "html",
                                  out ="Results/Tables/E2Reg_no_balanced.html")







## Create new data frame


finalDF <-saveRDS(finalDF, file = "Data/Analysis-Data/DF-final.RDS")




