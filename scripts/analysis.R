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
              "vcd", "plyr", "ggannotate","scales", "fastDummies","gt", "MASS", "modelsummary", "nnet", "margins", "socviz")
ipak(packages)

# load DF

df <-readRDS("data/Final Data/DF-final.RDS")

table(df$ideologia)

### SAMPLE DESCRIPTIVES ###

df <-dplyr::select(df, AgeRecod:ideologia,E1, E1Treat, E3Treat:E3Sad, HomoIndex,DigitIndex)

df <-df[!(df$GenRecod=="Otro" | df$EducRec=="Sin Estudios"),] #eliminate "others" in genre and "withoutstudies" due to very small numbers of cases

df <-mutate(df, EducRec = dplyr::recode(df$EducRec,"Básica" = "1", "Media" = "1", "Superior" = "2", "Postgrado" = "3"))

table(df$EducRec)


### CREATE BASE VARIABLES


df$GenRecod <-relevel(factor(df$GenRecod), ref = "Femenino")
df$AgeRecod <-relevel(factor(df$AgeRecod), ref = "18 a 29 años")
df$ideologia <-relevel(factor(df$ideologia), ref = "Ninguno")
df$IncomeRecod <-relevel(factor(df$IncomeRecod), ref = "Menos de $224.000")
df$E3Treat <-relevel(factor(df$E3Treat), ref = "Conocido-Misinfo")
df$E1Treat <-relevel(factor(df$E1Treat), ref = "Control")
df$EducRec <-relevel(factor(df$EducRec), ref = "1")



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

df$ideologia<- factor(df$ideologia, levels = c("Ninguno", "E. Izquierda", "Centro-Izquierda", "Centro", "Centro-Derecha", "E. Derecha"))
df$AgeRecod<- factor(df$AgeRecod, levels = c("18 a 29 años", "30 a 40 años", "41 a 65 años", "+66 años"))
df$IncomeRecod<- factor(df$IncomeRecod, levels = c("Menos de $224.000",  "Entre $224.001 - $448.000", "Ente $448.001 y $1.000.000", "Entre $1.000.001 - $3.000.000" ,
                                                   "Más de $3.000.000"))



### TESTING RANDOMIZATION ###

##################
## Experiment 1 ##n
##################

test <-multinom(E3Treat ~ HomoIndex + DigitIndex + GenRecod + AgeRecod + IncomeRecod + ideologia + EducRec, data = df)

summary(test)

stargazer::stargazer(test,
                     title = "Experiment 1: Balanced test to check",
                     dep.var.caption = "Probability",
                     dep.var.labels = c("Strong Tie/Validated", "Weak Tie/Misinformation", "Weak Tie/Validated"),
                     covariate.labels = c("High Echo Chamber", "High Digital Citizenship", "Men", "30 to 40 years",
                                          "41 to 65 years", "+66 years", "Low income", "Low-mid",
                                          "Mid-High income","Highest", "Far Left", "Center-left","Center","Center-right","Far right",
                                          "Undergraduate","Graduate","Constant"),
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

E1M1 <-glm(E3 ~ E3Treat, data = df, family = "binomial"(link = "logit"))
E1M2 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex, data = df, family = "binomial"(link = "logit"))
E1M3 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat), data = df, family = "binomial"(link = "logit"))


E1RegBal <-stargazer(E1M1, E1M2, E1M3,
                    title = "Social media Broke ties probabilities",
                    dep.var.caption = "Social media unfollow probabilities",
                    dep.var.labels = c("Model 1", "Model 2", "Model 3"),
                    covariate.labels = c("Strong Tie \\ Misinfo", "Strong Tie \\ Arg. Validated","Weak Tie \\ Arg. Validated",
                                         "High Eco chamber", "High Digital Citizenship", "Strong Tie \\ Misinfo * High Eco Chamber",
                                         "Strong Tie \\ Validated * High Eco Chamber", "Weak Tie \\ Validated * High EcoChamber",
                                          "Strong Tie\\ Misinfo * High Dig. citizen", "Strong Tie \\Validated * High Dig. Citizen",
                                        "Weak Tie \\ Validated* High Digit. Citizen", "Constant"),
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    column.sep.width = "1pt",
                    notes.label = "Significance levels",
                    type = "html",
                    out = "Results/Tables/E1-balanced.html")


## Marginal effects

summary(E1M3)

test1 <-margins(E1M3)
summary(test1)

plot(test1)

test2 <-as_tibble(summary(test1))
prefixes <-c("E3Treat", "HomoIndex", "DigitIndex")
test2$factor <- prefix_replace(test2$factor, "E3Treat", "Treatment:  ")
test2 %>% dplyr::select(factor, AME, lower, upper) 


p1 <- ggplot(data = test2, aes(x = reorder(factor, AME),
                              y = AME, ymin = lower, ymax = upper))

p1 + geom_hline(yintercept = 0, color = "green") +
  geom_pointrange() + coord_flip() +
  labs(x = NULL, y = "Average Marginal Effect") 

## Unbalanced variables

E1M5 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod, data = df, family = "binomial"(link = "logit"))
E1M6 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod + EducRec, data = df, family = "binomial"(link = "logit"))
E1M7 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod + EducRec + GenRecod, data = df, family = "binomial"(link = "logit"))
E1M8 <-glm(E3 ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod + EducRec + GenRecod + ideologia + IncomeRecod,
           data = df, family = "binomial"(link = "logit"))

E1RegNoBal <-stargazer::stargazer(E1M5,E1M6,E1M7,E1M8, out = "Results/Tables/E1_no_balanced.html")

## Marginal effects from  non-balanced

summary(E1M8)


noba <-margins(E1M8)
summary(noba)

plot(noba)
noba2 <-as.tibble(summary(noba))

noba2 <-noba2%>%
  dplyr::filter(noba2$p <= 0.05)

summary(noba2)


noba2$factor <- prefix_replace(noba2$factor,"E3Treat", "Treatment:  ")
noba2 %>% dplyr::select(factor, AME, lower, upper) 


p <- ggplot(data = noba2, aes(x = reorder(factor, AME),
                              y = AME, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, color = "green") +
  geom_pointrange() + coord_flip() +
  scale_x_discrete(labels = c("+66 Years","30 to 40 years","41 to 65 years","Strong Tie \n Arg. Validated","Male","High Eco Chamber", "Center-Left", "Far Left")) +
  geom_text(aes(label = round(AME,3)), 
            size = 5,hjust = 1.8)+
   labs(x = NULL, y = "Average Marginal Effect",
       title = "Experiment 1 - Marginal Effects to unfollow friends on social media",
       caption = "All variables are p < 0.05 \n Base variables: Without Ideology, low Echo Chamber, Female, Weak tie /Misinfo, 18 to 29 years")+
  theme(axis.text = element_text(size = 15, hjust = 1.2),
        axis.title= element_text(size=16,face="bold"),
        plot.title = element_text(size = 18, face = "bold", hjust = .5),
        plot.caption = element_text(size = 12))+
  guides(color = "none")


ggsave(p, filename = "Results/Plots/E1_Marginal_effects.png",
       dpi = 600, width = 15, height = 10)


### eMOTIONS WITHOUT controls

df$E3Treat <-relevel(factor(df$E3Treat), ref = "Conocido-Misinfo")


MBAHappy <-lm(E3Joy ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat), data = df)
MBAAnger <-lm(E3Angry ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat), data = df)
MBASad <-lm(E3Sad ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat), data = df)
MBAFear <-lm(E3Fear ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat), data = df)


stargazer::stargazer(MBAHappy, MBAAnger, MBASad, MBAFear,
                     title = "Experiment 1 - Emotions During social media discussions",
                     dep.var.caption = "OLS of Emotions",
                     dep.var.labels = c("Hapiness", "Anger", "Sadness", "Fear"),
                     covariate.labels = c("Stron Tie \\ Misinfo","Strong Tie \\ Arg. Validated", "Weak Tie \\ Validated",
                                          "High Eco chamber", "High Digital Citizenship", "Strong Tie \\ Misinfo * High Eco Chamber",
                                          "Strong Tie \\ Validated * High Eco Chamber", "Weak Tie \\ Validated * High Eco Chamber",
                                          "Strong Tie\\ Validated * High Dig. citizen", "Strong Tie \\Misinfo * High Dig. Citizen",
                                          "Weak Tie \\ Validated* High Digit. Citizen", "Constant"),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     column.sep.width = "1pt",
                     notes.label = "Significance levels",
                     type = "html",
                     out = "Results/Tables/E1Reg_Emotions_Balanced.html")



### EMOTIONS REGRESSION - controls

df$E3Treat <-relevel(factor(df$E3Treat), ref = "Conocido-Misinfo")


MHappy <-lm(E3Joy ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod +
                   EducRec + GenRecod + ideologia + IncomeRecod, data = df)
MAnger <-lm(E3Angry ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod +
               EducRec + GenRecod + ideologia + IncomeRecod, data = df)

MSad <-lm(E3Sad ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod +
             EducRec + GenRecod + ideologia + IncomeRecod, data = df)
MFear <-lm(E3Fear ~ E3Treat + (HomoIndex*E3Treat) + (DigitIndex*E3Treat) + AgeRecod +
              EducRec + GenRecod + ideologia + IncomeRecod, data = df)


stargazer::stargazer(MHappy, MAnger, MSad, MFear,
                     title = "Experiment 1 - Emotions During social media discussions",
                     dep.var.caption = "Robust Lineal models of Emotions",
                     dep.var.labels = c("Hapiness", "Anger", "Sadness", "Fear"),
                     covariate.labels = c("Strong Tie \\ Misinfo","Strong Tie \\ Arg. Validated", "Weak Tie \\ Validated",
                                          "High Eco chamber", "High Digital Citizenship", "30 to 40 years", "41 to 65 years",
                                          "+66 years", "Undergraduate","Graduate", "Male","Far Left", "Center-Left","Center",
                                          "Center-right","Far Right","Low Income","Low-Mid Income","Mid-High income","Highest",
                                          "Strong Tie \\ Misinfo * High Eco Chamber",
                                          "Strong Tie \\ Validated * High Eco Chamber", "Weak Tie \\ Validated * High Eco Chamber",
                                          "Strong Tie\\ Misinfo * High Dig. citizen", "Strong Tie \\ Validated * High Dig. Citizen",
                                          "Weak Tie \\ Validated* High Digit. Citizen", "Constant"),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     column.sep.width = "1pt",
                     notes.label = "Significance levels",
                     type = "html",
                     out = "Results/Tables/E1Reg_Emotions_Unbalanced.html")



##################
## Experiment 2 ##
##################

# Check variables 

testE2 <-multinom(E1Treat ~ HomoIndex + DigitIndex + GenRecod + AgeRecod + IncomeRecod + ideologia + EducRec, data = df)


summary(testE2)

stargazer::stargazer(testE2,
                     title = "Experiment 2: Balanced test to check Digital CitizenShip and Echo Chamber membership",
                     dep.var.caption = "Experimental conditions",
                     covariate.labels = c("High Echo Chamber", "High Digital Citizenship", "Men","30 to 40 years",
                                          "41 to 65 years", "+65 years", "Low income", "Low-mid",
                                          "Mid-High income","Highest", "Center wing", "Right-Wing","Left-wing","Undergraduate",
                                          "Graduate", "Constant"),
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

stargazer::stargazer(glm1,glm2,glm3,
                     title = "Experiment 2: Anger levels by treatment and EchoChamber membership and digital citizenship",
                     dep.var.caption = "Anger levels",
                     covariate.labels = c("T1: Like-Minded", "T2: Opposite", "High Eco Chamber", "High Digital Citizenship", "Like-Minded * High EcoChamber",
                                          "Opposite* High EcoChamber","Like-minded*High Digit. Citizen", "Opposite*High Digit.citizen", "Constant"),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     column.sep.width = "1pt",
                     notes.label = "Significance levels",
                     out = "Results/Tables/E2Reg_Balanced.html")
                     

# Regresion with unbalanced covariates

lm4 <-rlm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod, data = df)
lm5 <-rlm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec, data = df)
lm6 <-rlm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod, data = df)
lm7 <-rlm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod + ideologia, data = df)
lm8 <-rlm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod + ideologia + EducRec, data = df)


options(scipen = 999)
or <-polr(as.factor(E1) ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod + ideologia + EducRec,
          data = df, Hess = TRUE)

modelsummary::modelsummary(or)

ctable <-coef(summary(or))

p <-(pnorm(abs(ctable[,"t value"]), lower.tail = FALSE)*2)

ctable<-cbind(ctable, "pvalue" = p)

glm4 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod, data = df)
glm5 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec, data = df)
glm6 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod, data = df)
glm7 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod + ideologia, data = df)
glm8 <-glm(E1 ~ E1Treat + (HomoIndex*E1Treat) + (DigitIndex*E1Treat) + AgeRecod + EducRec + GenRecod + ideologia + EducRec, data = df)


summary(glm8)
E2RegNoBal <-stargazer::stargazer(lm4,glm4,lm5,glm5,lm6,glm6, lm7,glm7,lm8,glm8,
                                  title = "Experiment 2: Anger levels by treatment with controls",
                                  dep.var.caption = "Anger levels",
                                  covariate.labels = c("T1: Like-Minded", "T2: Opposite", "High Eco Chamber", "High Digital Citizenship",
                                                       "+66 years", "30 to 04 years","41 to 65 years", "Undergraduate", "Graduate","Male",
                                                       "Right wing", "Left Wing", "Without ideology",
                                                       "Like-Minded * High EcoChamber","Opposite* High EcoChamber","Like-minded*High Digit. Citizen", 
                                                       "Opposite*High Digit.citizen","Constant"),
                                  star.cutoffs = c(0.05, 0.01, 0.001),
                                  column.sep.width = "1pt",
                                  notes.label = "Significance levels",
                                  type = "html",
                                  out ="Results/Tables/E2Reg_no_balanced.html")





