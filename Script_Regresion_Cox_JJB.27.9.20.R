base<-C19_CLI_FIN_RPC_20_SEP_
str(base)
######### MAIN DATABASE ##############
# Data configuration ####
base$`Year-2`<- factor(base$`Year-2`, 
                       levels =c(0,1),
                       labels = c("<= 60 years","> 60 years"))
base$Gender<-factor(base$Gender,
                  levels = c(0,1),
                  labels = c("Female","Male"))
base$HTA<-factor(base$HTA,
                 levels = c(0,1),
                 labels = c("No","Yes"))
base$DM<-factor(base$DM,
                levels = c(0,1),
                labels = c("No","Yes"))
base$`NM-G`<-factor(base$`NM-G`,
                    levels = c(0,1),
                    labels = c("No","Yes"))
base$CIRR<-factor(base$CIRR,
                  levels = c(0,1),
                  labels = c("No","Yes"))
base$`ERC-G`<-factor(base$`ERC-G`,
                     levels = c(0,1),
                     labels = c("No","Yes"))
base$`ERC-V`<-factor(base$`ERC-V`,
                     levels = c(0,1),
                     labels = c("No","Yes"))
base$HIPOT<-factor(base$HIPOT,
                   levels = c(0,1),
                   labels = c("No","Yes"))
base$OBES<-factor(base$OBES,
                  levels = c(0,1),
                  labels = c("No","Yes"))
base$VIH<-factor(base$VIH,
                  levels = c(0,1),
                  labels = c("No","Yes"))
base$FA<-factor(base$FA,
                levels = c(0,1),
                labels = c("No","Yes"))
base$Asthma<-factor(base$Asthma,
                  levels = c(0,1),
                  labels = c("No","Yes"))
base$Severity[base$Severity=="X"]<-NA
is.na(base$Severity)<-base$Severity=="NA"
base$Severity<-factor(base$Severity,
                       levels = c(0,1,2,3),
                       labels = c("Mild","Moderate","Severe","Critical"))
# Bivariate
base$Outcome<-factor(base$Outcome,
                       levels = c(1,0),
                       labels = c("Death","Discharge"))
# Evaluation of continuous variable (age) ####
library(nortest)
lillie.test(base$Years)
library(car)
leveneTest(y=base$Years,
           group = base$Outcome,
           center = "mean")
t.test(base$Years~base$Outcome,
       alternative = "two.sided", 
       var.equal = TRUE,
       mu=0,
       conf.level = 0.95)

#Creating new variable
grupo_Discharge<-base[base$Outcome=="Discharge",]
grupo_Death<-base[base$Outcome=="Death",]
summary(grupo_Discharge$Length)
summary(grupo_Death$Length)


# Bivariate analysis ####
library(tidyverse)
library(tableone)
table1=CreateTableOne(data=base,strata = "Outcome")
table1=print(tabla1,showAllLevels=T)
write.csv(table1,file = "Table1_biv.csv")

# Multivariate analysis and Cox Regression ####
library(survival)
library(KMsurv)
library(survMisc)
library(ggpubr)
library(ggplot2)
library(survminer)
library(ggfortify)
library(flexsurv)
library(dplyr)
### IMPORTANT: The variable UNLABELLED must be factored out, so that only
### appears as 1 and 0
# STEP 1: Bivariate test for outcome by covariates:
regresion_cox_year_general=
  coxph(Surv(Length, 
             Outcome) ~
          Year, 
        data = base)
summary(regresion_cox_Year_general)

regresion_cox_Year=
  coxph(Surv(Length, 
             Outcome) ~
          `Year-2`, 
        data = base)
summary(regresion_cox_Year)

regresion_cox_Gender=
  coxph(Surv(Length, 
             Outcome) ~
          Gender, 
        data = base)
summary(regresion_cox_Gender)

regresion_cox_HTA=
  coxph(Surv(Length, 
             Outcome) ~
          HTA, 
        data = base)
summary(regresion_cox_HTA)

regresion_cox_DM=
  coxph(Surv(Length, 
             Outcome) ~
          DM, 
        data = base)
summary(regresion_cox_DM)

regresion_cox_OBES=
  coxph(Surv(Length, 
             Outcome) ~
          OBES, 
        data = base)
summary(regresion_cox_OBES)

regresion_cox_HIV=
  coxph(Surv(Length, 
             Outcome) ~
          HIV, 
        data = base)
summary(regresion_cox_VIH)

regresion_cox_CIRR=
  coxph(Surv(Length, 
             Outcome) ~
          base$CIRR, 
        data = base)
summary(regresion_cox_CIRR)

regresion_cox_NM=
  coxph(Surv(Length, 
             Outcome) ~
          base$`NM-G`, 
        data = base)
summary(regresion_cox_NM)

regresion_cox_ERC_G=
  coxph(Surv(Length, 
             Outcome) ~
          base$`ERC-G`, 
        data = base)
summary(regresion_cox_ERC_G)

regresion_cox_ERC_V=coxph(Surv(Length, Outcome) ~base$`ERC-V`, data = base)
summary(regresion_cox_ERC_V)

regresion_cox_FA=coxph(Surv(Length, Outcome) ~FA, data = base)
summary(regresion_cox_FA)

regresion_cox_HIPOT=coxph(Surv(Length, Outcome) ~HIPOT, data = base)
summary(regresion_cox_HIPOT)

regresion_cox_Asthma=coxph(Surv(Length, Outcome) ~Asthma, data = base)
summary(regresion_cox_Asthma)

regresion_cox_severity=coxph(Surv(Length, Outcome) ~base$Severity, data = base)
summary(regresion_cox_severity)



