# Set Working Directory
getwd()
setwd("C:/Users/cposick/Documents/Data/MIDUS Data")
getwd()

library(usethis)
use_git_config(user.name = "chadposick", user.email = "cposick@georgiasouthern.edu")

# Clear Environment - Load MIDUS 3 Data
rm(list=ls(all=TRUE))
library(rio)
MIDUS3 <- import("MIDUS 3 Data.rda")
View(MIDUS3)

#Dependent Variables
MIDUS3$currenthealth <- recode(MIDUS3$C1SA1, "-1=NA; 98=NA")

MIDUS3$controlhealth <- as.numeric(MIDUS3$C1SA4)
MIDUS3$controlhealth <- recode(MIDUS3$controlhealth, "-1=NA; 98=NA")
MIDUS3$controlhealth_ln <- log(MIDUS3$controlhealth)

MIDUS3$comparehealth <- as.factor(MIDUS3$C1SA7A)
MIDUS3$comparehealth <- recode(MIDUS3$comparehealth, "1=5; 2=4; 3=3; 4=2; 5=1; -1=NA; 8=NA")

#Focal Independent Variables
MIDUS3$physassault <- as.numeric(MIDUS3$C1SE12L)
MIDUS3$physassault <- recode(MIDUS3$physassault, "1=1; 2=0; -1=NA")
MIDUS3$agephysassault <- recode(MIDUS3$C1SE12L11, "-1=NA; 98:99=NA")

MIDUS3$sexassault <- as.numeric(MIDUS3$C1SE12M)
MIDUS3$sexassault <- recode(MIDUS3$sexassault,"1=1; 2=0; -1=NA") 
MIDUS3$agesexassault <- recode(MIDUS3$C1SE12M11, "-1=NA; 98:99=NA")

#Covariates
MIDUS3$age <- recode(MIDUS3$C1PRAGE, "98:99=NA")
MIDUS3$female <- as.numeric(MIDUS3$C1PRSEX)
MIDUS3$female <- recode(MIDUS3$female, "1=0; 2=1")
MIDUS3$non_white <- as.numeric(MIDUS3$C1PF7A)
MIDUS3$non_white <- recode(MIDUS3$non_white, "1=0; 2:6=1; 7:9=NA")
MIDUS3$education <- as.numeric(MIDUS3$C1PB1)
MIDUS3$education <- recode(MIDUS3$education, "13:hi=NA")
MIDUS3$hhmembers<- MIDUS3$C1PKHSIZ
MIDUS3$eversmoke <- as.numeric(MIDUS3$C1PA38A)
MIDUS3$eversmoke <- recode(MIDUS3$eversmoke, "1=1; 2=0; else=NA")
MIDUS3$agreeable <- recode(MIDUS3$C1SAGREE, "4.1:hi=NA")
MIDUS3$extravert <- recode(MIDUS3$C1SEXTRA, "4.1:hi=NA")
MIDUS3$neurot <- recode(MIDUS3$C1SNEURO, "4.1:hi=NA")
MIDUS3$conscience <- recode(MIDUS3$C1SCONS1, "4.1:hi=NA")
MIDUS3$openness <- recode(MIDUS3$C1SOPEN, "4.1:hi=NA")

#Analysis Packages and Options
library(dplyr)
library(car)
library(psych)
library(pastecs)
library(gmodels)
library(DescTools)
library(ggplot2)
library(GGally)
library(broom)
library(stargazer)
library(reshape2)
library(lme4)
library(compiler)
library(parallel)
library(boot)
library(lattice)
library(sjstats)
library(tidyverse)
library(sandwich)
library(ordinal)
library(QuantPsyc)

options(scipen = 999, digits=2)

#Age of Onset Descriptive Statistics
datadesc <- data.frame(MIDUS3$currenthealth,
                       MIDUS3$controlhealth,
                       MIDUS3$comparehealth,
                       MIDUS3$physassault, 
                       MIDUS3$agephysassault, 
                       MIDUS3$sexassault, 
                       MIDUS3$agesexassault, 
                       MIDUS3$age, MIDUS3$female, 
                       MIDUS3$non_white, MIDUS3$education, 
                       MIDUS3$hhmembers, MIDUS3$eversmoke, 
                       MIDUS3$agreeable, 
                       MIDUS3$extravert, MIDUS3$neurot, 
                       MIDUS3$conscience, MIDUS3$openness)
stargazer(datadesc, 
          title="Table 1. Descriptive Statistics", ci=T, 
          summary.stat = c("n", "mean", "sd", "min", "max"),
          type="text", digits=2, out="TABLE1.txt")

#Age of Onset and Current Health
MODEL1 <- lm(MIDUS3$currenthealth ~ MIDUS3$physassault + MIDUS3$age +
   MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
   MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
   MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness)

summary(MODEL1)
lm.beta(MODEL1)

MODEL2 <- lm(MIDUS3$currenthealth ~ MIDUS3$agephysassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness)

summary(MODEL2)
lm.beta(MODEL2)

MODEL3 <- lm(MIDUS3$currenthealth ~ MIDUS3$sexassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness)

summary(MODEL3)
lm.beta(MODEL3)

MODEL4 <- lm(MIDUS3$currenthealth ~ MIDUS3$agesexassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness)

summary(MODEL4)
lm.beta(MODEL4)

#Age of Onset and Comparative Health
MODEL5 <- clm(MIDUS3$comparehealth ~ MIDUS3$physassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness, 
               link = "logit", data=MIDUS3)

summary(MODEL5)

MODEL6 <- glm(MIDUS3$comparehealth ~ MIDUS3$agephysassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness,
              family="poisson", vcov = sandwich, data=MIDUS3)

summary(MODEL6)

MODEL7 <- glm(MIDUS3$comparehealth ~ MIDUS3$sexassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness,
               family="poisson", data=MIDUS3)

summary(MODEL7)

MODEL8 <- glm(MIDUS3$comparehealth ~ MIDUS3$agesexassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness,
               family="poisson", vcov = sandwich, data=MIDUS3)

summary(MODEL8)

#Age of Onset and Health Autonomy
MODEL9 <- lm(MIDUS3$controlhealth ~ MIDUS3$physassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness)

summary(MODEL9)
lm.beta(MODEL9)

MODEL10 <- lm(MIDUS3$controlhealth ~ MIDUS3$agephysassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness)

summary(MODEL10)
lm.beta(MODEL10)

MODEL11 <- lm(MIDUS3$controlhealth ~ MIDUS3$sexassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness)

summary(MODEL11)
lm.beta(MODEL11)

MODEL12 <- lm(MIDUS3$controlhealth ~ MIDUS3$agesexassault + MIDUS3$age +
               MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
               MIDUS3$eversmoke + MIDUS3$agreeable + MIDUS3$extravert + 
               MIDUS3$neurot + MIDUS3$conscience + MIDUS3$openness)

summary(MODEL12)
lm.beta(MODEL12)

#### Missing Value Analysis
sum(is.na(MIDUS3$controlhealth))
summary(is.na(MIDUS3$controlhealth))

##
MIDUS3$heart <- recode(MIDUS3$C1PA7, "1=1; 2=0; 7:8=NA")
MIDUS3$jail <- recode(MIDUS3$C1SE12O, "1=1; 2=0; -1=NA")
MIDUS3$legal <- recode(MIDUS3$C1SJ11CI, "1=1; 2=0; -1=NA; 3:8=NA")
MIDUS3$heartattack <- recode(MIDUS3$C1PA8, "1=1; 2=0; 7:8=NA")
MIDUS3$police <- recode(MIDUS3$C1SP1H, "-1=NA; 9998=NA")

MODEL1 <- glm(MIDUS3$heartattack ~ MIDUS3$sexassault + MIDUS3$physassault +
                MIDUS3$female + MIDUS3$non_white + MIDUS3$education + MIDUS3$hhmembers + 
                MIDUS3$police, family=binomial, data=MIDUS3)

MODEL1 <- glm(MIDUS3$heartattack ~ MIDUS3$legal*MIDUS3$non_white,
               family="binomial", data=MIDUS3)

summary(MODEL1)
