

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
CCBHC
```{r}
setwd("P:/Evaluation/CCBHC IN/Data/SPARS Data Downloads")
CCBHC_dat = read.csv("Adult youth combined data 2.4.20.csv", header = TRUE, na.strings = c(-99, -98, -97, -1, -4, -5, -7, -9, -2, -6, -8, " "))

```
Look at the variaables
```{r}
### Get baseline data
CCBHC_dat_base = subset(CCBHC_dat, InterviewType_07 == 1)
dim(CCBHC_dat_base)
library(prettyR)
describe.factor(CCBHC_dat_base$NightsHospitalMHC)
head(CCBHC_dat_base)
library(pscl)
#TimesER
CCBHC_dat_base$TimesER
describe.factor(CCBHC_dat_base$RxOpioids_Use)
CCBHC_model = hurdle(TimesER ~ Gender  + Tobacco_Use + Alcohol_Use  + NightsHomeless + LivingConditionsSatisfaction  +Employment + NumTimesArrested +EnoughMoneyForNeeds + Hopeless, data = CCBHC_dat_base, dist = "negbin", zero.dist = "binomial")
library(car)
vif(CCBHC_model)
summary(CCBHC_model)
```
Create a standardized variable then try t predict whether someone is going to the ER or not
Alcohol_Use, EnoughMoneyForNeeds, LivingConditionsSatisfaction
```{r}
CCBHC_dat_base_score = data.frame(Alcohol_Use = CCBHC_dat_base$Alcohol_Use, EnoughMoneyForNeeds = CCBHC_dat_base$EnoughMoneyForNeeds,LivingConditionsSatisfaction = CCBHC_dat_base$LivingConditionsSatisfaction, NightsHospitalMHC = CCBHC_dat_base$NightsHospitalMHC)
CCBHC_dat_base_score_complete = na.omit(CCBHC_dat_base_score)
dim(CCBHC_dat_base_score_complete)

CCBHC_dat_base_score_complete$total_score = rowSums(CCBHC_dat_base_score_complete[,1:3])

CCBHC_dat_base_score_complete$NightsHospitalMHC = ifelse(CCBHC_dat_base_score_complete$NightsHospitalMHC > 0, 1,0)
#CCBHC_dat_base_score_complete$NightsHospitalMHC = as.factor(CCBHC_dat_base_score_complete$NightsHospitalMHC)
library(pROC)
describe.factor(CCBHC_dat_base_score_complete$NightsHospitalMHC)

roc_hosptial = roc(CCBHC_dat_base_score_complete$total_score, CCBHC_dat_base_score_complete$NightsHospitalMHC)

plot(roc_hosptial)
```

