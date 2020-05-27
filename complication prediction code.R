## PACKAGES
library(foreign)
library(dplyr)
library(haven)
library(ggplot2)
library(gridExtra)
library(Amelia)
library(tidyverse)
library(ggcorrplot)
library(lsr)
library(car)
library(faraway)
# missing data:
library(mice)
library(VIM)
library(naniar)
library(caret)
library("pROC")
library(Hmisc)
library(glmnet)
library(SuperLearner)
library(randomForest)
library(snow)

# citation("pROC")

## LOAD DATA
data <- read_sav("C:/Users/Bou/Documents/Stat Science/THESIS/Data/Predictiemodel clean June 2019.sav")

data <- data %>% rename(age = xleeftijd,
                sex = geslacht,
                length = xlengte,
                weight = xgewicht,
                wtloss = xgewverl)

## SELECT VARIABLES (FOR BOTH RESECTION TYPES, SPLIT COMES LATER)
data <- 
  data %>%
  select(
    
    ### PREDICTORS/FEATURES
    # General
    # jaarok, 
    age, sex, length, weight, wtloss, 
    
    # Comorbidity (Main Groups):
    comorb, comorbcar, comorbvas, comorbdia, comorbneu, comorbmda,comorburo, 
    comorbtro, comorbmus, comorbend, comorbinf, comorbmal, comorbove,
    
    # Patient had former chest or stomach surgery
    anamok, anamok1, anamok2, anamok3, anamok4, anamok5, anamok6, anamok7,
    
    # Number of surgeries in history
    xokvg,
    
    # Does patient use inhibitory medication
    anammed2,
    
    # adenocarcinoma & plaveiselcarcinoma (soort weefsel)
    xtumhist,
    
    # oesophagus or stomach tumor (1 = slokdarm, 2 = maag)
    tumbulk, xtumlok1, xtumlok2,
    
    # T classification (1 = maag)
    xscorect, xscorect1, scorecn, scorecn1, scorecm,
    
    # is patient pre-treated and how (0 = no, 1 = , 2 = )
    xneoadjtype,
    
    # ASA Score (fitness of patient)
    asascore,
    
    # goal of surgery: curative, prophylactic, palliative
    curaok,
    
    # Type of surgery
    resecok, procok, recontype, analig,
    
    ### OUTCOMES
    # post-operative complication:
    compl,
    
    # anastomotic leakage
    compnlek, 
    # pulmonary complication
    comppulm, 
    # Chance of re-operation, reason re-operation
    reint, aardrei3, rederei,
    # expected days of hospitalization
    xovlopnduur
    
  )

## CORRECT DATA TYPES
# data$jaarok <- factor(data$jaarok)
data$sex <- as_factor(data$sex)

data$comorb <- as_factor(data$comorb)
# Replace NA's when comorb = no
data <- data %>%
  mutate(comorbvas = replace(comorbvas, comorb == "Geen comorbiditeit", "Nee"),
         comorbcar = replace(comorbcar, comorb == "Geen comorbiditeit", "Nee"),
         comorbdia = replace(comorbdia, comorb == "Geen comorbiditeit", "Nee"),
         comorbneu = replace(comorbneu, comorb == "Geen comorbiditeit", "Nee"),
         comorbmda = replace(comorbmda, comorb == "Geen comorbiditeit", "Nee"),
         comorburo = replace(comorburo, comorb == "Geen comorbiditeit", "Nee"),
         comorbtro = replace(comorbtro, comorb == "Geen comorbiditeit", "Nee"),
         comorbmus = replace(comorbmus, comorb == "Geen comorbiditeit", "Nee"),
         comorbend = replace(comorbend, comorb == "Geen comorbiditeit", "Nee"),
         comorbinf = replace(comorbinf, comorb == "Geen comorbiditeit", "Nee"),
         comorbove = replace(comorbove, comorb == "Geen comorbiditeit", "Nee"))

data$comorbcar <- as_factor(data$comorbcar)
data$comorbvas <- as_factor(data$comorbvas)
data$comorbdia <- as_factor(data$comorbdia)
data$comorbneu <- as_factor(data$comorbneu)
data$comorbmda <- as_factor(data$comorbmda)
data$comorburo <- as_factor(data$comorburo)
data$comorbtro <- as_factor(data$comorbtro)
data$comorbmus <- as_factor(data$comorbmus)
data$comorbend <- as_factor(data$comorbend)
data$comorbinf <- as_factor(data$comorbinf)
data$comorbove <- as_factor(data$comorbove)


# Replace onbekend with NA in comorbmal, drop unused levels
data$comorbmal <- as_factor(data$comorbmal)
data <- data %>%
  mutate(comorbmal = replace(comorbmal, comorbmal == "Onbekend", NA))
data$comorbmal <- droplevels(data$comorbmal)

data$anamok <- as_factor(data$anamok)
# Replace NA's when anamok = no
data <- data %>%
  mutate(anamok1 = replace(anamok1, anamok == "Nee", "Nee"),
         anamok2 = replace(anamok2, anamok == "Nee", "Nee"),
         anamok3 = replace(anamok3, anamok == "Nee", "Nee"),
         anamok4 = replace(anamok4, anamok == "Nee", "Nee"),
         anamok5 = replace(anamok5, anamok == "Nee", "Nee"),
         anamok6 = replace(anamok6, anamok == "Nee", "Nee"),
         anamok7 = replace(anamok7, anamok == "Nee", "Nee"))
data$anamok1 <- as_factor(data$anamok1)
data$anamok2 <- as_factor(data$anamok2)
data$anamok3 <- as_factor(data$anamok3)
data$anamok4 <- as_factor(data$anamok4)
data$anamok5 <- as_factor(data$anamok5)
data$anamok6 <- as_factor(data$anamok6)
data$anamok7 <- as_factor(data$anamok7)
#
data <- data %>%
  mutate(xokvg = replace(xokvg, anamok == "Nee", 0))
#
data$anammed2 <- as_factor(data$anammed2)
data$xtumhist <- as_factor(data$xtumhist)
data$tumbulk <- as_factor(data$tumbulk)
data$tumbulk <- droplevels(data$tumbulk)
# alleen voor slokdarm data
data$xtumlok1 <- as_factor(data$xtumlok1)
data$xtumlok1 <- droplevels(data$xtumlok1)
# alleen voor maag data
data$xtumlok2 <- as_factor(data$xtumlok2)
data$xtumlok2 <- droplevels(data$xtumlok2)

# alleen voor slokdarm data
data$xscorect <- as_factor(data$xscorect)
data <- data %>%
  mutate(xscorect = replace(xscorect, xscorect == "Tx = status onbekend", NA))
data$xscorect <- droplevels(data$xscorect)
# alleen voor maag data
data$xscorect1 <- as_factor(data$xscorect1)
data <- data %>%
  mutate(xscorect1 = replace(xscorect1, xscorect1 == "Tx = status onbekend", NA))
data$xscorect1 <- droplevels(data$xscorect1)

# alleen voor slokdarm data
data$scorecn <- as_factor(data$scorecn)
data <- data %>%
  mutate(scorecn = replace(scorecn, scorecn == "Nx = cN status onbekend", NA))
data <- data %>%
  mutate(scorecn = replace(scorecn, scorecn == "N+ = klin. pathologische lymfeklieren, onbekend hoeveel", NA))
data$scorecn <- droplevels(data$scorecn)


# alleen voor maag data
data$scorecn1 <- as_factor(data$scorecn1)
data <- data %>%
  mutate(scorecn1 = replace(scorecn1, scorecn1 == "Nx = cN status onbekend", NA))
data <- data %>%
  mutate(scorecn1 = replace(scorecn1, scorecn1 == "N+ = klin. pathologische lymfeklieren, onbekend hoeveel", NA))
data$scorecn1 <- droplevels(data$scorecn1)


#
data$scorecm <- as_factor(data$scorecm)
levels(data$scorecm) <- c("Nee", "Ja", "9")
data <- data %>%
  mutate(scorecm = replace(scorecm, scorecm == "9", NA))
data$scorecm <- droplevels(data$scorecm)

# 
data$xneoadjtype <- as_factor(data$xneoadjtype)
data$xneoadjtype <- droplevels(data$xneoadjtype)
#
data$asascore <- as_factor(data$asascore)
data$asascore <- droplevels(data$asascore)
#
data$curaok <- as_factor(data$curaok)

data$resecok <- as_factor(data$resecok)
data$resecok <- droplevels(data$resecok)

#
data$procok <- as_factor(data$procok)
data$procok <- droplevels(data$procok)
#
data$recontype <- as_factor(data$recontype)
data <- data %>%
  mutate(recontype = replace(recontype, recontype == "Onbekend", NA))
data$recontype <- droplevels(data$recontype)
#
data$analig <- as_factor(data$analig)
data <- data %>%
  mutate(analig = replace(analig, analig == "Onbekend", NA))
data$analig <- droplevels(data$analig)

#
data$compl <- as_factor(data$compl)
data <- data %>%
  mutate(compl = replace(compl, compl == "Onbekend", NA))
data$compl <- droplevels(data$compl)

data <- data %>%
  mutate(compnlek = replace(compnlek, compl == "Nee", "Nee"),
         comppulm = replace(comppulm, compl == "Nee", "Nee"))
#
data$compnlek <- as_factor(data$compnlek)
data <- data %>%
  mutate(compnlek = replace(compnlek, compnlek == "Onbekend", NA))
data$compnlek <- droplevels(data$compnlek)
#
data$comppulm <- as_factor(data$comppulm)
data$comppulm <- droplevels(data$comppulm)

#
data$reint <- as_factor(data$reint)
data <- data %>%
  mutate(reint = replace(reint, reint == "Onbekend", NA))
data$reint <- droplevels(data$reint)
#
data$aardrei3 <- as_factor(data$aardrei3)
data <- data %>%
  mutate(aardrei3 = replace(aardrei3, reint == "Nee", "Nee"),
         aardrei3 = replace(aardrei3, aardrei3 == "Onbekend", NA))
data$aardrei3 <- droplevels(data$aardrei3)
#
data$rederei <- as_factor(data$rederei)
data <- data %>%
  mutate(rederei = replace(rederei, rederei == "Onbekend", NA),
         rederei = replace(rederei, rederei == "9999", NA))
data$rederei <- droplevels(data$rederei)

## Consolidate classes BEFORE SPLIT
# comorbmal:
table(data$comorbmal)
levels(data$comorbmal)
data$comorbmal <-  recode(data$comorbmal, 
                          'c("Actueel (nog te behandelen)", "Palliatief behandeld") = "Actueel";
                          c("Curatief behandeld < 5 jaar geleden", "Curatief behandeld > 5 jaar geleden") = "Behandeld"')
# xtumlok1
table(data$xtumlok1)
levels(data$xtumlok1) <- c("Proximale thoracale slokdarm",
                           "Mid thoracale slokdarm",
                           "Distale slokdarm",
                           "Slokdarm-maag overgangstumor")
levels(data$xtumlok1)
data$xtumlok1 <-  recode(data$xtumlok1, 
                          'c("Mid thoracale slokdarm", "Proximale thoracale slokdarm") = "Proximale/Mid thoracale slokdarm"')
# xscorect
table(data$xscorect)
levels(data$xscorect) <- c("T0 Geen tumor, Tis is carcinoma in situ, hooggradige dysplasie",
                           "T1 Ingroei in lamina propria, musccularis mucosase of submucosa",
                           "T2 Ingroei in muscularis propria",
                           "T3 Ingroei in adventitia",
                           "T4 Ingroei in omliggende structuren"
                           )
data$xscorect <-  recode(data$xscorect, 
                         'c("T0 Geen tumor, Tis is carcinoma in situ, hooggradige dysplasie", 
                         "T1 Ingroei in lamina propria, musccularis mucosase of submucosa") = "T0/T1 Ingroei in lamina propria, musccularis mucosase of submucosa"')
# xscorect1
table(data$xscorect1)
levels(data$xscorect1) <- c("T0 Geen tumor, Tis is carcinoma in situ, hooggradige dysplasie",
                           "T1 Ingroei in lamina propria, musccularis mucosase of submucosa",
                           "T2 Ingroei in muscularis propria",
                           "T3 Ingroei in adventitia",
                           "T4 Ingroei in omliggende structuren"
)
data$xscorect1 <-  recode(data$xscorect1, 
                         'c("T0 Geen tumor, Tis is carcinoma in situ, hooggradige dysplasie", 
                         "T1 Ingroei in lamina propria, musccularis mucosase of submucosa") = "T0/T1 Ingroei in lamina propria, musccularis mucosase of submucosa"')
#
levels(data$asascore)
data$asascore <-  recode(data$asascore, 
                          'c("III - ernstige systemische ziekte", "IV - constant levensbedreigende systemische ziekte") = 
                         "III - ernstige systemische ziekte/IV - constant levensbedreigende systemische ziekte"')
# exclude xneoadjtype
table(data$xneoadjtype)
levels(data$xneoadjtype)
data <- data %>% filter(xneoadjtype != "Radiotherapie" )
data$xneoadjtype <- droplevels(data$xneoadjtype)

# procok
table(data$procok)
levels(data$procok)
data$procok <-  recode(data$procok, 'c("Minimaal invasief thorax", "Minimaal invasief thorax en abdomen") = 
                       "Minimaal invasief thorax en/of abdomen"')


# Split data in location of tumor, ESOPHAGUS
data_slok <- data %>%
  filter(tumbulk == "Slokdarm (inclusief slokdarm-maagovergang)") %>%
  select(-c(tumbulk, xtumlok2, xscorect1, scorecn1, curaok, anamok, comorb, aardrei3, rederei, xokvg, compl, xovlopnduur, reint))

# Split data in location of tumor, STOMACH
data_maag <- data %>%
  filter(tumbulk == "Maag") %>%
  select(-c(tumbulk, xtumlok1, xscorect, scorecn, curaok, anamok, comorb, aardrei3, rederei, xokvg, compl, xovlopnduur, reint))

## Consolidate classes AFTER SPLIT

# recontype esophagus
table(data_slok$recontype)
levels(data_slok$recontype)
data_slok$recontype <-  recode(data_slok$recontype, 'c("Geen reconstructie", "Coloninterponaat", "Jejunuminterponaat",
                          "Oesofagojejunostomie (Roux-Y)", "Gastroenterostomie (BII of Roux-Y)", "Anders") = "Overig"')
# recontype stomach
table(data_maag$recontype)
levels(data_maag$recontype)
data_maag$recontype <-  recode(data_maag$recontype, 
                               'c("Geen reconstructie", "Buismaag", "Coloninterponaat", "Jejunuminterponaat", "Anders") = "overig"')
# resecok esophagus
table(data_slok$resecok)
data_slok <- data_slok %>% filter(resecok != "Totale maagresectie" & resecok != "Partiele maagresectie")
data_slok$resecok <- droplevels(data_slok$resecok)
# resecok stomach
table(data_maag$resecok)
data_maag <- data_maag %>% filter(resecok != "Transhiatale slokdarmresectie" & resecok != "Transthoracale slokdarmresectie")
data_maag$resecok <- droplevels(data_maag$resecok)

# analig esophagus
table(data_slok$analig)
levels(data_slok$analig)
data_slok$analig <-  recode(data_slok$analig, 'c("Geen anastomose", "Intra-abdominaal", "Anders") = "Overig"')

# analig stoamch
table(data_maag$analig)
levels(data_maag$analig)
data_maag$analig <-  recode(data_maag$analig, 
                            'c("Geen anastomose", "Hals", "Intrathoracaal", "Anders") = "Overig"')

rm(data)

### DESCRIPTIVES OF ALL VARIABLES ###############################################################

# summary of esophagus data
# continuous:
summary(data_slok %>%
          select(age, length, weight, wtloss))
# sd:
data_slok %>%
  select(age, length, weight, wtloss) %>%
  summarise_all(list(~ sd(., na.rm = TRUE)))
# categorical:
summary(data_slok %>%
          select(-c(age, length, weight, wtloss)))
# categorical frequencies:
vars <- colnames(data_slok %>%
                   select(-c(age, length, weight, wtloss)))
for (var in vars){
  print(data_slok %>%
          select_(var)  %>%
          group_by_(var) %>%
          summarise(n = n()) %>%
          mutate(freq = n / sum(n)))
}
###### amount of no comorbidity
data_slok %>% filter(comorbcar == "Nee",
                     comorbdia == "Nee",
                     comorbneu == "Nee",
                     comorbmda == "Nee",
                     comorburo == "Nee",
                     comorbtro == "Nee",
                     comorbmus == "Nee",
                     comorbend == "Nee",
                     comorbinf == "Nee",
                     comorbove == "Nee",
                     comorbvas == "Nee")
100*(1194 / 4228) # esophagus
###### amount of no anamok
data_slok %>% filter(anamok1 == "Nee",
                     anamok2 == "Nee",
                     anamok3 == "Nee",
                     anamok4 == "Nee",
                     anamok5 == "Nee",
                     anamok6 == "Nee",
                     anamok7 == "Nee")
100*(3048 / 4228) # esophagus
### both complications
data_slok %>% filter(compnlek ==  "Ja",
                     comppulm == "Ja")
100*(386 / 4228)
# options(digits=8)
# options(pillar.sigfig=4)
# summary of stomach data
# continuous:
summary(data_maag %>%
          select(age, length, weight, wtloss))
# sd:
data_maag %>%
  select(age, length, weight, wtloss) %>%
  summarise_all(list(~ sd(., na.rm = TRUE))) 
# categorical:
summary(data_maag %>%
          select(-c(age, length, weight, wtloss)))
# categorical frequencies:
vars <- colnames(data_maag %>%
                   select(-c(age, length, weight, wtloss)))
for (var in vars){
  print(data_maag %>%
          select_(var)  %>%
          group_by_(var) %>%
          summarise(n = n()) %>%
          mutate(freq = n / sum(n)))
}
data_maag %>% filter(comorbcar == "Nee",
                     comorbdia == "Nee",
                     comorbneu == "Nee",
                     comorbmda == "Nee",
                     comorburo == "Nee",
                     comorbtro == "Nee",
                     comorbmus == "Nee",
                     comorbend == "Nee",
                     comorbinf == "Nee",
                     comorbove == "Nee",
                     comorbvas == "Nee")
100*(489 / 2199) # stomach
###### amount of no anamok
data_maag %>% filter(anamok1 == "Nee",
                     anamok2 == "Nee",
                     anamok3 == "Nee",
                     anamok4 == "Nee",
                     anamok5 == "Nee",
                     anamok6 == "Nee",
                     anamok7 == "Nee")
100*(1352 / 2199) # stomach

### both complications
data_maag %>% filter(compnlek ==  "Ja",
                     comppulm == "Ja")
58 / 2199
### Histograms ############################################################################
hist <- geom_histogram(aes(y=..density..), binwidth=1, color="black", 
                       fill="white", position = "dodge")
dens <- stat_density(geom="line", col = "red")
# esophagus
g <- grid.arrange(
  ggplot(data_slok, aes(x = age)) + hist + dens,
  ggplot(data_slok, aes(x = length)) + hist + dens,
  ggplot(data_slok, aes(x = weight)) + hist + dens,
  ggplot(data_slok, aes(x = wtloss)) + hist + dens
  )
# ggsave("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/histograms.png", g)
# stomach
g_maag <- grid.arrange(
  ggplot(data_maag, aes(x = age)) + hist + dens,
  ggplot(data_maag, aes(x = length)) + hist + dens,
  ggplot(data_maag, aes(x = weight)) + hist + dens,
  ggplot(data_maag, aes(x = wtloss)) + hist + dens
)
# ggsave("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/histograms_stomach.png", g_maag)


# density per compnlek
grid.arrange(
ggplot(data_slok %>% filter(compnlek != "NA")) +
  geom_density(aes(x = age, group = compnlek, fill = compnlek),alpha=0.3, adjust=2),
ggplot(data_slok %>% filter(compnlek != "NA")) +
  geom_density(aes(x = length, group = compnlek, fill = compnlek),alpha=0.3, adjust=2),
ggplot(data_slok %>% filter(compnlek != "NA")) +
  geom_density(aes(x = weight, group = compnlek, fill = compnlek),alpha=0.3, adjust=2),
ggplot(data_slok %>% filter(compnlek != "NA")) +
  geom_density(aes(x = wtloss, group = compnlek, fill = compnlek),alpha=0.3, adjust=2)
)

# density per comppulm
grid.arrange(
  ggplot(data_slok %>% filter(comppulm != "NA")) +
    geom_density(aes(x = age, group = comppulm, fill = comppulm),alpha=0.3, adjust=2),
  ggplot(data_slok %>% filter(comppulm != "NA")) +
    geom_density(aes(x = length, group = comppulm, fill = comppulm),alpha=0.3, adjust=2),
  ggplot(data_slok %>% filter(comppulm != "NA")) +
    geom_density(aes(x = weight, group = comppulm, fill = comppulm),alpha=0.3, adjust=2),
  ggplot(data_slok %>% filter(comppulm != "NA")) +
    geom_density(aes(x = wtloss, group = comppulm, fill = comppulm),alpha=0.3, adjust=2)
)

### Trends ############################################################################
library(Hmisc)
data_slok$compnlek_num <- ifelse(data_slok$compnlek == "Ja",1 , 0)
data_slok$comppulm_num <- ifelse(data_slok$comppulm == "Ja",1 , 0)
s <- summary(compnlek_num ~ age + sex + length + weight + wtloss +
          comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
          comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
          anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
          xtumhist + xtumlok1 + scorecn + scorecm +
          anammed2 + xscorect + asascore +
          xneoadjtype + resecok + procok + recontype + analig, data = data_slok)
plot(s, subtitles = FALSE)

# age
ggplot(data_slok, aes(x = age, y = compnlek_num)) +
  histSpikeg(compnlek_num ~ age , lowess = TRUE , data = data_slok) +
  ylim(0,1) + ylab (NULL)
ggplot(data_slok, aes(x = age + sex, y = compnlek_num, color = sex)) +
  histSpikeg(compnlek_num ~ age + sex, lowess = TRUE , data = data_slok) +
  ylim(0,1) + ylab (NULL)
# weight
ggplot(data_slok, aes(x = weight, y = compnlek_num)) +
  histSpikeg(compnlek_num ~ weight , lowess = TRUE , data = data_slok) +
  ylim(0,1) + ylab (NULL)
ggplot(data_slok, aes(x = weight + sex, y = compnlek_num, color = sex)) +
  histSpikeg(compnlek_num ~ weight + sex, lowess = TRUE , data = data_slok) +
  ylim(0,1) + ylab (NULL)
# length
ggplot(data_slok, aes(x = length, y = compnlek_num)) +
  histSpikeg(compnlek_num ~ length , lowess = TRUE , data = data_slok) +
  ylim(0,1) + ylab (NULL)
ggplot(data_slok, aes(x = length + sex, y = compnlek_num, color = sex)) +
  histSpikeg(compnlek_num ~ length + sex, lowess = TRUE , data = data_slok) +
  ylim(0,1) + ylab (NULL)
# wtloss
ggplot(data_slok, aes(x = wtloss, y = compnlek_num)) +
  histSpikeg(compnlek_num ~ wtloss , lowess = TRUE , data = data_slok) +
  ylim(0,1) + ylab (NULL)
ggplot(data_slok, aes(x = wtloss + sex, y = compnlek_num, color = sex)) +
  histSpikeg(compnlek_num ~ wtloss + sex, lowess = TRUE , data = data_slok) +
  ylim(0,1) + ylab (NULL)

### CORRELATION MATRIX ######################################################################

###############################################################################
#
# cor2
# -- Compute correlations of columns of a dataframe of mixed types
#
###############################################################################
#
# author  : Srikanth KS (talegari)
# license : GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
#
###############################################################################
#' @title cor2
#' 
#' @description Compute correlations of columns of a dataframe of mixed types. 
#'   The dataframe is allowed to have columns of these four classes: integer, 
#'   numeric, factor and character. The character column is considered as 
#'   categorical variable.
#'  
#' @details The correlation is computed as follows: \itemize{
#'   
#'   \item integer/numeric pair: pearson correlation using `cor` function. The 
#'   valuelies between -1 and 1.
#'   
#'   \item integer/numeric - factor/categorical pair: correlation coefficient or
#'   squared root of R^2 coefficient of linear regression of integer/numeric
#'   variable over factor/categorical variable using `lm` function. The value
#'   lies between 0 and 1. \item factor/categorical pair: cramersV value is
#'   computed based on chisq test using `lsr::cramersV` function. The value lies
#'   between 0 and 1.
#'   }
#' 
#'   For a comprehensive implementation, use `polycor::hetcor`
#'     
#' @param df input data frame
#' 
#' @author Srikanth KS(gmail to sri dot teach)
#' 
#' @keywords GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
#' 
#' @examples
#' iris_cor <- cor2(iris)
#' corrplot::corrplot(iris_cor)
#' corrgram::corrgram(iris_cor)
#'   
cor2 = function(df){
  
  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))
  
  cor_fun <- function(pos_1, pos_2){
    
    # both are numeric
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("integer", "numeric")){

      r <- tryCatch({r = stats::cor(df[[pos_1]]
                                    , df[[pos_2]]
                                    , use = "pairwise.complete.obs"
      )},
                    error = function(cond) {r = NA},
                    finally = {})
    }
    
    # one is numeric and other is a factor/character
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("factor", "character")){

      r <- tryCatch({r = sqrt(
        summary(
          stats::lm(df[[pos_1]] ~ as.factor(df[[pos_2]])))[["r.squared"]])},
                    error = function(cond) {r = NA},
                    finally = {})
    }
    
    if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
       class(df[[pos_1]]) %in% c("factor", "character")){
      
      r <- tryCatch({r = sqrt(
        summary(
          stats::lm(df[[pos_2]] ~ as.factor(df[[pos_1]])))[["r.squared"]])},
                    error = function(cond) {r = NA},
                    finally = {})
    }
    
    # both are factor/character
    if(class(df[[pos_1]]) %in% c("factor", "character") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      
      r <- tryCatch({r = lsr::cramersV(df[[pos_1]], df[[pos_2]], simulate.p.value = TRUE)},
                    error = function(cond) {r = NA},
                    finally = {})
      
    }
    
    return(r)
  } 
  
  cor_fun <- Vectorize(cor_fun)
  
  # now compute corr matrix
  corrmat <- outer(1:ncol(df)
                   , 1:ncol(df)
                   , function(x, y) cor_fun(x, y)
  )
  
  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)
  
  return(corrmat)
}

# esophagus
cors_eso <- cor2(data_slok)
ggcorrplot(cors_eso, hc.order = FALSE, type = "lower", 
           title = "Association matrix esophagus data")
which(cors_eso > .5 & cors_eso != 1, arr.ind = TRUE)
which(cors_eso < -.5 & cors_eso != 1, arr.ind = TRUE)
high_cors_eso <- cor2(data_slok %>% select(sex, length,weight,
                                           comorbdia, comorbneu,comorburo, anamok3, anamok4, anamok5, 
                                           xtumhist, xtumlok1,
                          recontype, resecok, procok))
high_cors_eso
# ggsave("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/Association_matrix_esophagus_data.png")

# stomach
cors_maag <- cor2(data_maag)
ggcorrplot(cors_maag, hc.order = FALSE, type = "lower", 
           title = "Association matrix stomach data")
which(cors_maag > .5 & cors_maag != 1, arr.ind = TRUE)
which(cors_maag < -.5 & cors_maag != 1, arr.ind = TRUE)
high_cors_maag <- cor2(data_maag %>% select(sex, length, weight,
                                            comorbdia, comorbneu, anamok1, anamok3, anamok4, anamok5, 
                                           xtumlok2, xtumhist,
                                           recontype, resecok))
high_cors_maag
cor_pmat(high_cors_maag)
# ggsave("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/Association_matrix_stomach_data.png")


# C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots

### MISSING VALUES ########################################################################
# missingness map esophagus
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/missingness_esophagus_data.png")
data_slok %>% slice(sample(1:n())) %>% missmap(col = c("red", "lightblue"), y.labels = NULL, y.at = NULL)
dev.off()
# missingness map stomach
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/missingness_stomach_data.png")
data_maag %>% slice(sample(1:n())) %>% missmap(col = c("red", "lightblue"), y.labels = NULL, y.at = NULL)
dev.off()
# missing per var and row
pMiss <- function(x){sum(is.na(x))/length(x)*100}
sort(apply(data_slok, 2, pMiss))
tail(sort(apply(data_slok, 1, pMiss)), 40)
gg_miss_var(data_slok, show_pct = TRUE)
# ggsave("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/missing_num_esophagus_data.png")
# missing per var and row
pMiss <- function(x){sum(is.na(x))/length(x)*100}
sort(apply(data_maag, 2, pMiss))
tail(sort(apply(data_maag, 1, pMiss)), 40)
gg_miss_var(data_maag, show_pct = TRUE)
# ggsave("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/missing_num_stomach_data.png")

### IMPUTATION #############################################################################

## pmm esophagus
imp <- mice(data_slok, method = "pmm", m = 1)
data_slok <- complete(imp)
## pmm stomach
imp <- mice(data_maag, method = "pmm", m = 1)
data_maag <- complete(imp)
rm(imp)
# train/test split esophagus
set.seed(123)
slok_idx <- createDataPartition(data_slok$compnlek, p = .75, list = FALSE)
slok_trn <- data_slok[slok_idx,]
slok_tst <- data_slok[-slok_idx,]
rm(slok_idx)
# train/test split stomach
set.seed(123)
data_maag_idx <- createDataPartition(data_maag$compnlek, p = .75, list = FALSE)
maag_trn <- data_maag[data_maag_idx,]
maag_tst <- data_maag[-data_maag_idx,]
rm(data_maag_idx)


### LOGISTIC MODELS #############################################################################
### esophagus ##########################
### cross validated logistic model accuracy compnlek
set.seed(123)
slok_glm_mod_compnlek <- train(compnlek ~ age + sex + length + weight + wtloss +
                         comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                         comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                         anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                         xtumhist + xtumlok1 + scorecn + scorecm +
                         anammed2 + xscorect + asascore +
                         xneoadjtype + resecok + procok + recontype + analig,
                       data = slok_trn,
                       trControl = trainControl(method = "cv", 
                                                number = 10, 
                                                summaryFunction = twoClassSummary,
                                                classProbs = TRUE),
                       method = "glm",
                       family = "binomial",
                       metric = "ROC"
                       )
# model

slok_glm_mod_compnlek$results
sumary(slok_glm_mod_compnlek$finalModel)
# test results
preds <- predict(slok_glm_mod_compnlek, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_glm_mod_compnlek, newdata = slok_tst, type = "prob")
probs_slok_glm_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    print.auc = TRUE)

# title("AUC esophagus compnlek logistic")

### cross validated logistic model accuracy comppulm
set.seed(123)
slok_glm_mod_comppulm <- train(comppulm ~ age + sex + length + weight + wtloss +
                         comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                         comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                         anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                         xtumhist + xtumlok1 + scorecn + scorecm +
                         anammed2 + xscorect + asascore +
                         xneoadjtype + resecok + procok + recontype + analig,
                       data = slok_trn,
                       trControl = trainControl(method = "cv", 
                                                number = 10, 
                                                summaryFunction = twoClassSummary,
                                                classProbs = TRUE),
                       method = "glm",
                       family = "binomial",
                       metric = "ROC"
)
# model
slok_glm_mod_comppulm
slok_glm_mod_comppulm$results
sumary(slok_glm_mod_comppulm$finalModel)
# test results
preds <- predict(slok_glm_mod_comppulm, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_glm_mod_comppulm, newdata = slok_tst, type = "prob")
probs_slok_glm_mod_comppulm <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$comppulm, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$comppulm, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    print.auc = TRUE)
# title("AUC esophagus comppulm logistic")

#######################################

### stomach ##########################
# 161 / (1906 + 161) # % compnlek (8%)
# 345 / (1878 + 345) # % comppulm (15%)
# cross validated logistic model accuracy compnlek
set.seed(123)
maag_glm_mod_compnlek <- train(compnlek ~ age + sex + length + weight +
                         comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                         comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                         anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                         xtumhist + xtumlok2 + scorecn1 + scorecm +
                         anammed2 + xscorect1 + asascore +
                         xneoadjtype + resecok + procok + recontype + analig,
                       data = maag_trn,
                       trControl = trainControl(method = "cv", 
                                                number = 10, 
                                                summaryFunction = twoClassSummary,
                                                classProbs = TRUE),
                       method = "glm",
                       family = "binomial",
                       metric = "ROC"
)
# model
maag_glm_mod_compnlek
maag_glm_mod_compnlek$results
sumary(maag_glm_mod_compnlek$finalModel)
# test results
preds <- predict(maag_glm_mod_compnlek, newdata = maag_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(maag_glm_mod_compnlek, newdata = maag_tst, type = "prob")
probs_maag_glm_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = maag_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(maag_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    print.auc = TRUE)
# title("AUC stomach comppulm logistic")



### LASSO MODELS #############################################################################

# MODEL MATRIX TRAIN
trainX <- slok_trn %>% select(age,sex,length,weight,wtloss,
                              comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                              comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                              anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                              xtumhist,xtumlok1,scorecn,scorecm,
                              anammed2,xscorect,asascore,
                              xneoadjtype,resecok,procok,recontype,analig)
trainY <- slok_trn$compnlek
trainX <- model.matrix(trainY ~ ., data = trainX)
# MODEL MATRIX TEST
testX <- slok_tst %>% select(age,sex,length,weight,wtloss,
                              comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                              comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                              anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                              xtumhist,xtumlok1,scorecn,scorecm,
                              anammed2,xscorect,asascore,
                              xneoadjtype,resecok,procok,recontype,analig)
testY <- slok_tst$compnlek
testX <- model.matrix(testY ~ ., data = testX)
#

set.seed(123)
cv.lasso <- cv.glmnet(trainX, trainY, family = "binomial", alpha = 1, lambda = NULL, standardize = TRUE)
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, cv.lasso$lambda.min) # same as train method if lambda = equal.
coef(cv.lasso, cv.lasso$lambda.1se) # same as train method if lambda = equal.
#
lasso.model <- glmnet(trainX, trainY, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)
lasso.model.1se <- glmnet(trainX, trainY, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.1se)
#
probabilities <- lasso.model.1se %>% predict(newx = testX)
predicted.classes <- ifelse(probabilities > 0.5, "Ja", "Nee")
# Model accuracy
observed.classes <- testY
mean(predicted.classes == observed.classes)

### esophagus ##########################
# cross validated lasso model accuracy compnlek SLOK
set.seed(123)
grd <- expand.grid(alpha = 1, lambda = seq(0.0001, 0.0900, length = 30))
slok_lasso_mod_compnlek <- train(compnlek ~ age + sex + length + weight + wtloss +
                                   comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                   comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                   anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                   xtumhist + xtumlok1 + scorecn + scorecm +
                                   anammed2 + xscorect + asascore +
                                   xneoadjtype + resecok + procok + recontype + analig,
                                 data = slok_trn,
                                 method = "glmnet",
                                 trControl = trainControl(method="cv", 
                                                          number = 10, 
                                                          summaryFunction = twoClassSummary,
                                                          classProbs = TRUE),
                                 tuneGrid = grd,
                                 metric = "ROC"
                                 )
# model
slok_lasso_mod_compnlek
coef(slok_lasso_mod_compnlek$finalModel, slok_lasso_mod_compnlek$bestTune$lambda)
# cv accurcay plot
plot(slok_lasso_mod_compnlek)
# test results
preds <- predict(slok_lasso_mod_compnlek, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_lasso_mod_compnlek, newdata = slok_tst, type = "prob")
probs_slok_lasso_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    print.auc = TRUE)

# cross validated lasso model accuracy comppulm SLOK
set.seed(123)
grd <- expand.grid(alpha = 1, lambda = seq(0.0001, 0.0900, length = 30))
slok_lasso_mod_comppulm <- train(comppulm ~ age + sex + length + weight + wtloss +
                                   comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                   comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                   anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                   xtumhist + xtumlok1 + scorecn + scorecm +
                                   anammed2 + xscorect + asascore +
                                   xneoadjtype + resecok + procok + recontype + analig,
                                 data = slok_trn,
                                 method = "glmnet",
                                 trControl = trainControl(method="cv", 
                                                          number = 10, 
                                                          summaryFunction = twoClassSummary,
                                                          classProbs = TRUE),
                                 tuneGrid = grd,
                                 metric = "ROC"
)
# model
slok_lasso_mod_comppulm
slok_lasso_mod_comppulm$results
# cv accurcay plot
plot(slok_lasso_mod_comppulm)
# test results
preds <- predict(slok_lasso_mod_comppulm, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_lasso_mod_comppulm, newdata = slok_tst, type = "prob")
probs_slok_lasso_mod_comppulm <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$comppulm, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$comppulm, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    print.auc = TRUE)


### stomach ##########################
# cross validated lasso model accuracy compnlek MAAG
set.seed(123)
grd <- expand.grid(alpha = 1, lambda = seq(0.0001, 0.0900, length = 30))
maag_lasso_mod_compnlek <- train(compnlek ~ age + sex + length + weight +
                                   comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                   comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                   anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                   xtumhist + xtumlok2 + scorecn1 + scorecm +
                                   anammed2 + xscorect1 + asascore +
                                   xneoadjtype + resecok + procok + recontype + analig,
                                 data = maag_trn,
                                 method = "glmnet",
                                 trControl = trainControl(method="cv", 
                                                          number = 10, 
                                                          summaryFunction = twoClassSummary,
                                                          classProbs = TRUE),
                                 tuneGrid = grd,
                                 metric = "ROC"
)
# model
maag_lasso_mod_compnlek
maag_lasso_mod_compnlek$results
# cv accurcay plot
plot(maag_lasso_mod_compnlek)
# test results
preds <- predict(maag_lasso_mod_compnlek, newdata = maag_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(maag_lasso_mod_compnlek, newdata = maag_tst, type = "prob")
probs_maag_lasso_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = maag_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(maag_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    print.auc = TRUE)


### knn MODELS #############################################################################

### esophagus ##########################
# cross validated knn model accuracy compnlek SLOK
set.seed(123)
slok_knn_mod_compnlek <- train(compnlek ~ age + sex + length + weight + wtloss +
                                 comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                 comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                 anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                 xtumhist + xtumlok1 + scorecn + scorecm +
                                 anammed2 + xscorect + asascore +
                                 xneoadjtype + resecok + procok + recontype + analig,
                               data = slok_trn,
                               trControl = trainControl(method="cv", 
                                                        number = 10, 
                                                        summaryFunction = twoClassSummary,
                                                        classProbs = TRUE),
                                method = "knn",
                                preProcess = c("center","scale"),
                               tuneGrid = expand.grid(k = seq(1, 100, by = 2)),
                               metric = "Roc"
                               )
# model

slok_knn_mod_compnlek$results
# cv accurcay plot
plot(slok_knn_mod_compnlek)
# test results
preds <- predict(slok_knn_mod_compnlek, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_knn_mod_compnlek, newdata = slok_tst, type = "prob")
probs_slok_knn_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    print.auc = TRUE)

# plot.roc(slok_tst$compnlek, probs[,2], add = TRUE, print.auc = TRUE, print.auc.y = 40, 
#          percent = TRUE, col = "#4daf4a")
# legend("bottomright", 
#        legend = c("logistic regression", "kNN"), 
#        col = c("#377eb8", "#4daf4a"), lwd = 4)

# cross validated knn model accuracy comppulm SLOK
set.seed(123)
slok_knn_mod_comppulm <- train(comppulm ~ age + sex + length + weight + wtloss +
                                  comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                  comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                  anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                  xtumhist + xtumlok1 + scorecn + scorecm +
                                  anammed2 + xscorect + asascore +
                                  xneoadjtype + resecok + procok + recontype + analig,
                                data = slok_trn,
                               trControl = trainControl(method="cv", 
                                                        number = 10, 
                                                        summaryFunction = twoClassSummary,
                                                        classProbs = TRUE),
                                method = "knn",
                                preProcess = c("center","scale"),
                               tuneGrid = expand.grid(k = seq(1, 100, by = 2)),
                               metric = "ROC"
)
# model
slok_knn_mod_comppulm
slok_knn_mod_comppulm$results
# cv accurcay plot
plot(slok_knn_mod_comppulm)
# test results
preds <- predict(slok_knn_mod_comppulm, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_knn_mod_comppulm, newdata = slok_tst, type = "prob")
probs_slok_knn_mod_comppulm <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$comppulm, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$comppulm, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    print.auc = TRUE)


### stomach ##########################
# cross validated knn model accuracy compnlek MAAG
set.seed(123)
maag_knn_mod_compnlek <- train(compnlek ~ age + sex + length + weight +
                                 comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                 comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                 anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                 xtumhist + xtumlok2 + scorecn1 + scorecm +
                                 anammed2 + xscorect1 + asascore +
                                 xneoadjtype + resecok + procok + recontype + analig,
                                data = maag_trn,
                               trControl = trainControl(method="cv", 
                                                        number = 10, 
                                                        summaryFunction = twoClassSummary,
                                                        classProbs = TRUE),
                                method = "knn",
                                preProcess = c("center","scale"),
                               tuneGrid = expand.grid(k = seq(1, 100, by = 2)),
                               metric = "ROC"
)
# model
maag_knn_mod_compnlek
maag_knn_mod_compnlek$results
# cv accurcay plot
plot(maag_knn_mod_compnlek)
# test results
preds <- predict(maag_knn_mod_compnlek, newdata = maag_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(maag_knn_mod_compnlek, newdata = maag_tst, type = "prob")
probs_maag_knn_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = maag_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(maag_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    print.auc = TRUE)

### Neural Network MODELS #############################################################################
# pp <- preProcess(slok_trn_num, method = list(scale = c("age", "length", "weight"),
#                                          center = c("age", "length", "weight"),
#                                          YeoJohnson = c("wtloss")))
# predict(pp, slok_tst_num)
### esophagus ##########################
# cross validated NN model accuracy compnlek SLOK
##
##
set.seed(123)
slok_NN_mod_compnlek <- train(compnlek ~ age + sex + length + weight + wtloss +
                                 comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                 comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                 anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                 xtumhist + xtumlok1 + scorecn + scorecm +
                                 anammed2 + xscorect + asascore +
                                 xneoadjtype + resecok + procok + recontype + analig,
                            
                              data = slok_trn,
                              trControl = trainControl(method='cv',
                                                       number=10, 
                                                       summaryFunction = twoClassSummary, 
                                                       classProbs = TRUE,
                                                       search = "random"),
                              method = "nnet",
                              tuneLength = 50,
                              preProcess = c("center", "scale"),
                              metric = "ROC",
                              maxit = 500
)
# model
slok_NN_mod_compnlek
slok_NN_mod_compnlek$results
# cv accurcay plot
plot(slok_NN_mod_compnlek)
# test results
preds <- predict(slok_NN_mod_compnlek, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_NN_mod_compnlek, newdata = slok_tst, type = "prob")
probs_slok_NN_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#3db837",
    print.auc = TRUE)

# cross validated NN model accuracy comppulm SLOK
##
##
set.seed(123)
slok_NN_mod_comppulm <- train(comppulm ~ age + sex + length + weight + wtloss +
                                comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                xtumhist + xtumlok1 + scorecn + scorecm +
                                anammed2 + xscorect + asascore +
                                xneoadjtype + resecok + procok + recontype + analig,
                              data = slok_trn,
                              trControl = trainControl(method='cv',
                                                       number=10, 
                                                       summaryFunction = twoClassSummary, 
                                                       classProbs = TRUE,
                                                       search = "random"),
                              method = "nnet",
                              tuneLength = 50,
                              preProcess = c("center", "scale"),
                              metric = "ROC",
                              maxit = 500
)
# model
slok_NN_mod_comppulm
slok_NN_mod_comppulm$results
# cv accurcay plot
plot(slok_NN_mod_comppulm)
# test results
preds <- predict(slok_NN_mod_comppulm, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_NN_mod_comppulm, newdata = slok_tst, type = "prob")
probs_slok_NN_mod_comppulm <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$comppulm, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$comppulm, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#3db837",
    print.auc = TRUE)

# cross validated NN model accuracy comppulm SLOK
##
##
set.seed(123)
maag_NN_mod_compnlek <- train(compnlek ~ age + sex + length + weight +
                                comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                xtumhist + xtumlok2 + scorecn1 + scorecm +
                                anammed2 + xscorect1 + asascore +
                                xneoadjtype + resecok + procok + recontype + analig,
                              
                              data = maag_trn,
                              trControl = trainControl(method='cv',
                                                       number=10, 
                                                       summaryFunction = twoClassSummary, 
                                                       classProbs = TRUE,
                                                       search = "random"),
                              method = "nnet",
                              tuneLength = 50,
                              preProcess = c("center", "scale"),
                              metric = 'ROC',
                              maxit = 500
)
# model
maag_NN_mod_compnlek$
maag_NN_mod_compnlek$results
# cv accurcay plot
plot(maag_NN_mod_compnlek)
# test results
preds <- predict(maag_NN_mod_compnlek, newdata = maag_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(maag_NN_mod_compnlek, newdata = maag_tst, type = "prob")
probs_maag_NN_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = maag_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(maag_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#3db837",
    print.auc = TRUE)

### sv MODELS #############################################################################

### esophagus ##########################
# cross validated sv model accuracy compnlek SLOK POLY
set.seed(123)
slok_svm_mod_compnlek_poly <- train(compnlek ~ age + sex + length + weight + wtloss +
                                 comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                 comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                 anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                 xtumhist + xtumlok1 + scorecn + scorecm +
                                 anammed2 + xscorect + asascore +
                                 xneoadjtype + resecok + procok + recontype + analig,
                               data = slok_trn,
                               trControl = trainControl(method='cv',
                                                        number=5, 
                                                        summaryFunction = twoClassSummary, 
                                                        classProbs = TRUE,
                                                        search = "random"),
                               preProcess = c("center", "scale"),
                               method = "svmPoly",
                               metric = "ROC",
                               tuneLength = 30
                               # tuneGrid = grid_poly
)
# model
slok_svm_mod_compnlek_poly
slok_svm_mod_compnlek_poly$results
# cv accurcay plot
plot(slok_svm_mod_compnlek_poly)
# test results
preds <- predict(slok_svm_mod_compnlek_poly, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_svm_mod_compnlek_poly, newdata = slok_tst, type = "prob")
probs_slok_svm_mod_compnlek_poly <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#3db837",
    print.auc = TRUE)

# cross validated sv model accuracy compnlek SLOK RADIAL
set.seed(123)
slok_svm_mod_compnlek_radial <- train(compnlek ~ age + sex + length + weight + wtloss +
                                      comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                      comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                      anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                      xtumhist + xtumlok1 + scorecn + scorecm +
                                      anammed2 + xscorect + asascore +
                                      xneoadjtype + resecok + procok + recontype + analig,
                                    data = slok_trn,
                                    trControl = trainControl(method='cv',
                                                             number=5, 
                                                             summaryFunction = twoClassSummary, 
                                                             classProbs = TRUE,
                                                             search = "random"),
                                    preProcess = c("center", "scale"),
                                    method = "svmRadial",
                                    metric = "ROC",
                                    tuneLength = 30
)
# model
slok_svm_mod_compnlek_radial
slok_svm_mod_compnlek_radial$results
# cv accurcay plot
plot(slok_svm_mod_compnlek_radial)
# test results
preds <- predict(slok_svm_mod_compnlek_radial, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_svm_mod_compnlek_radial, newdata = slok_tst, type = "prob")
probs_slok_svm_mod_compnlek_radial <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#3db837",
    print.auc = TRUE)


# cross validated sv model accuracy compulm SLOK POLY
set.seed(123)
slok_svm_mod_compulm_poly <- train(comppulm ~ age + sex + length + weight + wtloss +
                                 comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                 comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                 anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                 xtumhist + xtumlok1 + scorecn + scorecm +
                                 anammed2 + xscorect + asascore +
                                 xneoadjtype + resecok + procok + recontype + analig,
                               data = slok_trn,
                               trControl = trainControl(method='cv',
                                                        number=5, 
                                                        summaryFunction = twoClassSummary, 
                                                        classProbs = TRUE,
                                                        search = "random"),
                              preProcess = c("center", "scale"),
                              method = "svmPoly",
                              metric = "ROC",
                              tuneLength = 30
)
# model
slok_svm_mod_compulm_poly
slok_svm_mod_compulm_poly$results
# cv accurcay plot
plot(slok_svm_mod_compulm_poly)
# test results
preds <- predict(slok_svm_mod_compulm_poly, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_svm_mod_compulm_poly, newdata = slok_tst, type = "prob")
probs_slok_svm_mod_compulm_poly <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$comppulm, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$comppulm, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#3db837",
    print.auc = TRUE)

# cross validated sv model accuracy compulm SLOK RADIAL
set.seed(123)
slok_svm_mod_compulm_radial <- train(comppulm ~ age + sex + length + weight + wtloss +
                                     comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                     comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                     anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                     xtumhist + xtumlok1 + scorecn + scorecm +
                                     anammed2 + xscorect + asascore +
                                     xneoadjtype + resecok + procok + recontype + analig,
                                   data = slok_trn,
                                   trControl = trainControl(method='cv',
                                                            number=5, 
                                                            summaryFunction = twoClassSummary, 
                                                            classProbs = TRUE,
                                                            search = "random"),
                                   preProcess = c("center", "scale"),
                                   method = "svmRadial",
                                   metric = "ROC",
                                   tuneLength = 30
)
# model
slok_svm_mod_compulm_radial
slok_svm_mod_compulm_radial$results
# cv accurcay plot
plot(slok_svm_mod_compulm_radial)
# test results
preds <- predict(slok_svm_mod_compulm_radial, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_svm_mod_compulm_radial, newdata = slok_tst, type = "prob")
probs_slok_svm_mod_compulm_radial <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$comppulm, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$comppulm, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#3db837",
    print.auc = TRUE)

### stomach ##########################
# cross validated svm model accuracy compnlek MAAG POLY
set.seed(123)
maag_svm_mod_compnlek_poly <- train(compnlek ~ age + sex + length + weight +
                                 comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                 comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                 anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                 xtumhist + xtumlok2 + scorecn1 + scorecm +
                                 anammed2 + xscorect1 + asascore +
                                 xneoadjtype + resecok + procok + recontype + analig,
                               data = maag_trn,
                               trControl = trainControl(method='cv',
                                                        number=5, 
                                                        summaryFunction = twoClassSummary, 
                                                        classProbs = TRUE,
                                                        search = "random"),
                               preProcess = c("center", "scale"),
                               method = "svmPoly",
                               metric = "ROC",
                               tuneLength = 30
)
# model
maag_svm_mod_compnlek_poly
maag_svm_mod_compnlek_poly$results
# cv accurcay plot
plot(maag_svm_mod_compnlek_poly)
# test results
preds <- predict(maag_svm_mod_compnlek_poly, newdata = maag_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(maag_svm_mod_compnlek_poly, newdata = maag_tst, type = "prob")
probs_maag_svm_mod_compnlek_poly <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = maag_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(maag_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#3db837",
    print.auc = TRUE)

# cross validated svm model accuracy compnlek MAAG RADIAL
set.seed(123)
maag_svm_mod_compnlek_radial <- train(compnlek ~ age + sex + length + weight +
                                      comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                      comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                      anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                      xtumhist + xtumlok2 + scorecn1 + scorecm +
                                      anammed2 + xscorect1 + asascore +
                                      xneoadjtype + resecok + procok + recontype + analig,
                                    data = maag_trn,
                                    trControl = trainControl(method='cv',
                                                             number=5, 
                                                             summaryFunction = twoClassSummary, 
                                                             classProbs = TRUE,
                                                             search = "random"),
                                    preProcess = c("center", "scale"),
                                    method = "svmRadial",
                                    metric = "ROC",
                                    tuneLength = 30
)
# model
maag_svm_mod_compnlek_radial
maag_svm_mod_compnlek_radial$results
# cv accurcay plot
plot(maag_svm_mod_compnlek_radial)
# test results
preds <- predict(maag_svm_mod_compnlek_radial, newdata = maag_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(maag_svm_mod_compnlek_radial, newdata = maag_tst, type = "prob")
probs_maag_svm_mod_compnlek_radial <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = maag_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(maag_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#3db837",
    print.auc = TRUE)

### rf MODELS #############################################################################

### esophagus ##########################
# cross validated rf model accuracy compnlek SLOK
set.seed(123)
slok_rf_mod_compnlek <- train(compnlek ~ age + sex + length + weight + wtloss +
                                  comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                  comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                  anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                  xtumhist + xtumlok1 + scorecn + scorecm +
                                  anammed2 + xscorect + asascore +
                                  xneoadjtype + resecok + procok + recontype + analig,
                                data = slok_trn,
                              trControl = trainControl(method='cv',
                                                       number=5, 
                                                       summaryFunction = twoClassSummary, 
                                                       classProbs = TRUE,
                                                       search = "random"),
                                method = "rf",
                               tuneLength  = 50,
                              metric = "ROC"
)
# model
slok_rf_mod_compnlek
slok_rf_mod_compnlek$results
# cv accurcay plot
plot(slok_rf_mod_compnlek)
# test results
preds <- predict(slok_rf_mod_compnlek, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_rf_mod_compnlek, newdata = slok_tst, type = "prob")
probs_slok_rf_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#afb837",
    print.auc = TRUE)

# cross validated rf model accuracy compulm SLOK
set.seed(123)
slok_rf_mod_comppulm <- train(comppulm ~ age + sex + length + weight + wtloss +
                                 comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                 comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                 anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                 xtumhist + xtumlok1 + scorecn + scorecm +
                                 anammed2 + xscorect + asascore +
                                 xneoadjtype + resecok + procok + recontype + analig,
                               data = slok_trn,
                              trControl = trainControl(method='cv',
                                                       number=5, 
                                                       summaryFunction = twoClassSummary, 
                                                       classProbs = TRUE,
                                                       search = "random"),
                               method = "rf",
                               tuneLength  = 50,
                              metric = "ROC"
)
# model
slok_rf_mod_comppulm
slok_rf_mod_comppulm$results
# cv accurcay plot
plot(slok_rf_mod_comppulm)
# test results
preds <- predict(slok_rf_mod_comppulm, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_rf_mod_comppulm, newdata = slok_tst, type = "prob")
probs_slok_rf_mod_comppulm <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$comppulm, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$comppulm, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#afb837",
    print.auc = TRUE)

### stomach ##########################
# cross validated rf model accuracy compnlek MAAG
set.seed(123)
maag_rf_mod_compnlek <- train(compnlek ~ age + sex + length + weight +
                                 comorbcar + comorbvas + comorbdia + comorbneu + comorbmda + comorburo +
                                 comorbtro + comorbmus + comorbend + comorbinf + comorbmal + comorbove +
                                 anamok1 + anamok2 + anamok3 + anamok4 + anamok5 + anamok6 + anamok7 +
                                 xtumhist + xtumlok2 + scorecn1 + scorecm +
                                 anammed2 + xscorect1 + asascore +
                                 xneoadjtype + resecok + procok + recontype + analig,
                               data = maag_trn,
                              trControl = trainControl(method='cv',
                                                       number=5, 
                                                       summaryFunction = twoClassSummary, 
                                                       classProbs = TRUE,
                                                       search = "random"),
                               method = "rf",
                               tuneLength  = 50,
                              metric = "ROC"
)
# model
maag_rf_mod_compnlek
maag_rf_mod_compnlek$results
# cv accurcay plot
plot(maag_rf_mod_compnlek)
# test results
preds <- predict(maag_rf_mod_compnlek, newdata = maag_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(maag_rf_mod_compnlek, newdata = maag_tst, type = "prob")
probs_maag_rf_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = maag_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(maag_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#afb837",
    print.auc = TRUE)

### 'AdaBoost.M1' MODELS #############################################################################


### esophagus ##########################
# cross validated adaboost model accuracy compnlek SLOK
# divide data into X and Y part 
trainX <- slok_trn %>% select(age,sex,length,weight,wtloss,
                              comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                              comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                              anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                              xtumhist,xtumlok1,scorecn,scorecm,
                              anammed2,xscorect,asascore,
                              xneoadjtype,resecok,procok,recontype,analig)
trainY <- slok_trn$compnlek
#
grid <- expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3),
                    coeflearn = c("Breiman", "Freund", "Zhu"))
set.seed(123)
slok_ada_mod_compnlek <- train(trainX, trainY, 
                               method = "AdaBoost.M1", 
                               trControl = trainControl(method = "cv", 
                                                        number = 5, 
                                                        summaryFunction = twoClassSummary,
                                                        search = "random",
                                                        classProbs = TRUE,
                                                        verboseIter = TRUE),
                               tuneLength = 15,
                               preProc = c("center", "scale"),
                               metric = "ROC"
)
# model
slok_ada_mod_compnlek
slok_ada_mod_compnlek$results
# cv accurcay plot
plot(slok_ada_mod_compnlek)
# test results
preds <- predict(slok_ada_mod_compnlek, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_ada_mod_compnlek, newdata = slok_tst, type = "prob")
probs_slok_ada_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#afb837",
    print.auc = TRUE)

### cross validated adaboost model accuracy compulm SLOK
# divide data into X and Y part 
trainX <- slok_trn %>% select(age,sex,length,weight,wtloss,
                              comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                              comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                              anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                              xtumhist,xtumlok1,scorecn,scorecm,
                              anammed2,xscorect,asascore,
                              xneoadjtype,resecok,procok,recontype,analig)
trainY <- slok_trn$comppulm
#
set.seed(123)
slok_ada_mod_comppulm <- train(trainX, trainY, 
                               method = "AdaBoost.M1", 
                               trControl = trainControl(method = "cv", 
                                                        number = 5, 
                                                        summaryFunction = twoClassSummary,
                                                        search = "random",
                                                        classProbs = TRUE,
                                                        verboseIter = TRUE),
                               tuneLength = 15,
                               preProc = c("center", "scale"),
                               metric = "ROC"
)
# model
slok_ada_mod_comppulm
slok_ada_mod_comppulm$results
# cv accurcay plot
plot(slok_ada_mod_comppulm)
# test results
preds <- predict(slok_ada_mod_comppulm, newdata = slok_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(slok_ada_mod_comppulm, newdata = slok_tst, type = "prob")
probs_slok_ada_mod_comppulm <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$comppulm, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$comppulm, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#afb837",
    print.auc = TRUE)

### stomach ##########################
# cross validated adaboost model accuracy compnlek MAAG
# divide data into X and Y part 
trainX <- maag_trn %>% select(age,sex,length,weight,
                                comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                                comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                                anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                                xtumhist,xtumlok2,scorecn1,scorecm,
                                anammed2,xscorect1,asascore,
                                xneoadjtype,resecok,procok,recontype,analig)
trainY <- maag_trn$compnlek
#
set.seed(123)
maag_ada_mod_compnlek <- train(trainX, trainY, 
                               method = "AdaBoost.M1", 
                               trControl = trainControl(method = "cv", 
                                                        number = 5, 
                                                        summaryFunction = twoClassSummary,
                                                        search = "random",
                                                        classProbs = TRUE,
                                                        verboseIter = TRUE),
                               tuneLength = 15,
                               preProc = c("center", "scale"),
                               metric = "ROC"
)
# model
maag_ada_mod_compnlek
maag_ada_mod_compnlek$results
# cv accurcay plot
plot(maag_ada_mod_compnlek)
# test results
preds <- predict(maag_ada_mod_compnlek, newdata = maag_tst)
preds_num <- ifelse(preds == "Ja", 1, 0)
probs <- predict(maag_ada_mod_compnlek, newdata = maag_tst, type = "prob")
probs_maag_ada_mod_compnlek <- probs["Ja"]
# threshold probs
probs_t <- function(t) ifelse(probs[,1] > t, "Nee", "Ja")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = maag_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(maag_tst$compnlek, probs[,2], plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#afb837",
    print.auc = TRUE)


### SUPER LEARNER #############################################################################

### DATA
# divide train data into X and Y part 
trainX <- slok_trn %>% select(age,sex,length,weight,wtloss,
                              comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                              comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                              anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                              xtumhist,xtumlok1,scorecn,scorecm,
                              anammed2,xscorect,asascore,
                              xneoadjtype,resecok,procok,recontype,analig)
# outcome
trainY <- as.numeric(slok_trn$compnlek) - 1
# model matrix version
trainX_mm <- data.frame(model.matrix(trainY ~ ., data = trainX))

# divide test data into X and Y part 
testX <- slok_tst %>% select(age,sex,length,weight,wtloss,
                              comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                              comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                              anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                              xtumhist,xtumlok1,scorecn,scorecm,
                              anammed2,xscorect,asascore,
                              xneoadjtype,resecok,procok,recontype,analig)
# outcome
testY <- as.numeric(slok_tst$compnlek) - 1
# model matrix version
testX_mm <- data.frame(model.matrix(testY ~ ., data = testX))

### CUSTOM MODELS SLOK_COMPNLEK FOR SNOW PARALLEL
SL.knn.better_compnlek <- create.Learner("SL.kernelKnn", params=list(k = slok_knn_mod_compnlek$bestTune$k))
SL.nn.better_compnlek <- create.Learner("SL.nnet", params=list(size = slok_NN_mod_compnlek$bestTune$size, 
                                                  decay = slok_NN_mod_compnlek$bestTune$decay))
SL.svmPoly.better_compnlek <- create.Learner("SL.ksvm", params=list(kernel = "polydot",
                                                  degree = slok_svm_mod_compnlek_poly$bestTune$degree,
                                                  scale = slok_svm_mod_compnlek_poly$bestTune$scale))
SL.svmRadial.better_compnlek <- create.Learner("SL.ksvm", params=list(C =  slok_svm_mod_compnlek_radial$bestTune$C,
                                                  kernel = "rbfdot",
                                                  sigma = slok_svm_mod_compnlek_radial$bestTune$sigma))
SL.rf.better_compnlek <- create.Learner("SL.randomForest", params=list(num.trees = 500, 
                                                          mtry = slok_rf_mod_compnlek$bestTune$mtry))
SL.xgboost.better_compnlek <- create.Learner("SL.xgboost", 
                                             params=list(max_depth = slok_ada_mod_compnlek$bestTune$maxdepth))
### SINGLE MODEL SLOK COMPNLEK
# select number of cores
cluster = parallel::makeCluster(3)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, list(SL.knn.better_compnlek$names,
                                      SL.nn.better_compnlek$names,
                                      SL.svmPoly.better_compnlek$names,
                                      SL.svmRadial.better_compnlek$names,
                                      SL.rf.better_compnlek$names))
parallel::clusterSetRNGStream(cluster, 1)
system.time({
  slok_sl_mod_compnlek = snowSuperLearner(Y = trainY, X = trainX_mm, family = binomial(),
                        cluster = cluster,
                        method = "method.AUC",
                        SL.library = c("SL.mean",
                                       "SL.glm",
                                       "SL.glmnet",
                                       SL.knn.better_compnlek$names,
                                       SL.nn.better_compnlek$names,
                                       SL.svmPoly.better_compnlek$names,
                                       SL.svmRadial.better_compnlek$names,
                                       SL.rf.better_compnlek$names,
                                       "SL.xgboost"))
})
parallel::stopCluster(cluster)
slok_sl_mod_compnlek$fitLibrary

pred = predict(slok_sl_mod_compnlek, testX_mm, onlySL = TRUE)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(testY, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, testY)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc
###
# test results
probs <- as.numeric(pred$pred)
probs_slok_SL_mod_compnlek <- probs
# threshold probs
probs_t <- function(t) ifelse(probs > t, "Ja", "Nee")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$compnlek, probs, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#afb837",
    print.auc = TRUE)

### CROSS-VALIDATED SLOK COMPNLEK
# select number of cores
cluster = parallel::makeCluster(3)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, list(SL.knn.better_compnlek$names,
                                      SL.nn.better_compnlek$names,
                                      SL.svmPoly.better_compnlek$names,
                                      SL.svmRadial.better_compnlek$names,
                                      SL.rf.better_compnlek$names))
parallel::clusterSetRNGStream(cluster, 1)
system.time({
  cv_slok_sl_mod_compnlek = CV.SuperLearner(X = trainX_mm, Y = trainY, 
                          family = "binomial", 
                          V = 10,
                          SL.library = list("SL.mean",
                                            "SL.glm", 
                                            "SL.glmnet",
                                            SL.knn.better_compnlek$names,
                                            SL.nn.better_compnlek$names,
                                            SL.svmPoly.better_compnlek$names,
                                            SL.svmRadial.better_compnlek$names,
                                            SL.rf.better_compnlek$names,
                                            "SL.xgboost"),
                          method = "method.AUC",
                          parallel = cluster,
                          verbose = TRUE)
})
parallel::stopCluster(cluster)
summary(cv_slok_sl_mod_compnlek)

# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/sl_cv_plot_eso_compnlek.png",
#     width = 2*480, height = 2*480)
plot(cv_slok_sl_mod_compnlek)
dev.off()

############################################################################

### DATA SLOK COMPPULM
# divide data into X and Y part 
trainX <- slok_trn %>% select(age,sex,length,weight,wtloss,
                              comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                              comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                              anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                              xtumhist,xtumlok1,scorecn,scorecm,
                              anammed2,xscorect,asascore,
                              xneoadjtype,resecok,procok,recontype,analig)
# outcome
trainY <- as.numeric(slok_trn$comppulm) - 1
# model matrix version
trainX_mm <- data.frame(model.matrix(trainY ~ ., data = trainX))

# divide data into X and Y part 
testX <- slok_tst %>% select(age,sex,length,weight,wtloss,
                             comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                             comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                             anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                             xtumhist,xtumlok1,scorecn,scorecm,
                             anammed2,xscorect,asascore,
                             xneoadjtype,resecok,procok,recontype,analig)
# outcome
testY <- as.numeric(slok_tst$comppulm) - 1
# model matrix version
testX_mm <- data.frame(model.matrix(testY ~ ., data = testX))

### CUSTOM MODELS SLOK_COMPPULM FOR SNOW PARALLEL
SL.knn.better_comppulm <- create.Learner("SL.kernelKnn", params=list(k = slok_knn_mod_comppulm$bestTune$k))
SL.nn.better_comppulm <- create.Learner("SL.nnet", params=list(size = slok_NN_mod_comppulm$bestTune$size, 
                                                               decay = slok_NN_mod_comppulm$bestTune$decay))
SL.svmPoly.better_comppulm <- create.Learner("SL.ksvm", params=list(kernel = "polydot",
                                                                    degree = slok_svm_mod_compulm_poly$bestTune$degree,
                                                                    scale = slok_svm_mod_compulm_poly$bestTune$scale))
SL.svmRadial.better_comppulm <- create.Learner("SL.ksvm", params=list(C =  slok_svm_mod_compulm_radial$bestTune$C,
                                                                      kernel = "rbfdot",
                                                                      sigma = slok_svm_mod_compulm_radial$bestTune$sigma))
SL.rf.better_comppulm <- create.Learner("SL.randomForest", params=list(num.trees = 500, 
                                                                       mtry = slok_rf_mod_comppulm$bestTune$mtry))
SL.xgboost.better_comppulm <- create.Learner("SL.xgboost", 
                                             params=list(max_depth = slok_ada_mod_comppulm$bestTune$maxdepth))

### SINGLE MODEL SLOK COMPPULM
# select number of cores
cluster = parallel::makeCluster(3)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, list(SL.knn.better_compnlek$names,
                                      SL.nn.better_compnlek$names,
                                      SL.svmPoly.better_compnlek$names,
                                      SL.svmRadial.better_compnlek$names,
                                      SL.rf.better_compnlek$names))
parallel::clusterSetRNGStream(cluster, 1)
system.time({
  slok_sl_mod_compulm = snowSuperLearner(Y = trainY, X = trainX_mm, family = binomial(),
                        cluster = cluster,
                        method = "method.AUC",
                        SL.library = c("SL.mean",
                                       "SL.glm",
                                       "SL.glmnet",
                                       SL.knn.better_compnlek$names,
                                       SL.nn.better_compnlek$names,
                                       SL.svmPoly.better_compnlek$names,
                                       SL.svmRadial.better_compnlek$names,
                                       SL.rf.better_compnlek$names,
                                       "SL.xgboost"))
})
parallel::stopCluster(cluster)

slok_sl_mod_compulm
slok_sl_mod_compulm2

pred = predict(slok_sl_mod_compulm, testX_mm, onlySL = TRUE)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(testY, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, testY)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc
###
# test results
probs <- as.numeric(pred$pred)
probs_slok_SL_mod_comppulm <- probs
# threshold probs
probs_t <- function(t) ifelse(probs > t, "Ja", "Nee")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = slok_tst$comppulm, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(slok_tst$comppulm, probs, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#afb837",
    print.auc = TRUE)

### CROSS-VALIDATED SLOK COMPPULM
# select number of cores
cluster = parallel::makeCluster(3)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, list(SL.knn.better_compnlek$names,
                                      SL.nn.better_compnlek$names,
                                      SL.svmPoly.better_compnlek$names,
                                      SL.svmRadial.better_compnlek$names,
                                      SL.rf.better_compnlek$names))
parallel::clusterSetRNGStream(cluster, 1)
system.time({
  cv_slok_sl_mod_compulm = CV.SuperLearner(X = trainX_mm, Y = trainY, 
                          family = "binomial", 
                          V = 10,
                          SL.library = list("SL.mean",
                                            "SL.glm", 
                                            "SL.glmnet",
                                            SL.knn.better_compnlek$names,
                                            SL.nn.better_compnlek$names,
                                            SL.svmPoly.better_compnlek$names,
                                            SL.svmRadial.better_compnlek$names,
                                            SL.rf.better_compnlek$names,
                                            "SL.xgboost"),
                          method = "method.AUC", 
                          parallel = cluster,
                          verbose = TRUE)
})
parallel::stopCluster(cluster)
summary(cv_slok_sl_mod_compulm)


# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/sl_cv_plot_eso_comppulm.png",
#     width = 2*480, height = 2*480)
plot(cv_slok_sl_mod_compulm)
dev.off()



################################################################

### DATA STOMACH
# divide data into X and Y part 
trainX <- maag_trn %>% select(age,sex,length,weight,
                              comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                              comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                              anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                              xtumhist,xtumlok2,scorecn1,scorecm,
                              anammed2,xscorect1,asascore,
                              xneoadjtype,resecok,procok,recontype,analig)
# outcome
trainY <- as.numeric(maag_trn$compnlek) - 1
# model matrix version
trainX_mm <- data.frame(model.matrix(trainY ~ ., data = trainX))

# divide data into X and Y part 
testX <- maag_tst %>% select(age,sex,length,weight,
                             comorbcar,comorbvas,comorbdia,comorbneu,comorbmda,comorburo,
                             comorbtro,comorbmus,comorbend,comorbinf,comorbmal,comorbove,
                             anamok1,anamok2,anamok3,anamok4,anamok5,anamok6,anamok7,
                             xtumhist,xtumlok2,scorecn1,scorecm,
                             anammed2,xscorect1,asascore,
                             xneoadjtype,resecok,procok,recontype,analig)
# outcome
testY <- as.numeric(maag_tst$compnlek) - 1
# model matrix version
testX_mm <- data.frame(model.matrix(testY ~ ., data = testX))


### CUSTOM MODELS SLOK_COMPPULM FOR SNOW PARALLEL
SL.knn.better_maagcompnlek <- create.Learner("SL.kernelKnn", params=list(k = maag_knn_mod_compnlek$bestTune$k))
SL.nn.better_maagcompnlek <- create.Learner("SL.nnet", params=list(size = maag_NN_mod_compnlek$bestTune$size, 
                                                               decay = maag_NN_mod_compnlek$bestTune$decay))
SL.svmPoly.better_maagcompnlek <- create.Learner("SL.ksvm", params=list(kernel = "polydot",
                                                                    degree = maag_svm_mod_compnlek_poly$bestTune$degree,
                                                                    scale = maag_svm_mod_compnlek_poly$bestTune$scale))
SL.svmRadial.better_maagcompnlek <- create.Learner("SL.ksvm", params=list(C =  maag_svm_mod_compnlek_radial$bestTune$C,
                                                                      kernel = "rbfdot",
                                                                      sigma = maag_svm_mod_compnlek_radial$bestTune$sigma))
SL.rf.better_maagcompnlek <- create.Learner("SL.randomForest", params=list(num.trees = 500, 
                                                                       mtry = maag_rf_mod_compnlek$bestTune$mtry))
SL.xgboost.better_maagcompnlek <- create.Learner("SL.xgboost", 
                                             params=list(max_depth = maag_ada_mod_compnlek$bestTune$maxdepth))


### SINGLE MODEL STOMACH COMPNLEK
# select number of cores
cluster = parallel::makeCluster(3)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, list(SL.knn.better_maagcompnlek$names,
                                      SL.nn.better_maagcompnlek$names,
                                      SL.svmPoly.better_maagcompnlek$names,
                                      SL.svmRadial.better_maagcompnlek$names,
                                      SL.rf.better_maagcompnlek$names))
parallel::clusterSetRNGStream(cluster, 1)
system.time({
  maag_sl_mod_compnlek = snowSuperLearner(Y = trainY, X = trainX_mm, family = binomial(),
                        cluster = cluster,
                        method = "method.AUC", 
                        SL.library = c("SL.mean",
                                       "SL.glm",
                                       "SL.glmnet",
                                       SL.knn.better_maagcompnlek$names,
                                       SL.nn.better_maagcompnlek$names,
                                       SL.svmPoly.better_maagcompnlek$names,
                                       SL.svmRadial.better_maagcompnlek$names,
                                       SL.rf.better_maagcompnlek$names,
                                       "SL.xgboost"))
})
parallel::stopCluster(cluster)
maag_sl_mod_compnlek
pred = predict(maag_sl_mod_compnlek, testX_mm, onlySL = TRUE)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(testY, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, testY)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc
###
# test results
probs <- as.numeric(pred$pred)
probs_maag_SL_mod_compnlek <- probs
# threshold probs
probs_t <- function(t) ifelse(probs > t, "Ja", "Nee")
probs_t_num <- function(t) ifelse(probs[,1] > t, "0", "1")
# confusion matrix based on t (default = .5)
confusionMatrix(data = factor(probs_t(.5)), reference = maag_tst$compnlek, positive = "Ja")
# TEST AUC
par(pty = "s") # plot type square
roc(maag_tst$compnlek, probs, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab =  "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#afb837",
    print.auc = TRUE)

### CROSS-VALIDATED STOMACH COMPNLEK
# select number of cores
cluster = parallel::makeCluster(3)
parallel::clusterEvalQ(cluster, library(SuperLearner))
parallel::clusterExport(cluster, list(SL.knn.better_compnlek$names,
                                      SL.nn.better_compnlek$names,
                                      SL.svmPoly.better_compnlek$names,
                                      SL.svmRadial.better_compnlek$names,
                                      SL.rf.better_compnlek$names))
parallel::clusterSetRNGStream(cluster, 1)
system.time({
  cv_maag_sl_mod_compnlek = CV.SuperLearner(X = trainX_mm, Y = trainY, 
                                         family = "binomial", 
                                         V = 10,
                                         SL.library = list("SL.mean",
                                                           "SL.glm", 
                                                           "SL.glmnet",
                                                           SL.knn.better_compnlek$names,
                                                           SL.nn.better_compnlek$names,
                                                           SL.svmPoly.better_compnlek$names,
                                                           SL.svmRadial.better_compnlek$names,
                                                           SL.rf.better_compnlek$names,
                                                           "SL.xgboost"),
                                         parallel = cluster,
                                         method = "method.AUC",
                                         verbose = TRUE)
})
parallel::stopCluster(cluster)
summary(cv_maag_sl_mod_compnlek)

# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/sl_cv_plot_sto_compnlek.png",
#     width = 2*480, height = 2*480)
plot(cv_maag_sl_mod_compnlek)
dev.off()
### ALL MODELS ##############################################################################

# slok_compnlek mods
slok_glm_mod_compnlek
slok_lasso_mod_compnlek$modelInfo
slok_knn_mod_compnlek
slok_NN_mod_compnlek
slok_svm_mod_compnlek_poly
slok_svm_mod_compnlek_radial
slok_rf_mod_compnlek
slok_ada_mod_compnlek
slok_sl_mod_compnlek

# slok_comppulm mods
slok_glm_mod_comppulm
slok_lasso_mod_comppulm
slok_knn_mod_comppulm
slok_NN_mod_comppulm
slok_svm_mod_compulm_poly
slok_svm_mod_compulm_radial
slok_rf_mod_comppulm
slok_ada_mod_comppulm
slok_sl_mod_compulm

# maag_comnlek mods
probs_maag_glm_mod_compnlek
maag_lasso_mod_compnlek
maag_knn_mod_compnlek
maag_NN_mod_compnlek
maag_svm_mod_compnlek_poly
maag_svm_mod_compnlek_radial
maag_rf_mod_compnlek
maag_ada_mod_compnlek
maag_sl_mod_compnlek

### PROBABILTY DATAFRAME #############################################################################
ls()

# probabs <- c("probs_maag_ada_mod_compnlek",       
# "probs_maag_glm_mod_compnlek",        "probs_maag_knn_mod_compnlek",        "probs_maag_lasso_mod_compnlek",      "probs_maag_NN_mod_compnlek",         "probs_maag_rf_mod_compnlek",         "probs_maag_svm_mod_compnlek_poly",  
# "probs_maag_svm_mod_compnlek_radial", "probs_slok_ada_mod_compnlek",        "probs_slok_ada_mod_comppulm",       "probs_slok_glm_mod_compnlek",        "probs_slok_glm_mod_comppulm",        "probs_slok_knn_mod_compnlek",       
# "probs_slok_knn_mod_comppulm",        "probs_slok_lasso_mod_compnlek",      "probs_slok_lasso_mod_comppulm",      "probs_slok_NN_mod_compnlek",         "probs_slok_NN_mod_comppulm",         "probs_slok_rf_mod_compnlek",        
# "probs_slok_rf_mod_comppulm",         "probs_slok_svm_mod_compnlek_poly",   "probs_slok_svm_mod_compnlek_radial", "probs_slok_svm_mod_compulm_poly",    "probs_slok_svm_mod_compulm_radial")

# Probabilities esophagus compnlek
slok_probs_compnlek <- tibble("eso_glm_compnlek" = probs_slok_glm_mod_compnlek$Ja,
                              "eso_lasso_compnlek" = probs_slok_lasso_mod_compnlek$Ja,
                              "eso_knn_compnlek" = probs_slok_knn_mod_compnlek$Ja,
                              "eso_svm_compnlek_poly" = probs_slok_svm_mod_compnlek_poly$Ja,
                              "eso_svm_compnlek_radial" = probs_slok_svm_mod_compnlek_radial$Ja,
                              "eso_NN_compnlek" = probs_slok_NN_mod_compnlek$Ja,
                              "eso_rf_compnlek" = probs_slok_rf_mod_compnlek$Ja,
                              "eso_ada_compnlek" = probs_slok_ada_mod_compnlek$Ja,
                              "eso_sl_compnlek" = probs_slok_SL_mod_compnlek)
# Probabilities esophagus comppulm
slok_probs_comppulm <- tibble("eso_glm_comppulm" = probs_slok_glm_mod_comppulm$Ja,
                              "eso_lasso_comppulm" = probs_slok_lasso_mod_comppulm$Ja,
                              "eso_knn_comppulm" = probs_slok_knn_mod_comppulm$Ja,
                              "eso_svm_comppulm_poly" = probs_slok_svm_mod_compulm_poly$Ja,
                              "eso_svm_comppulm_radial" = probs_slok_svm_mod_compulm_radial$Ja,
                              "eso_NN_comppulm" = probs_slok_NN_mod_comppulm$Ja,
                              "eso_rf_comppulm" = probs_slok_rf_mod_comppulm$Ja,
                              "eso_ada_comppulm" = probs_slok_ada_mod_comppulm$Ja,
                              "eso_sl_comppulm" = probs_slok_SL_mod_comppulm)
# Probabilities stomach compnlek
maag_probs_compnlek <- tibble("sto_glm_compnlek" = probs_maag_glm_mod_compnlek$Ja,
                              "sto_lasso_compnlek" = probs_maag_lasso_mod_compnlek$Ja,
                              "sto_knn_compnlek" = probs_maag_knn_mod_compnlek$Ja,
                              "sto_svm_compnlek_poly" = probs_maag_svm_mod_compnlek_poly$Ja,
                              "sto_svm_compnlek_radial" = probs_maag_svm_mod_compnlek_radial$Ja,
                              "sto_NN_compnlek" = probs_maag_NN_mod_compnlek$Ja,
                              "sto_rf_compnlek" = probs_maag_rf_mod_compnlek$Ja,
                              "sto_ada_compnlek" = probs_maag_ada_mod_compnlek$Ja,
                              "sto_sl_compnlek" = probs_maag_SL_mod_compnlek)

#
mod_names <- c("glm", "lasso", "knn", "svmPoly", "svmRadial", "NN", "rf", "ada", "SuperLearner")

### PROB PAIRS #############################################################################

# additional functions for PROB pairs plot
my_line <- function(x,y,...){
  points(x,y,...)
  abline(a = lm(y ~ x)$coefficients[1] , b = lm(y ~ x)$coefficients[2] , ...)
}
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); 
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r2 <- summary(lm(y ~ x))$r.squared
  txt <- format(c(r2, 0.123456789), digits = digits)[1]
  txt <- paste("r2 = ", txt, sep = "")
  text(0.5, 0.9, txt, cex = 1.5)
  
  res_se <- sqrt(deviance(lm(y ~ x))/df.residual(lm(y ~ x)))
  txt2 <- format(c(res_se, 0.123456789), digits = digits)[1]
  txt2 <- paste("r2 se = ", txt2, sep = "")
  text(0.5, .7, txt2, cex = 1.5)
  
  cor <- cor(x, y)
  txt3 <- format(c(cor, 0.123456789), digits = digits)[1]
  txt3 <- paste("cor = ", txt3, sep = "")
  text(0.5, .3, txt3, cex = 1.5)
  
  euc <- dist(rbind(x, y))
  txt4 <- format(c(euc, 0.123456789), digits = digits)[1]
  txt4 <- paste("eucl. = ", txt4, sep = "")
  text(0.5, .1, txt4, cex = 1.5)
}

### PROB PAIRS ESOPHAGUS COMPNLEK
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/probs_pairs_esophagus_compnlek2.png",
#     width = 2*480, height = 2*480)
pairs(slok_probs_compnlek, labels = mod_names, cex.labels=1.5, xlim = c(0,1), ylim = c(0,1),
      lower.panel = my_line, upper.panel = panel.cor)
dev.off()
### PROB PAIRS ESOPHAGUS COMPULM
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/probs_pairs_esophagus_comppulm2.png",
#     width = 2*480, height = 2*480)
pairs(slok_probs_comppulm, labels = mod_names, cex.labels=1.5, xlim = c(0,1), ylim = c(0,1), 
      lower.panel = my_line, upper.panel = panel.cor)
dev.off()
### PROB PAIRS STOMACH COMPNLEK
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/probs_pairs_stomach_compnlek2.png",
#     width = 2*480, height = 2*480)
pairs(maag_probs_compnlek, labels = mod_names, cex.labels=1.5, xlim = c(0,1), ylim = c(0,1),
      lower.panel = my_line, upper.panel = panel.cor)
dev.off()
### AUC PAIRS #############################################################################
### FUNCTIONS
dev.off()
pAUC <- function(var, prob_dat){
  set.seed(123)
  roc1 <- roc(var,
              pull(prob_dat[, 1]),
              xlab =  "False Positive Percentage",
              ylab = "True Positive Percentage", 
              percent=TRUE,
              # arguments for auc
              legacy.axes = TRUE,
              partial.auc=c(100, 90), partial.auc.correct=FALSE,
              partial.auc.focus="se",
              # arguments for ci
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              # arguments for plot
              plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
              print.auc=TRUE, print.auc.y = 50, show.thres=TRUE,
              col= "#fcbe03")
  roc2 <- roc(var, pull(prob_dat[, 2]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 45,
              partial.auc=c(100, 90), partial.auc.correct=FALSE,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#275c2d",
              partial.auc.focus="se")
  roc3 <- roc(var, pull(prob_dat[, 3]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 40,
              partial.auc=c(100, 90), partial.auc.correct=FALSE,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#fc0303",
              partial.auc.focus="se")
  roc4 <- roc(var, pull(prob_dat[, 4]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 35,
              partial.auc=c(100, 90), partial.auc.correct=FALSE,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#18fc03",
              partial.auc.focus="se")
  roc5 <- roc(var, pull(prob_dat[, 5]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 30,
              partial.auc=c(100, 90), partial.auc.correct=FALSE,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#9d03fc",
              partial.auc.focus="se")
  roc6 <- roc(var, pull(prob_dat[, 6]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 25,
              partial.auc=c(100, 90), partial.auc.correct=FALSE,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#fc03ca",
              partial.auc.focus="se")
  roc7 <- roc(var, pull(prob_dat[, 7]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 20,
              partial.auc=c(100, 90), partial.auc.correct=FALSE,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#03e3fc",
              partial.auc.focus="se")
  roc8 <- roc(var, pull(prob_dat[, 8]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 15,
              partial.auc=c(100, 90), partial.auc.correct=FALSE,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#031cfc",
              partial.auc.focus="se")
  roc9 <- roc(var, pull(prob_dat[, 9]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 10,
              partial.auc=c(100, 90), partial.auc.correct=FALSE,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#ff85c0",
              partial.auc.focus="se")
  legend("bottomright", legend=mod_names,
         col=c("#fcbe03", "#275c2d", "#fc0303", 
               "#18fc03", "#9d03fc", "#fc03ca", 
               "#03e3fc", "#031cfc", "#ff85c0"), lty=1, cex=1.2)
  
}

fullAUC <- function(var, prob_dat){
  set.seed(123)
  roc1 <- roc(var,
              pull(prob_dat[, 1]),
              xlab =  "False Positive Percentage",
              ylab = "True Positive Percentage", 
              percent=TRUE,
              # arguments for auc
              legacy.axes = TRUE,
              # arguments for ci
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              # arguments for plot
              plot=TRUE, grid=TRUE,
              print.auc=TRUE, print.auc.y = 50, show.thres=TRUE,
              col= "#fcbe03")
  roc2 <- roc(var, pull(prob_dat[, 2]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 45,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#275c2d")
  roc3 <- roc(var, pull(prob_dat[, 3]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 40,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#fc0303")
  roc4 <- roc(var, pull(prob_dat[, 4]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 35,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#18fc03")
  roc5 <- roc(var, pull(prob_dat[, 5]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 30,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#9d03fc")
  roc6 <- roc(var, pull(prob_dat[, 6]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 25,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#fc03ca")
  roc7 <- roc(var, pull(prob_dat[, 7]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 20,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#03e3fc")
  roc8 <- roc(var, pull(prob_dat[, 8]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 15,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#031cfc")
  roc9 <- roc(var, pull(prob_dat[, 9]),
              plot=TRUE, add=TRUE, percent=roc1$percent,
              print.auc=TRUE, print.auc.y = 10,
              ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
              col= "#ff85c0")
  legend("bottomright", legend=mod_names,
         col=c("#fcbe03", "#275c2d", "#fc0303", 
               "#18fc03", "#9d03fc", "#fc03ca", 
               "#03e3fc", "#031cfc", "#ff85c0"), lty=1, cex=1.2)
}

### AUC PAIRS ESO COMPNLEK #############################################################################

### OUTCOME SPECIFIC FUNCTIONS

part_auc_compnlek <- function(x, y, digits = 3, cex.cor, ...)
{
  usr <- par("usr"); 
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  a <- roc.test(slok_tst$compnlek, 
                predictor1 = x, 
                predictor2 = y, 
                partial.auc = c(100, 90), partial.auc.focus="se", partial.auc.correct=FALSE, percent = TRUE)
  
  # a part
  statist <- a$statistic
  txt <- format(c(statist, 0.123456789), digits = digits)[1]
  txt <- paste("d = ", txt, sep = "")
  text(0.5, 0.9, txt, cex = 1.3)
  
  p <- round(a$p.value, 3)
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p-value = ", txt2, sep = "")
  text(0.5, 0.7, txt2, cex = 1.3)
  
  auc1 <- a$roc1$auc
  txt3 <- format(c(auc1, 0.123456789), digits = digits)[1]
  txt3 <- paste("pAUC 1 = ", txt3, sep = "")
  text(0.5, 0.4, txt3, cex = 1.3)
  
  auc2 <- a$roc2$auc
  txt4 <- format(c(auc2, 0.123456789), digits = digits)[1]
  txt4 <- paste("pAUC2 = ", txt4, sep = "")
  text(0.5, 0.2, txt4, cex = 1.3)
  
}

full_auc_compnlek <- function(x, y, digits = 3, cex.cor, ...)
{
  usr <- par("usr"); 
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  d <- roc.test(slok_tst$compnlek, 
                predictor1 = y, 
                predictor2 = x, percent = TRUE)
  
  # a part
  statist <- d$statistic
  txt <- format(c(statist, 0.123456789), digits = digits)[1]
  txt <- paste("z = ", txt, sep = "")
  text(0.5, 0.9, txt, cex = 1.3)
  
  p <- round(d$p.value, 3)
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p-value = ", txt2, sep = "")
  text(0.5, 0.7, txt2, cex = 1.3)
  
  auc1 <- d$roc1$auc
  txt3 <- format(c(auc1, 0.123456789), digits = digits)[1]
  txt3 <- paste("AUC 1 = ", txt3, sep = "")
  text(0.5, 0.4, txt3, cex = 1.3)
  
  auc2 <- d$roc2$auc
  txt4 <- format(c(auc2, 0.123456789), digits = digits)[1]
  txt4 <- paste("AUC2 = ", txt4, sep = "")
  text(0.5, 0.2, txt4, cex = 1.3)
  
}

full_auc_compnlek2 <- function(x, y, digits = 3, cex.cor, ...)
{
  usr <- par("usr"); 
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  b <- roc(slok_tst$compnlek,
           y,
           xlab =  "False Positive Percentage",
           ylab = "True Positive Percentage", 
           percent=TRUE,
           # arguments for ci
           ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
           # arguments for plot
           plot=FALSE)
  
  c <- roc(slok_tst$compnlek,
           x,
           xlab =  "False Positive Percentage",
           ylab = "True Positive Percentage", 
           percent=TRUE,
           # arguments for ci
           ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
           # arguments for plot
           plot=FALSE)
  
  # b part
  AUC1 <- b$auc
  txt <- format(c(AUC1, 0.123456789), digits = digits)[1]
  txt <- paste("AUC 1 = ", txt, sep = "")
  text(0.5, 0.9, txt)
  
  AUC1_CI_L <- b$ci[1]
  txt2 <- format(c(AUC1_CI_L, 0.123456789), digits = digits)[1]
  txt2 <- paste("95% CI L = ", txt2, sep = "")
  text(0.5, 0.8, txt2)
  
  AUC1_CI_R <- b$ci[2]
  txt3 <- format(c(AUC1_CI_R, 0.123456789), digits = digits)[1]
  txt3 <- paste("95% CI R = ", txt3, sep = "")
  text(0.5, 0.7, txt3)
  
  # c part
  AUC2 <- c$auc
  txt4 <- format(c(AUC2, 0.123456789), digits = digits)[1]
  txt4 <- paste("AUC 2 = ", txt4, sep = "")
  text(0.5, 0.3, txt4)
  
  AUC2_CI_L <- c$ci[1]
  txt5 <- format(c(AUC2_CI_L, 0.123456789), digits = digits)[1]
  txt5 <- paste("95% CI L = ", txt5, sep = "")
  text(0.5, 0.2, txt5)
  
  AUC2_CI_R <- b$ci[2]
  txt6 <- format(c(AUC2_CI_R, 0.123456789), digits = digits)[1]
  txt6 <- paste("95% CI R = ", txt6, sep = "")
  text(0.5, 0.1, txt6)
}

### ROC AUC PAIRS ESOPHAGUS COMPNLEK
set.seed(123)
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/pAUC_pairs_eso_compnlek.png",
#     width = 2*480, height = 2*480)
pairs(slok_probs_compnlek, labels = mod_names, cex.labels=1.5, 
      lower.panel = part_auc_compnlek, upper.panel = NULL, xaxt='n', yaxt='n')
dev.off()
set.seed(123)
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/AUC_pairs_eso_compnlek.png",
#     width = 2*480, height = 2*480)
pairs(slok_probs_compnlek, labels = mod_names, cex.labels=1.5, 
      lower.panel = NULL, upper.panel = full_auc_compnlek, xaxt='n', yaxt='n')
dev.off()
### ROC CURVES WITH PARTIAL AUC
par(pty = "s") # plot type square
png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/pAUC_eso_compnlek_plot.png",
    width = 2*480, height = 2*480)
pAUC(slok_tst$compnlek, slok_probs_compnlek)
dev.off()
### ROC CURVES WITH FULL AUC
par(pty = "s") # plot type square
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/AUC_eso_compnlek_plot.png",
#     width = 2*480, height = 2*480)
fullAUC(slok_tst$compnlek, slok_probs_compnlek)
dev.off()

### AUC PAIRS ESO COMPUPULM #############################################################################
### OUTCOME SPECIFIC FUNCTIONS

part_auc_comppulm <- function(x, y, digits = 3, cex.cor, ...)
{
  usr <- par("usr"); 
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  a <- roc.test(slok_tst$comppulm, 
                predictor1 = x, 
                predictor2 = y, 
                partial.auc = c(100, 90), partial.auc.focus="se",partial.auc.correct=FALSE, percent = TRUE)
  
  # a part
  statist <- a$statistic
  txt <- format(c(statist, 0.123456789), digits = digits)[1]
  txt <- paste("d = ", txt, sep = "")
  text(0.5, 0.9, txt, cex = 1.3)
  
  p <- round(a$p.value, 3)
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p-value = ", txt2, sep = "")
  text(0.5, 0.7, txt2, cex = 1.3)
  
  auc1 <- a$roc1$auc
  txt3 <- format(c(auc1, 0.123456789), digits = digits)[1]
  txt3 <- paste("pAUC 1 = ", txt3, sep = "")
  text(0.5, 0.4, txt3, cex = 1.3)
  
  auc2 <- a$roc2$auc
  txt4 <- format(c(auc2, 0.123456789), digits = digits)[1]
  txt4 <- paste("pAUC2 = ", txt4, sep = "")
  text(0.5, 0.2, txt4, cex = 1.3)
  
}

full_auc_comppulm <- function(x, y, digits = 3, cex.cor, ...)
{
  usr <- par("usr"); 
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  d <- roc.test(slok_tst$comppulm, 
                predictor1 = y, 
                predictor2 = x, percent = TRUE)
  
  # a part
  statist <- d$statistic
  txt <- format(c(statist, 0.123456789), digits = digits)[1]
  txt <- paste("z = ", txt, sep = "")
  text(0.5, 0.9, txt, cex = 1.3)
  
  p <- round(d$p.value, 3)
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p-value = ", txt2, sep = "")
  text(0.5, 0.7, txt2, cex = 1.3)
  
  auc1 <- d$roc1$auc
  txt3 <- format(c(auc1, 0.123456789), digits = digits)[1]
  txt3 <- paste("AUC 1 = ", txt3, sep = "")
  text(0.5, 0.4, txt3, cex = 1.3)
  
  auc2 <- d$roc2$auc
  txt4 <- format(c(auc2, 0.123456789), digits = digits)[1]
  txt4 <- paste("AUC2 = ", txt4, sep = "")
  text(0.5, 0.2, txt4, cex = 1.3)
  
}

### ROC AUC PAIRS ESOPHAGUS COMPPULM
set.seed(123)
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/pAUC_pairs_eso_comppulm.png",
#     width = 2*480, height = 2*480)
pairs(slok_probs_comppulm, labels = mod_names, cex.labels=1.5, 
      lower.panel = part_auc_comppulm, upper.panel = NULL, xaxt='n', yaxt='n')
dev.off()
set.seed(123)
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/AUC_pairs_eso_comppulm.png",
#     width = 2*480, height = 2*480)
pairs(slok_probs_comppulm, labels = mod_names, cex.labels=1.5, 
      lower.panel = NULL, upper.panel = full_auc_comppulm, xaxt='n', yaxt='n')
dev.off()
### ROC CURVES WITH PARTIAL AUC
par(pty = "s") # plot type square
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/pAUC_eso_comppulm_plot.png",
#     width = 2*480, height = 2*480)
pAUC(slok_tst$comppulm, slok_probs_comppulm)
dev.off()
### ROC CURVES WITH FULL AUC
par(pty = "s") # plot type square
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/AUC_eso_comppulm_plot.png",
#     width = 2*480, height = 2*480)
fullAUC(slok_tst$comppulm, slok_probs_comppulm)
dev.off()
### AUC PAIRS ESO COMPUPULM #############################################################################
### OUTCOME SPECIFIC FUNCTIONS
part_auc_maag <- function(x, y, digits = 3, cex.cor, ...)
{
  usr <- par("usr"); 
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  a <- roc.test(maag_tst$compnlek, 
                predictor1 = x, 
                predictor2 = y, 
                partial.auc = c(100, 90), partial.auc.focus="se",partial.auc.correct=FALSE, percent = TRUE)
  
  # a part
  statist <- a$statistic
  txt <- format(c(statist, 0.123456789), digits = digits)[1]
  txt <- paste("d = ", txt, sep = "")
  text(0.5, 0.9, txt, cex = 1.3)
  
  p <- round(a$p.value, 3)
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p-value = ", txt2, sep = "")
  text(0.5, 0.7, txt2, cex = 1.3)
  
  auc1 <- a$roc1$auc
  txt3 <- format(c(auc1, 0.123456789), digits = digits)[1]
  txt3 <- paste("pAUC 1 = ", txt3, sep = "")
  text(0.5, 0.4, txt3, cex = 1.3)
  
  auc2 <- a$roc2$auc
  txt4 <- format(c(auc2, 0.123456789), digits = digits)[1]
  txt4 <- paste("pAUC2 = ", txt4, sep = "")
  text(0.5, 0.2, txt4, cex = 1.3)
  
}

full_auc_maag <- function(x, y, digits = 3, cex.cor, ...)
{
  usr <- par("usr"); 
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  d <- roc.test(maag_tst$compnlek, 
                predictor1 = y, 
                predictor2 = x, percent = TRUE)
  
  # a part
  statist <- d$statistic
  txt <- format(c(statist, 0.123456789), digits = digits)[1]
  txt <- paste("z = ", txt, sep = "")
  text(0.5, 0.9, txt, cex = 1.3)
  
  p <- round(d$p.value, 3)
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p-value = ", txt2, sep = "")
  text(0.5, 0.7, txt2, cex = 1.3)
  
  auc1 <- d$roc1$auc
  txt3 <- format(c(auc1, 0.123456789), digits = digits)[1]
  txt3 <- paste("AUC 1 = ", txt3, sep = "")
  text(0.5, 0.4, txt3, cex = 1.3)
  
  auc2 <- d$roc2$auc
  txt4 <- format(c(auc2, 0.123456789), digits = digits)[1]
  txt4 <- paste("AUC2 = ", txt4, sep = "")
  text(0.5, 0.2, txt4, cex = 1.3)
  
}

### ROC AUC PAIRS ESOPHAGUS COMPPULM
set.seed(123)
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/pAUC_pairs_sto_compnlek.png",
#     width = 2*480, height = 2*480)
pairs(maag_probs_compnlek, labels = mod_names, cex.labels=1.5, 
      lower.panel = part_auc_maag, upper.panel = NULL, xaxt='n', yaxt='n')
dev.off()

set.seed(123)
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/AUC_pairs_sto_compnlek.png",
#     width = 2*480, height = 2*480)
pairs(maag_probs_compnlek, labels = mod_names, cex.labels=1.5, 
      lower.panel = NULL, upper.panel = full_auc_maag, xaxt='n', yaxt='n')
dev.off()
### ROC CURVES WITH PARTIAL AUC
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/pAUC_sto_compnlek_plot.png",
#     width = 2*480, height = 2*480)
par(pty = "s") # plot type square
pAUC(maag_tst$compnlek, maag_probs_compnlek)
dev.off()
### ROC CURVES WITH FULL AUC
# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/AUC_sto_compnlek_plot.png",
#     width = 2*480, height = 2*480)
par(pty = "s") # plot type square
fullAUC(maag_tst$compnlek, maag_probs_compnlek)
dev.off()

### MODEL TIMES #############################################################################
# slok_compnlek mods
slok_glm_mod_compnlek$times
slok_lasso_mod_compnlek$times
slok_knn_mod_compnlek$times
slok_NN_mod_compnlek$times
slok_svm_mod_compnlek_poly$times
slok_svm_mod_compnlek_radial$times
slok_rf_mod_compnlek$times
slok_ada_mod_compnlek$times
slok_sl_mod_compnlek$times

# slok_comppulm mods
slok_glm_mod_comppulm$times
slok_lasso_mod_comppulm$times
slok_knn_mod_comppulm$times
slok_NN_mod_comppulm$times
slok_svm_mod_compulm_poly$times
slok_svm_mod_compulm_radial$times
slok_rf_mod_comppulm$times
slok_ada_mod_comppulm$times
slok_sl_mod_compulm$times

# maag_comnlek mods
probs_maag_glm_mod_compnlek$times
maag_lasso_mod_compnlek$times
maag_knn_mod_compnlek$times
maag_NN_mod_compnlek$times
maag_svm_mod_compnlek_poly$times
maag_svm_mod_compnlek_radial$times
maag_rf_mod_compnlek$times
maag_ada_mod_compnlek$times
maag_sl_mod_compnlek$times

### MODEL TIMES #############################################################################
dev.off()
mod_names <- c("glm", "lasso", "knn", "svmPoly", "svmRadial", "NN", "rf", "ada", "SuperLearner")
flexibility <- c(.1, .0, .4, .85, .9, .8, .7, .7, .8)
interpretability <- c(.8, .9, .4, .3, .3, .2, .6, .5, .1)
time <- c("fast", "fast", "medium", "slow", "medium", "slow", "slow",  'slow', "medium")
time_col <- c("green", "green", "orange", "red", "orange", "red", "red",  'red', "orange")

# png("C:/Users/Bou/Documents/Stat Science/THESIS/Text/plots/flex-int.png",
#     width = 2*480, height = 2*480)
plot(flexibility, interpretability,
     main = "Flexibility vs. Interpretability",
     xlab = "Flexibility",
     ylab = "Interpretability",
     col = time_col,
     pch = 16,
     xlim = c(0,1),
     ylim = c(0,1), 
     xaxt='n', yaxt='n')
pos_vector <- c(3,3,3,3,1,3,3,3,3)
text(flexibility, interpretability, labels = mod_names, 
     cex= 1, pos=pos_vector, font = 3)
title(sub="high", adj=1, line=0, font=2)
title(sub="low", adj=0, line=0, font=2)