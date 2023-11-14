getwd()

setwd("~/Desktop")

install.packages("remotes")
remotes::install_github("masurp/ggmirt")

library(foreign)
library(haven)
library(psych)
library(car)
library(lavaan)
library(summarytools)
library(dplyr)
library(mirt)
library(ggmirt)
library(ggplot2)

data<-read_sav("fatalism.sav")
data2 <- data %>% dplyr::filter(consent==1 & data$check==4)

load("fatalism.Rdata")

################################################################################
######### 0-1 recoding function
std01<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}

################################################################################


## Recoding 

# 1. Ladder needs recoding, where 10 indicates highest rung of the ladder 
data2$laddernew <- 11 - data2$ladder # subtracted all ladder values from 11 so that they are in reverse order from 1 to 10

## Dummy Variables

# 2. Resources need to be renamed to the specific variable 
names(data2)[names(data2) == "resource_1"] <- "time"
names(data2)[names(data2) == "resource_2"] <- "money"
names(data2)[names(data2) == "resource_3"] <- "energy"
names(data2)[names(data2) == "resource_4"] <- "space"

# recode resource items (high = more than enough)
data2$time <- car::recode(data2$time, "1=2; 3=1; 2=0")
data2$money <- car::recode(data2$money, "1=2; 3=1; 2=0")
data2$energy <- car::recode(data2$energy, "1=2; 3=1; 2=0")
data2$space <- car::recode(data2$space, "1=2; 3=1; 2=0")

data2$ressc<-std01(rowMeans(with(data2, cbind(time, money, energy, space))))

# 3. Race needs to become their own dummy variables 
names(data2)[names(data2) == "race_1"] <- "Asian"
names(data2)[names(data2) == "race_2"] <- "Black"
names(data2)[names(data2) == "race_3"] <- "Latinx"
names(data2)[names(data2) == "race_4"] <- "Arabic"
names(data2)[names(data2) == "race_5"] <- "Pacific"
names(data2)[names(data2) == "race_6"] <- "Native"
names(data2)[names(data2) == "race_7"] <- "White"
names(data2)[names(data2) == "race_8"] <- "Other"

for (i in 1:nrow(data2)) {
  if (is.na(data2[i, "White"])) {
    data2[i, "racewhite"] <- 0
  } else if (data2[i, "White"] == 1) {
    data2[i, "racewhite"] <- 1
  } else {
    data2[i, "racewhite"] <- 0
  } 
}

# 4. Behavior Dummy Variables 
names(data2)[names(data2) == "behavior_1"] <- "Meeting"
names(data2)[names(data2) == "behavior_2"] <- "Donation"
names(data2)[names(data2) == "behavior_3"] <- "Protest"
names(data2)[names(data2) == "behavior_4"] <- "SMpost"
names(data2)[names(data2) == "behavior_5"] <- "Persuasion"
names(data2)[names(data2) == "behavior_6"] <- "Sticker"
names(data2)[names(data2) == "behavior_7"] <- "Donation2"
names(data2)[names(data2) == "behavior_8"] <- "Argument"

# 5. Emotions (these are not dummy variables, but ordinal responses)
names(data2)[names(data2) == "emotions_2_1"] <- "Hopeful"
names(data2)[names(data2) == "emotions_2_2"] <- "Afraid"
names(data2)[names(data2) == "emotions_2_3"] <- "Outrage"
names(data2)[names(data2) == "emotions_2_4"] <- "Angry"
names(data2)[names(data2) == "emotions_2_5"] <- "Guilty"
names(data2)[names(data2) == "emotions_2_6"] <- "Happy"
names(data2)[names(data2) == "emotions_2_7"] <- "Worried"
names(data2)[names(data2) == "emotions_2_8"] <- "Proud"
names(data2)[names(data2) == "emotions_2_9"] <- "Irritated"
names(data2)[names(data2) == "emotions_2_10"] <- "Nervous"

# negative emotion scale
psych::alpha(with(data2, cbind(Afraid, Outrage, Angry, Nervous, 
                               Irritated, Guilty, Worried)))
data2$negsc<-std01(rowMeans(with(data2, cbind(Afraid, Outrage, Angry, Nervous, 
                                        Irritated, Guilty, Worried))))
# positive emotion scale
psych::alpha(with(data2, cbind(Hopeful, Happy, Proud)))
data2$possc<-std01(rowMeans(with(data2, cbind(Hopeful, Happy, Proud))))

# 6. Policy Behavior Dummy Variables 
names(data2)[names(data2) == "HL_response_opt2_1__1"] <- "HL_Meeting"
names(data2)[names(data2) == "HL_response_opt2_1__2"] <- "HL_Donation"
names(data2)[names(data2) == "HL_response_opt2_1__3"] <- "HL_Protest"
names(data2)[names(data2) == "HL_response_opt2_1__4"] <- "HL_SMpost"
names(data2)[names(data2) == "HL_response_opt2_1__5"] <- "HL_Persuasion"
names(data2)[names(data2) == "HL_response_opt2_1__6"] <- "HL_Sticker"
names(data2)[names(data2) == "HL_response_opt2_1__7"] <- "HL_Donation2"
names(data2)[names(data2) == "HL_response_opt2_1__8"] <- "HL_Argument"

# behavior items -- recode each into new dummy variable where 1 = did, 0 = did
# not -- and then average into scale / proportion of behaviors done
data2$hlp1<-ifelse(is.na(data2$HL_Meeting), 0, 1)
data2$hlp2<-ifelse(is.na(data2$HL_Donation), 0, 1)
data2$hlp3<-ifelse(is.na(data2$HL_Protest), 0, 1)
data2$hlp4<-ifelse(is.na(data2$HL_SMpost), 0, 1)
data2$hlp5<-ifelse(is.na(data2$HL_Persuasion), 0, 1)
data2$hlp6<-ifelse(is.na(data2$HL_Sticker), 0, 1)
data2$hlp7<-ifelse(is.na(data2$HL_Donation2), 0, 1)
data2$hlp8<-ifelse(is.na(data2$HL_Argument), 0, 1)

data2$behsc<-std01(rowMeans(with(data2, cbind(hlp1, hlp2, hlp3, hlp4, hlp5,
                                              hlp6, hlp7, hlp8))))

## Creating scales
# 1. Create full fatalism scale (just average out the scores)
data2$fatalism <- std01((data2$fatalism_1+ data2$fatalism_2+ data2$fatalism_3+ 
                     data2$fatalism_4+ data2$fatalism_5 + data2$fatalism_6+ 
                     data2$fatalism_7+ data2$fatalism_8+ data2$fatalism_9+ 
                     data2$fatalism_10+ data2$fatalism_11+ data2$fatalism_12+ 
                     data2$fatalism_13)/13)

# full scale (alpha = 0.83)
psych::alpha(with(data2, cbind(fatalism_1,  fatalism_10,                                
                               fatalism_11,                               
                               fatalism_12,                               
                               fatalism_13,                                
                               fatalism_2,                                 
                               fatalism_3,                                 
                               fatalism_4,                                
                               fatalism_5,                                
                               fatalism_6,                                
                               fatalism_7,                                 
                               fatalism_8,                                 
                               fatalism_9)))
# pre-determination (alpha = 0.76)
psych::alpha(with(data2, cbind(fatalism_1,                               
                               fatalism_2,                                 
                               fatalism_3,                                 
                               fatalism_4,                                
                               fatalism_5,                                
                               fatalism_6,                                
                               fatalism_7,                                 
                               fatalism_8)))
# pessimism (alpha = 0.69)
psych::alpha(with(data2, cbind(fatalism_10,                                
                               fatalism_11,                               
                               fatalism_12,                               
                               fatalism_13,                                
                               fatalism_9)))

# not clear that 2 factor structure fits better than one-factor

# internal efficacy (high = more)
data2$ie1<-car::recode(data2$ieff1, "1=1; 2=2; 4=3; 6=4; 7=5")
data2$ie2<-car::recode(data2$ieff2, "1=1; 2=2; 4=3; 6=4; 7=5")
data2$ie3<-car::recode(data2$ieff3, "1=1; 2=2; 4=3; 6=4; 7=5")
data2$ineff <- std01(rowMeans(with(data2, cbind(ie1, ie2, ie3))))

# external efficacy (high = more)
data2$ee1<-car::recode(data2$eeff1, "1=1; 2=2; 4=3; 6=4; 7=5")
data2$ee2<-car::recode(data2$eeff2, "1=1; 2=2; 4=3; 6=4; 7=5")
data2$ee3<-car::recode(data2$eeff3, "1=5; 2=4; 4=3; 6=2; 7=1")
data2$ee4<-car::recode(data2$eeff4, "1=5; 2=4; 4=3; 6=2; 7=1")
data2$exeff <- std01(rowMeans(with(data2, cbind(ee1, ee2, ee3, ee4))))

# satisfaction with level of engagement (high = more satisfaction)
data2$gs1<-std01(3-data2$guilt_satisfaction_1)
data2$gs2<-std01(data2$guilt_satisfaction_2-1)

### ideal and real point items -- make each into a dummy variable that = 1 if
### checked, 0 = if not checked. These will be the input items for the IRT 
### analysis.

### minyoung -- finish these:

# code ideal point items -- use this format
# there is no idealpoint_16 (only 19 items)
data2$ip1<-ifelse(is.na(data2$idealpoint_1), 0, 1)
data2$ip2<-ifelse(is.na(data2$idealpoint_2), 0, 1)
data2$ip3<-ifelse(is.na(data2$idealpoint_3), 0, 1)
data2$ip4<-ifelse(is.na(data2$idealpoint_4), 0, 1)
data2$ip5<-ifelse(is.na(data2$idealpoint_5), 0, 1)
data2$ip6<-ifelse(is.na(data2$idealpoint_6), 0, 1)
data2$ip7<-ifelse(is.na(data2$idealpoint_7), 0, 1)
data2$ip8<-ifelse(is.na(data2$idealpoint_8), 0, 1)
data2$ip9<-ifelse(is.na(data2$idealpoint_9), 0, 1)
data2$ip10<-ifelse(is.na(data2$idealpoint_10), 0, 1)
data2$ip11<-ifelse(is.na(data2$idealpoint_11), 0, 1)
data2$ip12<-ifelse(is.na(data2$idealpoint_12), 0, 1)
data2$ip13<-ifelse(is.na(data2$idealpoint_13), 0, 1)
data2$ip14<-ifelse(is.na(data2$idealpoint_14), 0, 1)
data2$ip15<-ifelse(is.na(data2$idealpoint_15), 0, 1)
data2$ip17<-ifelse(is.na(data2$idealpoint_17), 0, 1)
data2$ip18<-ifelse(is.na(data2$idealpoint_18), 0, 1)
data2$ip19<-ifelse(is.na(data2$idealpoint_19), 0, 1)
data2$ip20<-ifelse(is.na(data2$idealpoint_20), 0, 1)

# code ideal point items -- use this format
# there is no realpoint_20 (only 19 items)

data2$rp1<-ifelse(is.na(data2$realpoint_1), 0, 1)
data2$rp2<-ifelse(is.na(data2$realpoint_2), 0, 1)
data2$rp3<-ifelse(is.na(data2$realpoint_3), 0, 1)
data2$rp4<-ifelse(is.na(data2$realpoint_4), 0, 1)
data2$rp5<-ifelse(is.na(data2$realpoint_5), 0, 1)
data2$rp6<-ifelse(is.na(data2$realpoint_6), 0, 1)
data2$rp7<-ifelse(is.na(data2$realpoint_7), 0, 1)
data2$rp8<-ifelse(is.na(data2$realpoint_8), 0, 1)
data2$rp9<-ifelse(is.na(data2$realpoint_9), 0, 1)
data2$rp10<-ifelse(is.na(data2$realpoint_10), 0, 1)
data2$rp11<-ifelse(is.na(data2$realpoint_11), 0, 1)
data2$rp12<-ifelse(is.na(data2$realpoint_12), 0, 1)
data2$rp13<-ifelse(is.na(data2$realpoint_13), 0, 1)
data2$rp14<-ifelse(is.na(data2$realpoint_14), 0, 1)
data2$rp15<-ifelse(is.na(data2$realpoint_15), 0, 1)
data2$rp16<-ifelse(is.na(data2$realpoint_16), 0, 1)
data2$rp17<-ifelse(is.na(data2$realpoint_17), 0, 1)
data2$rp18<-ifelse(is.na(data2$realpoint_18), 0, 1)
data2$rp19<-ifelse(is.na(data2$realpoint_19), 0, 1)

# code political knowledge
data2$Q2 <- as.character(data2$Q2)
data2$Q3 <- as.character(data2$Q3)
data2$Q4 <- as.character(data2$Q4)
data2$Q5 <- as.character(data2$Q5)

data2$Q2rc <- ifelse(data2$Q2 == "2", 1,0)
data2$Q3rc <- ifelse(data2$Q3 == "5", 1,0)
data2$Q4rc <- ifelse(data2$Q4 == "2", 1,0)
data2$Q5rc <- ifelse(data2$Q5 == "7", 1,0)

data2$pk <- std01(data2$Q2rc + data2$Q3rc + data2$Q4rc + data2$Q5rc)

psych::alpha(with(data2, cbind(Q2rc, Q3rc, Q4rc, Q5rc)))

# minyoung -- code political interest
data2$interestrc <- 5-data2$interest

psych::alpha(with(data2, cbind(interestrc, ic1, ic2)))

data2$pi <- std01(data2$interestrc + data2$ic1 + data2$ic2) 

################################################################################

##### save coded dataframe as Rdata
save(data2, file="fatalism.Rdata")

################################################################################
### exploratory factor analysis on fatalism items, full scale
### appears to be 2 factors, but not predicted ones

# item-total correlations
# eliiminate all items with r<0.30
# no items eliminated
corr.test(data2[c("fatalism", "fatalism_1", "fatalism_10",                                
                  "fatalism_11",                               
                  "fatalism_12",                               
                  "fatalism_13",                                
                  "fatalism_2",                                 
                  "fatalism_3",                                 
                  "fatalism_4",                                
                  "fatalism_5",                                
                  "fatalism_6",                                
                  "fatalism_7",                                 
                  "fatalism_8",                                 
                  "fatalism_9")])

# full set of items to consider
items<-data2[c("fatalism_1", "fatalism_10",                                
               "fatalism_11",                               
               "fatalism_12",                               
               "fatalism_13",                                
               "fatalism_2",                                 
               "fatalism_3",                                 
               "fatalism_4",                                
               "fatalism_5",                                
               "fatalism_6",                                
               "fatalism_7",                                 
               "fatalism_8",                                 
               "fatalism_9")]

# KMO (must be over 0.50) and Bartlett sphericity (must be p<0.05)
# no items eliminated
items2<-na.omit(items)
r<- cor(items2)
KMO(r)
cortest.bartlett(r, n = 990)

# EFA on full set of items
# parallel analysis --> 2 factors
# factors do not clearly reflect structure of original scale
# all communalities>0.20, so no items eliminated on that basis
# cross-loading items: 1, 2, 5 (according to 75% rule), so drop
fa.parallel(items, fa="pc", n.iter=50)
fact <- fa(items, nfactors=2,  fm="pa", rotate = "oblimin")
summary(fact)
print(fact)

# reduce item pool by dropping 1, 2, 5:
items<-data2[c("fatalism_10",                                
               "fatalism_11",                               
               "fatalism_12",                               
               "fatalism_13",                                
               "fatalism_3",                                 
               "fatalism_4",                                
               "fatalism_6",                                
               "fatalism_7",                                
               "fatalism_8",                                 
               "fatalism_9")]
# reduced scale -- still two factors
# but factor 2 is just items 3 and 6, which have almost identical wording
# in CFA solutions, errors for these items correlate highly
# choose 6 only: higher loading in original solution, more clearly worded
fa.parallel(items, fa="pc", n.iter=50)
fact <- fa(items, nfactors=2,  fm="pa", rotate = "oblimin")
summary(fact)
print(fact)

# reduce item pool again by dropping 3:
items<-data2[c("fatalism_10",                                
               "fatalism_11",                               
               "fatalism_12",                               
               "fatalism_13",                                
               "fatalism_4",                                
               "fatalism_6",                                
               "fatalism_7",                                
               "fatalism_8",                                 
               "fatalism_9")]
# reduced scale -- parallel analysis suggests 1 factor, but factor 2 eigenvalue
# is just below cutoff. In two-factor model, 13 cross-loads and factor 2 is all
# item 6. In one-factor model, 6 and 13 have communalities below <0.20, and 6 
# has a loading <0.30. Suggests dropping 6, 13. 
fa.parallel(items, fa="pc", n.iter=50)
fact <- fa(items, nfactors=1,  fm="pa", rotate = "oblimin")
summary(fact)
print(fact)
fact <- fa(items, nfactors=2,  fm="pa", rotate = "oblimin")
summary(fact)
print(fact)

# of the non-crossloading items, only 3 and 6 load on factor 2
# these two (3 and 6) have virtually identical wording -- choose 6 only

# reduce item pool again by dropping 6, 13:
items<-data2[c("fatalism_10",                                
               "fatalism_11",                               
               "fatalism_12",                               
               "fatalism_4",                                
               "fatalism_7",                                 
               "fatalism_8",                                 
               "fatalism_9")]
fa.parallel(items, fa="pc", n.iter=50)
# this version is unidimensional, w/ 3 pre-determination items and 4 pessimism
# items from the original scale, all loadings > 0.50
fact <- fa(items, nfactors=1,  fm="pa", rotate = "oblimin")
summary(fact)
print(fact)

# do GRM on this unidimensional model:
f1<-mirt(data = items, 
          model = 1, 
          itemtype = "graded", 
          verbose = FALSE)
summary(f1)
coef(f1)
# area under information curve for each item
for(i in 1:7){
  message("item: ",i)
  print(areainfo(f1, c(-10,10), which.items = i))  
}
# item information curves
ggmirt::itemInfoPlot(f1, 
                     facet = F, 
                     legend=T, 
                     title = "Fatalism: Item Information Curves") +
  geom_line(linewidth=.75, alpha=.6) +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5)) +
  scale_color_brewer("Item", 
                     labels=c("f10", "f11", "f12", "f4", "f7", "f8", "f9"), 
                     palette = "Paired") 
# 7 and 12 have lowest information, do not cover regions of theta not covered
# by other items in the scale. But both 7 and 12 still have info>3, and 7 is
# one of only 3 pre-determination items. No obvious suggestions for deletion.

# reduced scale: alpha = 0.77 (versus alpha = 0.83 for full 13 item scale)
psych::alpha(data2[c("fatalism_10",                                
                     "fatalism_11",                               
                     "fatalism_12",                               
                     "fatalism_4",                                
                     "fatalism_7",                                 
                     "fatalism_8",                                 
                     "fatalism_9")])

# reduced scale: r = 0.88 with full 13 item scale
data2$fshort<-rowMeans(data2[c("fatalism_10",                                
                               "fatalism_11",                               
                               "fatalism_12",                               
                               "fatalism_4",                                
                               "fatalism_7",                                 
                               "fatalism_8",                                 
                               "fatalism_9")])
corr.test(data2$fatalism, data2$fshort)

# confirmatory factor analysis: 1 factor model for short scale
# good fit, all loadings >0.50
# but need to confirm on an independent sample
cf1<- 'pf =~ fatalism_4 + fatalism_7 + fatalism_8 +
             fatalism_9 + fatalism_10 + fatalism_11 + fatalism_12
      '
fit.cf1<- cfa(cf1, estimator = "MLR", data2)
summary(fit.cf1, fit.measures=TRUE, standardized=TRUE)

################################################################################
### short scale plus internal and external efficacy:
items<-data2[c("fatalism_10",                                
               "fatalism_11",                               
               "fatalism_12",                               
               "fatalism_4",                                
               "fatalism_7",                                 
               "fatalism_8",                                 
               "fatalism_9",
               "ie1",
               "ie2",
               "ie3",
               "ee1", 
               "ee2", 
               "ee3", 
               "ee4")]
# three factors
fa.parallel(items, fa="pc", n.iter=50)
# fatalism and efficacy scales are separable
fact <- fa(items, nfactors=3,  fm="pa", rotate = "oblimin")
summary(fact)
print(fact)

# confirmatory factor analysis: 3 factors
# good fit, all loadings >0.50
# but need to confirm on an independent sample
cf3<- 'pf =~ fatalism_4 + fatalism_7 + fatalism_8 +
             fatalism_9 + fatalism_10 + fatalism_11 + fatalism_12
      ie =~ ie1 + ie2 + ie3
      ee =~ ee1 + ee2 + ee3 + ee4
      '
fit.cf3<- cfa(cf3, estimator = "MLR", items)
summary(fit.cf3, fit.measures=TRUE, standardized=TRUE)

cf2<- 'pf =~ fatalism_4 + fatalism_7 + fatalism_8 +
             fatalism_9 + fatalism_10 + fatalism_11 + fatalism_12 +
      ee1 + ee2 + ee3 + ee4
      ie =~ ie1 + ie2 + ie3
      '
fit.cf2<- cfa(cf2, estimator = "MLR", items)
summary(fit.cf2, fit.measures=TRUE, standardized=TRUE)

lavTestLRT(fit.cf2, fit.cf3, method = "satorra.bentler.2010")

# some correlations
corr.test(with(data2, cbind(fatalism, fshort, ineff, exeff)))

################################################################################
### long scale plus internal and external efficacy
items<-data2[c("fatalism_1", "fatalism_10",                                
               "fatalism_11",                               
               "fatalism_12",                               
               "fatalism_13",                                
               "fatalism_2",                                 
               "fatalism_3",                                 
               "fatalism_4",                                
               "fatalism_5",                                
               "fatalism_6",                                
               "fatalism_7",                                 
               "fatalism_8",                                 
               "fatalism_9",
               "ie1",
               "ie2",
               "ie3",
               "ee1", 
               "ee2", 
               "ee3", 
               "ee4")]
# three factors
fa.parallel(items, fa="pc", n.iter=50)
# fatalism and efficacy scales are generally separable, but 3 and 6 are again
# creating their own factor.
fact <- fa(items, nfactors=3,  fm="pa", rotate = "oblimin")
summary(fact)
print(fact)

### long scale plus internal and external efficacy (minus 3 and 6)
items<-data2[c("fatalism_1", "fatalism_10",                                
               "fatalism_11",                               
               "fatalism_12",                               
               "fatalism_13",                                
               "fatalism_2",                                 
               "fatalism_4",                                
               "fatalism_5",                                
               "fatalism_7",                                 
               "fatalism_8",                                 
               "fatalism_9",
               "ie1",
               "ie2",
               "ie3",
               "ee1", 
               "ee2", 
               "ee3", 
               "ee4")]
# three factors
fa.parallel(items, fa="pc", n.iter=50)
# fatalism and efficacy scales are generally separable, but 3 and 6 are again
# creating their own factor.
fact <- fa(items, nfactors=3,  fm="pa", rotate = "oblimin")
summary(fact)
print(fact)
