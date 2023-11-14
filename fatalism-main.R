getwd()

setwd("~/Desktop")

library(foreign)
library(haven)
library(psych)
library(car)
library(lavaan)
library(summarytools)
library(dplyr)
library(reshape2)
library(R2jags)
library(ggplot2)
library(estimatr)

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

psych::alpha(with(data2, cbind(time, money, energy, space)))
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

# full scale (alpha = 0.81)
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
data2$fshort<-rowMeans(data2[c("fatalism_10",                                
                               "fatalism_11",                               
                               "fatalism_12",                               
                               "fatalism_4",                                
                               "fatalism_7",                                 
                               "fatalism_8",                                 
                               "fatalism_9")])


# factor analysis on fatalism items
#model setup 2 factor 
cf2<- 'pd =~ fatalism_1 + fatalism_2 + fatalism_3 + fatalism_4 + fatalism_5 + 
             fatalism_6 + fatalism_7 + fatalism_8
       pes =~ fatalism_9 + fatalism_10 + fatalism_11 + fatalism_12 +
             fatalism_13
       fatalism_3 ~~  fatalism_6      # similar wording
       '
#confirmatory factor analysis for 2 factor model
# items 3 and 7 load weakly
# lots of cross-loadings
# one strong error covariance due to similar item wording
fit.cf2<- cfa(cf2, estimator = "MLR", data2)
summary(fit.cf2, fit.measures=TRUE, standardized=TRUE)

#model setup 2 factor 
cf1<- 'pf =~ fatalism_1 + fatalism_2 + fatalism_3 + fatalism_4 + fatalism_5 + 
             fatalism_6 + fatalism_7 + fatalism_8 +
             fatalism_9 + fatalism_10 + fatalism_11 + fatalism_12 +
             fatalism_13
       fatalism_3 ~~  fatalism_6      # similar wording
      '
#confirmatory factor analysis for 1 factor model
fit.cf1<- cfa(cf1, estimator = "MLR", data2)
summary(fit.cf1, fit.measures=TRUE, standardized=TRUE)

#likelihood-ratio test
lavTestLRT(fit.cf2, fit.cf1, method = "satorra.bentler.2010")

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
# fix numbers
names(data2)[names(data2) == "idealpoint_17"] <- "idealpoint_16"
names(data2)[names(data2) == "idealpoint_18"] <- "idealpoint_17"
names(data2)[names(data2) == "idealpoint_19"] <- "idealpoint_18"
names(data2)[names(data2) == "idealpoint_20"] <- "idealpoint_19"
# recode
data2$idealpoint_1<-car::recode(data2$idealpoint_1, "1=1; NA=0")
data2$idealpoint_2<-car::recode(data2$idealpoint_2, "1=1; NA=0")
data2$idealpoint_3<-car::recode(data2$idealpoint_3, "1=1; NA=0")
data2$idealpoint_4<-car::recode(data2$idealpoint_4, "1=1; NA=0")
data2$idealpoint_5<-car::recode(data2$idealpoint_5, "1=1; NA=0")
data2$idealpoint_6<-car::recode(data2$idealpoint_6, "1=1; NA=0")
data2$idealpoint_7<-car::recode(data2$idealpoint_7, "1=1; NA=0")
data2$idealpoint_8<-car::recode(data2$idealpoint_8, "1=1; NA=0")
data2$idealpoint_9<-car::recode(data2$idealpoint_9, "1=1; NA=0")
data2$idealpoint_10<-car::recode(data2$idealpoint_10, "1=1; NA=0")
data2$idealpoint_11<-car::recode(data2$idealpoint_11, "1=1; NA=0")
data2$idealpoint_12<-car::recode(data2$idealpoint_12, "1=1; NA=0")
data2$idealpoint_13<-car::recode(data2$idealpoint_13, "1=1; NA=0")
data2$idealpoint_14<-car::recode(data2$idealpoint_14, "1=1; NA=0")
data2$idealpoint_15<-car::recode(data2$idealpoint_15, "1=1; NA=0")
data2$idealpoint_16<-car::recode(data2$idealpoint_16, "1=1; NA=0")
data2$idealpoint_17<-car::recode(data2$idealpoint_17, "1=1; NA=0")
data2$idealpoint_18<-car::recode(data2$idealpoint_18, "1=1; NA=0")
data2$idealpoint_19<-car::recode(data2$idealpoint_19, "1=1; NA=0")

# composite ideal-point scale
data2$ipsc<-rowMeans(with(data2, cbind(idealpoint_1, 
                                       idealpoint_2, 
                                       idealpoint_3,
                                       idealpoint_4,
                                       idealpoint_5,
                                       idealpoint_6,
                                       idealpoint_7,
                                       idealpoint_8,
                                       idealpoint_9,
                                       idealpoint_10,
                                       idealpoint_11,
                                       idealpoint_12,
                                       idealpoint_13,
                                       idealpoint_14,
                                       idealpoint_15,
                                       idealpoint_16,
                                       idealpoint_17,
                                       idealpoint_18,
                                       idealpoint_19)))

# code ideal point items -- use this format
# there is no realpoint_20 (only 19 items)
data2$realpoint_1<-car::recode(data2$realpoint_1, "1=1; NA=0")
data2$realpoint_2<-car::recode(data2$realpoint_2, "1=1; NA=0")
data2$realpoint_3<-car::recode(data2$realpoint_3, "1=1; NA=0")
data2$realpoint_4<-car::recode(data2$realpoint_4, "1=1; NA=0")
data2$realpoint_5<-car::recode(data2$realpoint_5, "1=1; NA=0")
data2$realpoint_6<-car::recode(data2$realpoint_6, "1=1; NA=0")
data2$realpoint_7<-car::recode(data2$realpoint_7, "1=1; NA=0")
data2$realpoint_8<-car::recode(data2$realpoint_8, "1=1; NA=0")
data2$realpoint_9<-car::recode(data2$realpoint_9, "1=1; NA=0")
data2$realpoint_10<-car::recode(data2$realpoint_10, "1=1; NA=0")
data2$realpoint_11<-car::recode(data2$realpoint_11, "1=1; NA=0")
data2$realpoint_12<-car::recode(data2$realpoint_12, "1=1; NA=0")
data2$realpoint_13<-car::recode(data2$realpoint_13, "1=1; NA=0")
data2$realpoint_14<-car::recode(data2$realpoint_14, "1=1; NA=0")
data2$realpoint_15<-car::recode(data2$realpoint_15, "1=1; NA=0")
data2$realpoint_16<-car::recode(data2$realpoint_16, "1=1; NA=0")
data2$realpoint_17<-car::recode(data2$realpoint_17, "1=1; NA=0")
data2$realpoint_18<-car::recode(data2$realpoint_18, "1=1; NA=0")
data2$realpoint_19<-car::recode(data2$realpoint_19, "1=1; NA=0")

# composite real-point scale
data2$rpsc<-rowMeans(with(data2, cbind(realpoint_1, 
                                       realpoint_2, 
                                       realpoint_3,
                                       realpoint_4,
                                       realpoint_5,
                                       realpoint_6,
                                       realpoint_7,
                                       realpoint_8,
                                       realpoint_9,
                                       realpoint_10,
                                       realpoint_11,
                                       realpoint_12,
                                       realpoint_13,
                                       realpoint_14,
                                       realpoint_15,
                                       realpoint_16,
                                       realpoint_17,
                                       realpoint_18,
                                       realpoint_19)))

# raw difference between ideal and real (ideal-real)
data2$irdif<-data2$ipsc-data2$rpsc

### Jane: IRT
ideal.item <- data2[,grepl("idealpoint_",names(data2)) & names(data2)!="zip"]
ideal.item$respondent <- c(1:nrow(data2))
real.item <- data2[,grepl("realpoint_",names(data2))]
real.item$respondent <- c(1:nrow(data2))

library(reshape2)
st.ideal <- melt(ideal.item,id.vars="respondent") 
names(st.ideal) <- c("respondent","item","value")
st.real <- melt(real.item,id.vars="respondent")
names(st.real) <- c("respondent","item","value")

N <- nrow(st.ideal)  # number of observations in the model 
J <- length(unique(st.ideal$item)) # number of observable indicators
K <- length(unique(st.ideal$respondent)) # number of people
itemid <- as.numeric(as.factor(st.ideal$item)) # unique indicator id
y.real <- st.real$value # observable behaviors
y.ideal <- st.ideal$value # observable behaviors
id <- st.ideal$respondent


irt.ord.model <- function(){
  for (i in 1:N) { # observations
    
    
    y.real[i] ~ dbern(eta.real[i])
    logit(eta.real[i]) <- alpha.real[itemid[i]] * (x.real[id[i]] - beta.real[itemid[i]])
    # }
    y.ideal[i] ~ dbern(eta.ideal[i])
    logit(eta.ideal[i]) <- alpha.ideal[itemid[i]] * (x.ideal[id[i]] - beta.ideal[itemid[i]])
    
    
  }
  
  for(k in 1:K){
    
    x.ideal[k] ~ dnorm(0,0.01);T(-3,3)  # latent variable
    x.real[k] ~ dnorm(0,0.01);T(-3,3)  # latent variable
    
    
    x.distance[k] <- x.ideal[k]-x.real[k]
  }
  
  
  for (j in 1:J) { # items
    
    beta.real[j] ~ dnorm(0,5) # difficulty
    alpha.real[j] ~ dnorm(0,5);T(0,) # discrimination
    beta.ideal[j] ~ dnorm(0,5) # difficulty
    alpha.ideal[j] ~ dnorm(0,5);T(0,) # discrimination
    
  }     
  
}


inits <-  function(){ list("x.real"=runif(K,-3,3),
                           "beta.real"=runif(J,-1,1),
                           "alpha.real"=runif(J,.001,1),
                           "x.ideal"=runif(K,-3,3),
                           "beta.ideal"=runif(J,-1,1),
                           "alpha.ideal"=runif(J,.001,1)
)}

jags_data <- list(y.real=y.real,y.ideal=y.ideal,id=id,itemid=itemid,N=N,J=J,K=K)


total.iter <- 10000

jags.mod <- jags(model.file = irt.ord.model,
                 data=names(jags_data),
                 n.chains=5,
                 inits=inits,
                 n.iter=total.iter,
                 # n.burnin=total.iter*.95,
                 parameters.to.save=c("beta.real","alpha.real","x.real",
                                      "beta.ideal","alpha.ideal","x.ideal",
                                      "x.distance"))

# Checking posteriors
sum(jags.mod$BUGSoutput$summary[,"Rhat"]<=1.01)/length(jags.mod$BUGSoutput$summary[,"Rhat"])*100
hist(jags.mod$BUGSoutput$summary[,"Rhat"])

# Extracting results and merging with data
out <- jags.mod$BUGSoutput


### individual-level
results.ind <- data.frame(id=unique(st.real$respondent))
results.ind$x.ideal <- apply(out$sims.list$x.ideal,2,median)
results.ind$x.ideal025 <- apply(out$sims.list$x.ideal,2,quantile,.025)
results.ind$x.ideal975 <- apply(out$sims.list$x.ideal,2,quantile,.975)

results.ind$x.real <- apply(out$sims.list$x.real,2,median)
results.ind$x.real025 <- apply(out$sims.list$x.real,2,quantile,.025)
results.ind$x.real975 <- apply(out$sims.list$x.real,2,quantile,.975)

results.ind$x.distance <- apply(out$sims.list$x.distance,2,median)
results.ind$x.distance025 <- apply(out$sims.list$x.distance,2,quantile,.025)
results.ind$x.distance975 <- apply(out$sims.list$x.distance,2,quantile,.975)

corr.test(with(results.ind, cbind(x.real, x.ideal)))

ggplot(results.ind,aes(x=x.real,y=x.ideal)) +
  geom_point() +
  labs(x="Latent real",y="Latent ideal")

ggplot(results.ind,aes(x=x.ideal)) +
  geom_histogram()

ggplot(results.ind,aes(x=x.real)) +
  geom_histogram()

ggplot(results.ind,aes(x=x.distance)) +
  geom_histogram()  +
  labs(x="ideal - real",
       title="Participation Gap")

results.sort <- results.ind[order(results.ind$x.distance),]
results.sort$order <- order(results.sort$x.distance)
results.sort$negative <- ifelse(results.sort$x.distance975<0,1,0)

ggplot(results.sort,aes(x=x.distance,y=order)) +
  geom_point() +
  geom_ribbon(aes(xmin=x.distance025,xmax=x.distance975))

# r between naive real and IRT real = 0.94
# r between naive ideal and IRT ideal = 0.95
# r between naive distance and IRT distance = 0.87

corr.test(with(data2, cbind(x.real, rpsc, x.ideal, ipsc, x.distance, irdif)))

### item-level
results.item <- data.frame(id=unique(st.real$item))

results.item$alpha.ideal <- apply(out$sims.list$alpha.ideal,2,median)
results.item$alpha.ideal025 <- apply(out$sims.list$alpha.ideal,2,quantile,.025)
results.item$alpha.ideal975 <- apply(out$sims.list$alpha.ideal,2,quantile,.975)

results.item$beta.ideal <- apply(out$sims.list$beta.ideal,2,median)
results.item$beta.ideal025 <- apply(out$sims.list$beta.ideal,2,quantile,.025)
results.item$beta.ideal975 <- apply(out$sims.list$beta.ideal,2,quantile,.975)


results.item$alpha.real <- apply(out$sims.list$alpha.real,2,median)
results.item$alpha.real025 <- apply(out$sims.list$alpha.real,2,quantile,.025)
results.item$alpha.real975 <- apply(out$sims.list$alpha.real,2,quantile,.975)

results.item$beta.real <- apply(out$sims.list$beta.real,2,median)
results.item$beta.real025 <- apply(out$sims.list$beta.real,2,quantile,.025)
results.item$beta.real975 <- apply(out$sims.list$beta.real,2,quantile,.975)



### merge into data
data2 <- data.frame(data2,results.ind) # check that this is right (it should be)

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
data2$interestrc <- std01(5-data2$interest)

psych::alpha(with(data2, cbind(interestrc, ic1, ic2)))

data2$pi <- std01(data2$interestrc + data2$ic1 + data2$ic2) 

################################################################################

##### save coded dataframe as Rdata
save(data2, file="fatalism.Rdata")

################################################################################
model0a <- lm(fatalism ~ x.distance, data=data2 )
summary(model0a)
model0b <- lm(fatalism ~ x.ideal + x.real, data=data2 )
summary(model0b)
model0c <- lm(fatalism ~ x.distance + x.ideal + x.real, data=data2 )
summary(model0c)
model0d <- lm(fatalism ~ x.distance + x.ideal + x.real + time + money + space + energy + possc + 
               negsc + laddernew + gender.0 + party_id + racewhite, data=data2 )
summary(model0d)

################################################################################
### pre-registered models with FULL fatalism scale (fshort)
### removed emotion scales b/c not pre-reg

model0a <- lm_robust(fatalism ~ x.distance, data=data2, se_type = "HC3")
summary(model0a)
model0b <- lm_robust(fatalism ~ x.ideal + x.real, data=data2, se_type = "HC3" )
summary(model0b)

# resources, composite
model0c <- lm_robust(fatalism ~ x.distance + ressc + 
                       laddernew + gender.0 + party_id + racewhite, data=data2, 
                     se_type = "HC3" )
summary(model0c)
model0d <- lm_robust(fatalism ~ x.ideal + x.real + ressc +
                       laddernew + gender.0 + party_id + racewhite, data=data2, 
                     se_type = "HC3" )
summary(model0d)

# resources, individual items
model0e <- lm_robust(fatalism ~ x.distance + time + money + space + energy + 
                       laddernew + gender.0 + party_id + racewhite, data=data2, 
                     se_type = "HC3")
summary(model0e)
model0f <- lm_robust(fatalism ~ x.ideal + x.real + time + money + space + energy + 
                       laddernew + gender.0 + party_id + racewhite, data=data2,
                     se_type = "HC3" )
summary(model0f)

################################################################################
### pre-registered models with reduced fatalism scale (fshort)
### removed emotion scales b/c not pre-reg

model0a <- lm_robust(fshort ~ x.distance, data=data2, se_type = "HC3")
summary(model0a)
model0b <- lm_robust(fshort ~ x.ideal + x.real, data=data2, se_type = "HC3" )
summary(model0b)

# resources, composite
model0c <- lm_robust(fshort ~ x.distance + ressc + 
                laddernew + gender.0 + party_id + racewhite, data=data2, 
                se_type = "HC3" )
summary(model0c)
model0d <- lm_robust(fshort ~ x.ideal + x.real + ressc +
                laddernew + gender.0 + party_id + racewhite, data=data2, 
                se_type = "HC3" )
summary(model0d)

# resources, individual items
model0e <- lm_robust(fshort ~ x.distance + time + money + space + energy + 
                laddernew + gender.0 + party_id + racewhite, data=data2, 
                se_type = "HC3")
summary(model0e)
model0f <- lm_robust(fshort ~ x.ideal + x.real + time + money + space + energy + 
                laddernew + gender.0 + party_id + racewhite, data=data2,
                se_type = "HC3" )
summary(model0f)

################################################################################

# reversed, for exploratory purposes (no pre-registered)
model0g <- lm(x.distance ~ fshort + ressc + 
                laddernew + gender.0 + party_id + racewhite, data=data2 )
summary(model0g)
model0h <- lm(x.distance ~ fshort + ressc + exeff + ineff +
                laddernew + gender.0 + party_id + racewhite, data=data2 )
summary(model0h)
model0i <- lm(x.distance ~ fshort + ressc + exeff + ineff + pk + interestrc +
                laddernew + gender.0 + party_id + racewhite, data=data2 )
summary(model0i)

################################################################################
## Preliminary Findings ## 
# 1. Regress fatalism on efficacy scales 
# fatalism ~ internal efficacy 

model1 <- lm(fatalism ~ ineff + exeff, data=data2)
summary(model1)

# checking for collinearity 
vif(model1)

model2 <- lm(fatalism ~ ineff + exeff + time + money + space + energy + possc + 
               negsc + laddernew + gender.0 + party_id + racewhite, data=data2 )
summary(model2)
vif(model2)

# regressing resource constraint on efficacy scales # 
model3 <- lm(ineff ~ time + money + space + energy, data=data2)
summary(model3)

model4 <- lm(exeff ~ time + money + space + energy, data=data2)
summary(model4)

# OLS result on fatalism without resource constraint
model5 <- lm(fatalism ~ ineff + exeff + negemo + laddernew + gender.0 + party_id + racewhite, data=data2 )
summary(model5)
