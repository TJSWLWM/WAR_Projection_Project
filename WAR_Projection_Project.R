library(readr)
library(dplyr)
library(zoo)
library(psych)
library(ROCR)
library(corrplot)
library(InformationValue)
library(pbkrtest)
library(leaps)
library(MASS)
library(corrplot)
library(glm2)
library(aod)
library(caret)
library(magrittr)
library(forecast)


# Data Import and Imputing Missing Values
Appearences <- read.csv("C:/Users/tanne/OneDrive/Documents/Job Stuff/Orioles Analyst Project/Appearances.csv")
Batting <- read.csv("C:/Users/tanne/OneDrive/Documents/Job Stuff/Orioles Analyst Project/Batting.csv")
People <- read.csv("C:/Users/tanne/OneDrive/Documents/Job Stuff/Orioles Analyst Project/People.csv")
Lahman <- read.csv("C:/Users/tanne/OneDrive/Documents/Job Stuff/Orioles Analyst Project/Lahman.csv")

#Filtering Data Frames to get 1994 on
Batting <- filter(Batting, yearID > 1993)
Appearences <- filter(Appearences, yearID > 1993)

#Checking what they look like
summary(Batting)
summary(Appearences)

#Calculating Games Started at Each position
attach(Appearences)
Appearences$GS_p <- (G_p/G_all)*GS
Appearences$GS_c <- (G_c/G_all)*GS
Appearences$GS_1b <- (G_1b/G_all)*GS
Appearences$GS_2b <- (G_2b/G_all)*GS
Appearences$GS_3b <- (G_3b/G_all)*GS
Appearences$GS_ss <- (G_ss/G_all)*GS
Appearences$GS_lf <- (G_lf/G_all)*GS
Appearences$GS_rf <- (G_rf/G_all)*GS
Appearences$GS_cf <- (G_cf/G_all)*GS
Appearences$GS_of <- (G_of/G_all)*GS
Appearences$GS_dh <- (G_dh/G_all)*GS

#Creating vector with defensive positional values to use to calculate DRAA
DefensiveValues <- c(9/150, 7/150, 3/150, 2.5/150, 2/150, -7/150, -9.5/150, -15/150)

attach(Appearences)
#Calculating DRAA
Appearences$DRAA <- GS_c*DefensiveValues[1] + GS_ss*DefensiveValues[2] + GS_2b*DefensiveValues[3] + GS_cf*DefensiveValues[4] + 
  GS_3b*DefensiveValues[5] + GS_lf*DefensiveValues[6] + GS_rf*DefensiveValues[6] + GS_1b*DefensiveValues[7] + GS_dh*DefensiveValues[8]

#Calculating OPS - run this first chunk first then run the chunk below.
attach(Batting)
Batting$PA <- AB + BB + IBB + HBP + SH + SF #PAs may be off by very small increments ~1-2. Needed for OBP calculation
Batting$OBP <- (H + BB + IBB + HBP)/PA #Calculating OBP
Batting$OBP[is.na(Batting$OBP)] <- 0 #Transmuting NAs into 0s for OBP
Batting$X1B <- (H-X2B - X3B - HR) #Calculating singles, needed for SLG
Batting$SLG <- (X1B + X2B*2 + X3B*3 + HR*4)/AB #Calculating SLG%
Batting$SLG[is.na(Batting$SLG)] <- 0 #Transmuting NAs into 0s for SLG

attach(Batting)
Batting$OPS <- OBP + SLG #Calculating OPS

####First Approach tried for creating league average OPS totals

#Total1994 <- data.frame()   

#Total1994$AB <-sum(Batting[Batting$yearID == "1994",]$AB, na.rm=TRUE)


#Isolating statistics for each year in order to calculate league average OPS for each year 

#Using AL Batting Data to avoid pitchers
AL <- filter(Batting, lgID == "AL")

#Creating Data Frames to use to calculate league average OPS totals for each season.
Average1994 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 1994, 1994))
Average1995 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 1995, 1995))
Average1996 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 1996, 1996))
Average1997 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 1997, 1997))
Average1998 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 1998, 1998))
Average1999 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 1999, 1999))
Average2000 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2000, 2000))
Average2001 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2001, 2001))
Average2002 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2002, 2002))
Average2003 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2003, 2003))
Average2004 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2004, 2004))
Average2005 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2005, 2005))
Average2006 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2006, 2006))
Average2007 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2007, 2007))
Average2008 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2008, 2008))
Average2009 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2009, 2009))
Average2010 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2010, 2010))
Average2011 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2011, 2011))
Average2012 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2012, 2012))
Average2013 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2013, 2013))
Average2014 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2014, 2014))
Average2015 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2015, 2015))
Average2016 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2016, 2016))
Average2017 <- AL %>%
  dplyr::select(yearID, AB, BB, IBB, HBP, SH, SF, PA, H, X1B, X2B, X3B, HR) %>% 
  filter(between(yearID, 2017, 2017))



#Calculating League Average OPS for each year
attach(Average1994)
OPS1994 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average1995)
OPS1995 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average1996)
OPS1996 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average1997)
OPS1997 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average1998)
OPS1998 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average1999)
OPS1999 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2000)
OPS2000 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2001)
OPS2001 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2002)
OPS2002 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2003)
OPS2003 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2004)
OPS2004 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2005)
OPS2005 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2006)
OPS2006 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2007)
OPS2007 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2008)
OPS2008 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2009)
OPS2009 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2010)
OPS2010 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2011)
OPS2011 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2012)
OPS2012 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2013)
OPS2013 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2014)
OPS2014 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2015)
OPS2015 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2016)
OPS2016 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)
attach(Average2017)
OPS2017 <- (sum(H,BB,IBB,HBP)/sum(PA)) + sum(X1B, X2B*2,X3B*3,HR*4)/sum(AB)


#Assigning the League Average OPS as a column value to the overall Batting dataframe for use for RAA calculation
Batting$LeagueAvOPS <-0 #Initializing Column
Batting$LeagueAvOPS[c(1:1030)] <- OPS1994
Batting$LeagueAvOPS[c(1031:2283)] <- OPS1995
Batting$LeagueAvOPS[c(2284:3536)] <- OPS1996
Batting$LeagueAvOPS[c(3537:4772)] <- OPS1997
Batting$LeagueAvOPS[c(4773:6094)] <- OPS1998
Batting$LeagueAvOPS[c(6095:7393)] <- OPS1999
Batting$LeagueAvOPS[c(7394:8777)] <- OPS2000
Batting$LeagueAvOPS[c(8778:10116)] <- OPS2001
Batting$LeagueAvOPS[c(10117:11435)] <- OPS2002
Batting$LeagueAvOPS[c(11436:12782)] <- OPS2003
Batting$LeagueAvOPS[c(12783:14128)] <- OPS2004
Batting$LeagueAvOPS[c(14129:15458)] <- OPS2005
Batting$LeagueAvOPS[c(15459:16835)] <- OPS2006
Batting$LeagueAvOPS[c(16836:18220)] <- OPS2007
Batting$LeagueAvOPS[c(18221:19605)] <- OPS2008
Batting$LeagueAvOPS[c(19606:20993)] <- OPS2009
Batting$LeagueAvOPS[c(20994:22349)] <- OPS2010
Batting$LeagueAvOPS[c(22350:23738)] <- OPS2011
Batting$LeagueAvOPS[c(23739:25146)] <- OPS2012
Batting$LeagueAvOPS[c(25147:26555)] <- OPS2013
Batting$LeagueAvOPS[c(26556:27990)] <- OPS2014
Batting$LeagueAvOPS[c(27991:29476)] <- OPS2015
Batting$LeagueAvOPS[c(29477:30959)] <- OPS2016
Batting$LeagueAvOPS[c(30960:32453)] <- OPS2017

#Calculate Offensive RAA
attach(Batting)
Batting$RAA <- (PA * (OPS-LeagueAvOPS))/3.2135

#df = merge(x=Batting,y=Appearences,by="playerID")

##Merging data frames into one usable data frame. df includes records without corresponding Lahman data, df1 does not. SQL procedures performed in R.

df = left_join(Batting, Appearences, by = c("playerID" = "playerID", "yearID" = "yearID", "teamID" = "teamID"))

df = left_join(df, Lahman, by = c("playerID" = "lahman_id", "yearID" = "ï..year")) #Includes records without corresponding Lahman data

df1 = right_join(df, Lahman, by = c("playerID" = "lahman_id", "yearID" = "ï..year")) #All Matched

df = df = left_join(df, People, by = c("playerID" = "playerID")) #Includes records without corresponding Lahman data

df1 = left_join(df1, People, by = c("playerID" = "playerID")) #Includes only records without corresponding Lahman data. Will be used later in LDA analysis.

#Calculating RAR and WAR
df$RAR = df$DRAA + df$RAA + 20
df$WAR = df$RAR/10

df1$RAR = df1$DRAA + df1$RAA + 20
df1$WAR = df1$RAR/10

##Filtering out unwanted entries

df$EffectiveYear = df$yearID #Creating an empty column
#Filtering so that players born in July or later are effectively one year younger in baseball terms
df$EffectiveYear = ifelse(df$birthYear >6, df$EffectiveYear-1, df$EffectiveYear) 
#Using the effective year to create an Age column
df$Age = df$EffectiveYear-df$birthYear
#Checking to make sure Age looks correct
summary(df$Age)
#Filtering out players 30 or older
df <- dplyr::filter(df, Age <31)
#Getting rid of players with no plate appearances
df <-  dplyr::filter(df, PA >0)
#Getting rid of pitchers (allowing for a couple of position player )
df <- dplyr::filter(df, G_p <4 & G_all > G_p)


##Doing the same thing for df1 
df1$EffectiveYear = df1$yearID #Creating an empty column
#Filtering so that players born in July or later are effectively one year younger in baseball terms
df1$EffectiveYear = ifelse(df1$birthYear >6, df1$EffectiveYear-1, df1$EffectiveYear) 
#Using the effective year to create an Age column
df1$Age = df1$EffectiveYear-df1$birthYear
#Checking to make sure Age looks correct
summary(df1$Age)
#Filtering out players 30 or older
df1 <- dplyr::filter(df1, Age <31)
#Getting rid of players with no plate appearances
df1 <-  dplyr::filter(df1, PA >0)
#Getting rid of pitchers (allowing for a couple of position player )
df1 <- dplyr::filter(df1, G_p <4 & G_all > G_p)


#Order by PlayerID, yearID
df<- df[with(df, order(playerID, yearID)), ]
#Determining whether there is a new player (first year?) 
df<- transform(df, FirstYear = c(NA, ifelse(diff(yearID) < 0, 1, 0)))
#dflags$SecondYear <- 0
#dflags$SecondYear <- ifelse(diff(dflags$yearID, lag = 2) < 0, 1, 0)

#All Lags
#dflag1  %<>% mutate_all(funs(lag1 = lag(., 1), lag2 = lag(., 2), lag3 = lag(., 3)))
dflag1 <-df
#dflags <- df #Was experimenting with using up to 3 years of lags. 
#Creating Lag 1 variables
dflag1  %<>% mutate_all(funs(lag1 = lag(., 1)))
#dflags  %<>% mutate_all(funs(lag1 = lag(., 1), lag2 = lag(., 2), lag3 = lag(., 3)))
#Filtering out First Years (players we do not have previous data to project their current seasons)
dflag1 <- filter(dflag1, FirstYear == 0)
#Checking to make sure this is correctly done
summary(dflag1$FirstYear)

#Doing same thing for df1
df1<- df1[with(df1, order(playerID, yearID)), ]
#Determining whether there is a new player (first year?) 
df1<- transform(df1, FirstYear = c(NA, ifelse(diff(yearID) < 0, 1, 0)))
#df1<-transform(df1, SecondYear = c(NA, ifelse(diff(yearID, differences = 2) < 0, 1, 0)))
df1lag1 <-df1
#Creating Lag 1 variables
df1lag1  %<>% mutate_all(funs(lag1 = lag(., 1)))
#Filtering out First Years (players we do not have previous data to project their current seasons)
df1lag1 <- filter(df1lag1, FirstYear == 0)
#Checking to make sure this is correctly done
summary(df1lag1$FirstYear)


#### Modeling

#Dividing the data into training and test sets
smp_size <- floor(0.75 * nrow(dflag1))

set.seed(660)
dt <- sample(seq_len(nrow(dflag1)), size = smp_size)

train <- dflag1[dt, ]
test <- dflag1[-dt, ]

## First exploring non-performance metric impacts on WAR with Linear Regression
SimpleLM <- lm(formula = WAR ~ Age + height +weight + yearID + lgID.x, data = train)
summary(SimpleLM)
#Explains about 5% of variation in WAR, Older players in this subset are better, along with heavier and taller players,
#NL players were better on average than AL players by 0.1 WAR Bat and Throwing Hand are not statistically significant


####Now using lag 1 variables 
Model1Lag1 <- lm(formula = WAR ~weight + lgID.x + HR_lag1 + WAR_lag1 +OBP_lag1 + SLG_lag1 + OBP_lag1:SLG_lag1, data = train)
summary(Model1Lag1)

#With prospect rankings
#Creating data frame for only observations with prospect ranks 
dfNoNA <- df1lag1[!(is.na(df1lag1$prospect_rank.x)), ]
#Running Linear Model - Alternative Linear discriminant analysis attempted below, ran into issues because data frames were different sizes in dimensions
WithProspects <- Model1Lag1 <- lm(formula = WAR ~weight + lgID.x + HR_lag1 + WAR_lag1 +OBP_lag1 + SLG_lag1 + OBP_lag1:SLG_lag1, data = dfNoNA)
summary(WithProspects)



######Evaluating Model
#Creating mse function
mse <- function(sm) 
  mean(sm$residuals^2)

#AIC, mse and BIC calculations
AIC(Model1Lag1)
mse(Model1Lag1)
BIC(Model1Lag1)

#Bootstrapping
boot.fn=function(data,index)
  return(coef(lm(formula = WAR ~weight + lgID.x + HR_lag1 + WAR_lag1 +OBP_lag1 + SLG_lag1 + OBP_lag1:SLG_lag1, data = data)))

set.seed(1)
boot.fn(train,sample(500,500,replace=T))


#Outputting Predictions
test$P_WAR <- predict(Model1Lag1, newdata = test, type = "response")
prediction <- test[c("yearID", "playerID", "P_WAR", "WAR")]
#Writing CSV file with predictions
write.csv(prediction, file = "WAR_projections.csv")


######## Closing Thoughts

#The model that I chose to use was a 1-year lagged model in which the previous season of performance predicted the next season of WAR. I was able to 
#predict approximately 30% of the variation in WAR with league ID (NL players were slightly better on average), previous year HRs, WAR, OBP, SLG and
#an interaction term between OBP and SLG. I kept OBP in the model despite it failing to meet the threshold for statistical significance because 
#I included an interaction term with OBP so I wanted to include all of the components of the interaction as well. 

#I thought about #and looked into using ARIMA modeling for this assignment, but the format of our data made designing a clean time series a challenge.
#Additionally, I would want to include more lags in the future and combine those lags in one model. I also attempted to use LDA for distinguishing means of 
#values with prospect values versus those without, but ran into issues with using LDA on data grames with different numbers of variables. 
#My attempt at using LDA is commented below. 




#Linear discriminant analysis for comparing players with prospect rankings versus those that do not - Attempt

#attach(df1lag1)
#dfNoNA <- df1lag1[!(is.na(df1lag1$prospect_rank.x)), ]
#dfNAs <- df1lag1[(is.na(df1lag1$prospect_rank.x)), ]

#lda.fit=lda(WAR ~ weight + lgID.x + HR_lag1 + WAR_lag1 +OBP_lag1 + SLG_lag1 + OBP_lag1:SLG_lag1, data=df1lag1, subset = dfNoNA)
#lda.fit #Shows the Group means referenced below. 
#plot(lda.fit)
#lda.pred=predict(lda.fit, dfNAs)
#lda.class=lda.pred$class
#table(lda.class,dfNoNAs)




  