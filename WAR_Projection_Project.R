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
library(Hmisc)
library(GGally)
library(VGAM)
library(party)
library(png)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(broom)
library(tidyr)
#library(misaem)
#library(AICcmodavg)
#library(zoib)
#library(rjags)
#library(data.table)


# Data Import and Imputing Missing Values
Appearences <- read.csv("Baseball Job Stuff/Orioles Analyst Project/Appearances.csv")
Batting <- read.csv("Baseball Job Stuff/Orioles Analyst Project/Batting.csv")
People <- read.csv("Baseball Job Stuff/Orioles Analyst Project/People.csv")
Lahman <- read.csv("Baseball Job Stuff/Orioles Analyst Project/Lahman.csv")

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
Batting$X1B <- (H-X2B - X3B - HR) #Calculating singles, needed for SLG


#Batting= aggregate(Batting[c('G','AB', 'R', 'H', 'X2B', 'X3B', 'HR', 'RBI', 'SB', 'CS', 'BB', 'SO', 'IBB', 'HBP', 'SH', 'SF', 
                     #'GIDP', 'PA', 'X1B')],
                #by = list(Batting$playerID, Batting$yearID, Batting$lgID),
                #FUN = sum)
attach(Batting)
Batting$OBP <- (H + BB + IBB + HBP)/PA #Calculating OBP
Batting$OBP[is.na(Batting$OBP)] <- 0 #Transmuting NAs into 0s for OBP
Batting$SLG <- (X1B + X2B*2 + X3B*3 + HR*4)/AB #Calculating SLG%
Batting$SLG[is.na(Batting$SLG)] <- 0 #Transmuting NAs into 0s for SLG

attach(Batting)
Batting$OPS <- OBP + SLG #Calculating OPS

####First Approach tried for creating league average OPS totals

#Total1994 <- data.frame()   

#Total1994$AB <-sum(Batting[Batting$yearID == "1994",]$AB, na.rm=TRUE)


#Isolating statistics for each year in order to calculate league average OPS for each year

#colnames(Batting)[1] <- "playerID"
#colnames(Batting)[2] <- "yearID"
#colnames(Batting)[3] <- "lgID"

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


##Merging data frames into one usable data frame.SQL procedures performed in R.

df = left_join(Batting, Appearences, by = c("playerID" = "playerID", "yearID" = "yearID", "teamID" = "teamID"))

df = left_join(df, Lahman, by = c("playerID" = "lahman_id", "yearID" = "ï..year")) #Includes records without corresponding Lahman data

df = df = left_join(df, People, by = c("playerID" = "playerID")) #Includes records without corresponding Lahman data


#Calculating RAR, WAR, WAR/PA, and creating blank column for Projected Plate Appearences
df$RAR = df$DRAA + df$RAA + 20
df$WAR = df$RAR/10

df<- transform(df, WAR_PA = WAR/PA)

#df$ProPA <- NA

Rookies <- filter(df, FirstYear == 1) #For use in projecting rookies

summary(Rookies$WAR_PA) #average 1.86

###Subtract 0.5 for non-top 100 prospects, Add 0.5 for top 100, 1 for top 50, 1.5 for top 10 prospects

##Filtering out unwanted entries

df$EffectiveYear = df$yearID #Creating an empty column
#Filtering so that players born in July or later are effectively one year younger in baseball terms
df$EffectiveYear = ifelse(df$birthYear >6, df$EffectiveYear-1, df$EffectiveYear) 
#Using the effective year to create an Age column
df$Age = df$EffectiveYear-df$birthYear
#Checking to make sure Age looks correct
summary(df$Age)
#Filtering out players older than 30
df <- dplyr::filter(df, Age <31)
#Getting rid of players with no plate appearances
df <-  dplyr::filter(df, PA >10)
#Getting rid of pitchers (allowing for a couple of position player pitching appearances)
df <- dplyr::filter(df, G_p <4 & G_all > G_p)

#Order by PlayerID, yearID
df<- df[with(df, order(playerID, yearID)), ]
#Determining whether there is a new player (first year?) 
df<- transform(df, FirstYear = c(NA, ifelse(diff(yearID) < 0, 1, 0)))

df<- df %>% distinct(playerID, yearID, .keep_all = TRUE)


####### Final Data Formatting - I am splitting the data into player seasons by age

All18 <- df %>% 
  filter(between(Age, 18, 18))
All19 <- df %>% 
  filter(between(Age, 19, 19))
All20 <- df %>% 
  filter(between(Age, 20, 20))
All21 <- df %>% 
  filter(between(Age, 21,21))
All22 <- df %>% 
  filter(between(Age, 22,22))
All23 <- df %>% 
  filter(between(Age, 23,23))
All24 <- df %>% 
  filter(between(Age, 24,24))
All25 <- df %>% 
  filter(between(Age, 25,25))
All26 <- df %>% 
  filter(between(Age, 26,26))
All27 <- df %>% 
  filter(between(Age, 27,27))
All28 <- df %>% 
  filter(between(Age, 28,28))
All29 <- df %>% 
  filter(between(Age, 29,29))
All30 <- df %>% 
  filter(between(Age, 30,30))

######## Adding suffixes in order to distinguish the data by season that I have split up for when I rejoin them.
colnames(All18) <- paste(colnames(df),"18",sep="_")
colnames(All19) <- paste(colnames(df),"19",sep="_")
colnames(All20) <- paste(colnames(df),"20",sep="_")
colnames(All21) <- paste(colnames(df),"21",sep="_")
colnames(All22) <- paste(colnames(df),"22",sep="_")
colnames(All23) <- paste(colnames(df),"23",sep="_")
colnames(All24) <- paste(colnames(df),"24",sep="_")
colnames(All25) <- paste(colnames(df),"25",sep="_")
colnames(All26) <- paste(colnames(df),"26",sep="_")
colnames(All27) <- paste(colnames(df),"27",sep="_")
colnames(All28) <- paste(colnames(df),"28",sep="_")
colnames(All29) <- paste(colnames(df),"29",sep="_")
colnames(All30) <- paste(colnames(df),"30",sep="_")


#Making playerID columns universal for joining
colnames(All18)[1] <- "playerID"
colnames(All19)[1] <- "playerID"
colnames(All20)[1] <- "playerID"
colnames(All21)[1] <- "playerID"
colnames(All22)[1] <- "playerID"
colnames(All23)[1] <- "playerID"
colnames(All24)[1] <- "playerID"
colnames(All25)[1] <- "playerID"
colnames(All26)[1] <- "playerID"
colnames(All27)[1] <- "playerID"
colnames(All28)[1] <- "playerID"
colnames(All29)[1] <- "playerID"
colnames(All30)[1] <- "playerID"


########## Now I am recombining the data into our final full dataset, joining by playerID

setAge = full_join(All18, All19, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All20, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All21, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All22, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All23, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All24, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All25, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All26, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All27, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All28, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All29, by = c("playerID" = "playerID"))
setAge = full_join(setAge, All30, by = c("playerID" = "playerID"))


setAge <- setAge[order(setAge$playerID),] #ordering by playerID
setAge[is.na(setAge)] = 0 #Setting NAs to 0s. Not ideal but in this case every row will have missing values by design and
#imputing the mean or median would heavily bias the model toward these centers.

#Prospect ranks being 0 or NA for these players would be misleading, so I am assigning all non-prospects a value of 150
setAge$prospect_rank_18[setAge$prospect_rank_18 == 0] <- 150
setAge$prospect_rank_19[setAge$prospect_rank_19 == 0] <- 150
setAge$prospect_rank_20[setAge$prospect_rank_20 == 0] <- 150
setAge$prospect_rank_21[setAge$prospect_rank_21 == 0] <- 150
setAge$prospect_rank_22[setAge$prospect_rank_22 == 0] <- 150
setAge$prospect_rank_23[setAge$prospect_rank_23 == 0] <- 150
setAge$prospect_rank_24[setAge$prospect_rank_24 == 0] <- 150
setAge$prospect_rank_25[setAge$prospect_rank_25 == 0] <- 150
setAge$prospect_rank_26[setAge$prospect_rank_26 == 0] <- 150
setAge$prospect_rank_27[setAge$prospect_rank_27 == 0] <- 150
setAge$prospect_rank_28[setAge$prospect_rank_28 == 0] <- 150
setAge$prospect_rank_29[setAge$prospect_rank_29 == 0] <- 150
setAge$prospect_rank_30[setAge$prospect_rank_30 == 0] <- 150


###Alternate functional approach to what is done above

#df.Rep <- function(.data_Frame, .search_Columns, .search_Value, .sub_Value){
  #.data_Frame[, .search_Columns] <- ifelse(.data_Frame[, .search_Columns]==.search_Value,.sub_Value/.search_Value,1) * .data_Frame[, .search_Columns]
  #return(.data_Frame)
#}

#df.Rep(setAge, c("prospect_rank_18", "prospect_rank_19", "prospect_rank_20", "prospect_rank_21", "prospect_rank_22",
               #"prospect_rank_23", "prospect_rank_24", "prospect_rank_25", "prospect_rank_26", "prospect_rank_27",
               #"prospect_rank_28", 
               #"prospect_rank_29", "prospect_rank_30"), 0, 150)


###Creating total WAR field
setAge <- setAge %>%
  mutate(totalWAR = rowSums(dplyr::select(.,WAR_18, WAR_19, WAR_20, WAR_21, WAR_22, WAR_23, 
                                          WAR_24, WAR_25, WAR_26, WAR_27, WAR_28, WAR_29, WAR_30), na.rm = TRUE))

###Train Test Splitting for full model
smp_size <- floor(0.75 * nrow(setAge))

## Setting the seed to make my partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(setAge)), size = smp_size)

train <- setAge[train_ind, ]
test <- setAge[-train_ind, ]


########## Modeling

#Model to project total WAR using all seasons of data.

TotalWARLM =lm(totalWAR ~OBP_19 + OBP_20 + OBP_21 + OBP_22 + OBP_23 + OBP_24
               + OBP_25 + OBP_26 + OBP_27 + OBP_28 + OBP_29 + OBP_30 + DRAA_22 + DRAA_30 + HR_20 + HR_21 + HR_22 + 
                 HR_23 + HR_24 + HR_25 + HR_26 + HR_27 + HR_28 + HR_29 + HR_30 + 
                 SO_20 + SO_22 + SO_23 + SO_24 + SO_25 + SO_26 + SO_27 + SO_29, data = train, na.action=na.omit)
summary(TotalWARLM)

tab_model(TotalWARLM) #Created to output regression results cleanly

test$PWAR <- predict(TotalWARLM, newdata = test, type = "response")
prediction <- test[c('playerID', 'totalWAR', 'PWAR')]
#Writing CSV file with predictions
write.csv(prediction, file = "totalWARResults.csv")



####Validating regression results
res <- TotalWARLM$residuals
plot(res, ylab="Residuals")

mse <- function(sm) 
  mean(sm$residuals^2)

#AIC, mse and BIC calculations

AIC(TotalWARLM)
mse(TotalWARLM)
BIC(TotalWARLM)


######Testing to see how many years we need to make good predictions - prospect rankings come majorly in handy here.

TotalWAR1824LM =lm(totalWAR ~OBP_20 + OBP_21 + OBP_22 + OBP_23 + OBP_24 + HR_19 
                   + HR_20 + HR_21 + HR_22 + HR_23 + HR_24 + prospect_rank_18 + prospect_rank_20 + 
                     SO_21 + SO_22 + SO_23 + SO_24, 
                   data = train, na.action=na.omit)

TotalWAR1824LMPA =lm(totalWAR ~OBP_20 + OBP_21 + OBP_22 + OBP_23 + OBP_24 + HR_19 
                   + HR_20 + HR_21 + HR_22 + HR_23 + HR_24 + prospect_rank_18 + prospect_rank_20 + 
                     SO_21 + SO_22 + SO_23 + SO_24 + PA_19 + PA_20 + PA_21 + PA_24, 
                   data = train, na.action=na.omit)

summary(TotalWAR1824LM)
summary(TotalWAR1824LMPA)

AIC(TotalWAR1824LM)
mse(TotalWAR1824LM)
BIC(TotalWAR1824LM)



###Wanted to test whether including PAs was the right decision, as it only led to a small increase in R^2 and Adjusted R^2
### at the cost of adding a lot more features. By AIC, which weighs the relative return of features, the PA model was better.
AIC(TotalWAR1824LMPA)
AIC(TotalWAR1824LM)
mse(TotalWAR1824LMPA)
mse(TotalWAR1824LM)
BIC(TotalWAR1824LMPA)
BIC(TotalWAR1824LM)


####Decision Tree - Wanted to see how a 18-24 tree model made decisions. Kept PA out to allow the tree visual 
#####to be more readable.

# Give the chart file a name.
png(file = "WAR1824_decision_tree.png")

# Create the tree.
output.tree <- ctree(
  totalWAR ~OBP_18 + OBP_19 + OBP_20 + OBP_21 + OBP_22 + OBP_23 + OBP_24 + HR_19 
  + HR_20 + HR_21 + HR_22 + HR_23 + HR_24 + prospect_rank_18 + prospect_rank_20,
  data = train)

# Plot the tree.
plot(output.tree)

# Save the file.
dev.off()

######### Individual Season projections - wanted to also look at projecting the player seasons by age for the purposes of 
########## making single season projections like we would want to in real time. 


WAR20LM =lm(WAR_20 ~ OBP_18 + OBP_19 + HR_19 + 
              prospect_rank_18 + prospect_rank_19 + prospect_rank_20 + DRAA_19 + PA_19, data = setAge, na.action=na.omit)

summary(WAR20LM)

WAR21LM =lm(WAR_21 ~ OBP_18 + OBP_19 + OBP_20 + HR_19 + HR_20 +prospect_rank_20 + prospect_rank_21
            + DRAA_19 + DRAA_20 + PA_19 + PA_20, 
            data = setAge, na.action=na.omit)

summary(WAR21LM)

WAR22LM =lm(WAR_22 ~ OBP_21 + HR_19 + HR_20 + HR_21 + prospect_rank_20 +
              prospect_rank_22 + 
              + DRAA_19 + DRAA_21 +SO_21 + PA_20 + PA_21, 
            data = setAge, na.action=na.omit)

summary(WAR22LM)

WAR23LM =lm(WAR_23 ~ OBP_22 + HR_21 + HR_22
              + DRAA_20 + DRAA_22 + prospect_rank_23 + PA_22, 
            data = setAge, na.action=na.omit)

summary(WAR23LM)

WAR24LM =lm(WAR_24 ~ OBP_21 + OBP_22 + OBP_23 + HR_21 + HR_22 + prospect_rank_22 +
              prospect_rank_24 +
              +DRAA_22 + SO_22 + SO_23 + SO_24, 
            data = setAge, na.action=na.omit)

summary(WAR24LM)

WAR25LM =lm(WAR_25 ~  OBP_24 + HR_21 + HR_22 + HR_23 + prospect_rank_23 +
              +prospect_rank_25 +
              + DRAA_22 + SO_22 + SO_23 + SO_24 + PA_23 + PA_24, 
            data = setAge, na.action=na.omit)

summary(WAR25LM)

WAR26LM =lm(WAR_26 ~ OBP_25 + HR_23 + HR_24 + HR_25
              + DRAA_25 + SO_23 + SO_24 + SO_25 + PA_24 + PA_25, 
            data = setAge, na.action=na.omit)

summary(WAR26LM)

WAR27LM =lm(WAR_27 ~ OBP_24 + OBP_25 + OBP_26 + HR_24 + HR_25 + HR_26 + SO_24 + SO_25 + SO_26 + PA_26, 
            data = setAge, na.action=na.omit)

summary(WAR27LM)

WAR28LM =lm(WAR_28 ~ OBP_25 + OBP_26 + OBP_27 + HR_25 + HR_26 + HR_27 + SO_25 + SO_26 +
              SO_27 + PA_27, 
            data = setAge, na.action=na.omit)

summary(WAR28LM)

WAR29LM =lm(WAR_29 ~ OBP_28 + HR_26 + HR_27 + HR_28 + SO_26 + SO_27 + SO_28 + PA_28, 
            data = setAge, na.action=na.omit)

summary(WAR29LM)

WAR30LM =lm(WAR_30 ~ OBP_29 + HR_27 + HR_28 + HR_29 + SO_27 + SO_29 + PA_29, 
            data = setAge, na.action=na.omit)

summary(WAR30LM)

#Making list of models 
ModelList <- list(WAR20LM, WAR21LM, WAR22LM, WAR23LM, WAR24LM, WAR25LM, WAR26LM, WAR27LM, WAR28LM, WAR29LM, WAR30LM)

#Model Validation
sapply(ModelList,AIC)
sapply(ModelList, mse)
sapply(ModelList, BIC)


ols_table <- ModelList %>%
  select(-statistic, -p.value) %>%
  mutate_each(funs(round(., 2)), -term) %>% 
  gather(key, value, estimate:std.error) %>%
  spread(model, value) 

ols_table

all_models <- rbind_list(
  WAR20LM %>% mutate(model = 1),
  WAR21LM %>% mutate(model = 2),
  WAR22LM %>% mutate(model = 3),
  WAR23LM %>% mutate(model = 4),
  WAR24LM %>% mutate(model = 5),
  WAR25LM %>% mutate(model = 6),
  WAR26LM %>% mutate(model = 7),
  WAR27LM %>% mutate(model = 8),
  WAR28LM %>% mutate(model = 9),
  WAR29LM %>% mutate(model = 10),
  WAR30LM %>% mutate(model = 11))

  
