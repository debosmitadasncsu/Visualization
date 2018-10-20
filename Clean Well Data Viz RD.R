# Visualization Project - Well Data Cleaning
# Homework Team 2 Fall 2 

#-------------------------------------------Load/Install required libraries--------------------------------- 
library(data.table)
library(dplyr)
library(stringr)
library(zoo)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(haven)
library(caschrono)
library(TSA)
library(quantmod)
library(lubridate)
library(readxl)

#-----------------------------------------VISUALIZATION PROJECT WELL DATA-------------------------------------------
# Importing Datasets for all of the Wells including rain and tide data
# We will merge the rain tide and well depth files for each individual well 
# for each, then will select a specific time period for modeling and visualization purposes.
# series formula to see how well each WELL does on our "single" model. 

#---------------------------------------------------Well F-45-------------------------------------------------------
f45w = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/F-45.xlsx", 
                   sheet = "Well")
f45r = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/F-45.xlsx", 
                   sheet = "Rain")
f45t = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/F-45.xlsx", 
                   sheet = "Tide")

# Calculating monthly Well Data
f45w_sum <- f45w %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))# average by month

# Calculating monthly Tide Data
f45t_sum <- f45t %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Tide_ft))# average by month

# Rename Rain Variable 
setnames(f45r, old=("RAIN _FT"), new=("RAIN_FT"))#had to rename a bad variable
# Calculating monthly Rain Data
f45r_sum <- f45r %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))# average by month

# Creating a Time Series Sequence to merge to capture any missing values for Well 
wellDates45 <- data.frame(datetime=seq.POSIXt(from= min(f45w_sum$datetime),
                                              to = max(f45w_sum$datetime),
                                              by = "month"),x = 1)
# for Tide
tideDates45 <- data.frame(datetime=seq.POSIXt(from= min(f45w_sum$datetime),
                                              to = max(f45w_sum$datetime),
                                              by = "month"),x = 1)

# for Rain
rainDates45 <- data.frame(datetime=seq.POSIXt(from= min(f45w_sum$datetime),
                                              to = max(f45w_sum$datetime),
                                              by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf45 <- anti_join(wellDates45,f45w_sum , by = "datetime")
# Left join
testwelldf45 <- left_join(wellDates45, f45w_sum , by="datetime")

# Using na.approx for imputing missing values 
testwelldf45$wellheight <- na.approx(testwelldf45$wellheight)

# Capturing total missing for tide
nottesttidedf45 <- anti_join(tideDates45,f45t_sum , by = "datetime")
# Left join
testtidedf45 <- left_join(tideDates45, f45t_sum , by="datetime")

# Using na.approx for imputing missing values 
testtidedf45$tide <- na.approx(testtidedf45$tide)

# Capturing total missing for rain
nottestraindf45 <- anti_join(rainDates45,f45r_sum , by = "datetime")
# Left join
testraindf45 <- left_join(rainDates45, f45r_sum , by="datetime")

# Using na.approx for imputing missing values 
testraindf45$raininches <- na.approx(testraindf45$raininches)

# Dropping columns 
testwelldf45$x = NULL
testraindf45$x = NULL
testtidedf45$x = NULL

# Final dataset
f45_monthly = merge (testwelldf45, testraindf45,  by ="datetime")
f45_monthly = merge (f45_monthly, testtidedf45,  by ="datetime")

#---------------------------------------REPEAT SIMILAR STEPS TO OTHER WELLS-----------------------------------------

#--------------------------------------------------Well F-179---------------------------------------------------------
f179w = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/F-179.xlsx", 
                    sheet = "Well")
f179r = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/F-179.xlsx", 
                    sheet = "Rain")
f179t = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/F-179.xlsx", 
                    sheet = "Tide")

# Calculating to be on a monthly basis for Well Data
f179w_sum <- f179w %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
f179t_sum <- f179t %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Tide_ft))

# Calculating to be on a monthly basis for Well Data
f179r_sum <- f179r %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates179 <- data.frame(datetime=seq.POSIXt(from= min(f179w_sum$datetime),
                                               to = max(f179w_sum$datetime),
                                               by = "month"),x = 1)
# for Tide
tideDates179 <- data.frame(datetime=seq.POSIXt(from= min(f179w_sum$datetime),
                                               to = max(f179w_sum$datetime),
                                               by = "month"),x = 1)

# for Rain
rainDates179 <- data.frame(datetime=seq.POSIXt(from= min(f179w_sum$datetime),
                                               to = max(f179w_sum$datetime),
                                               by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf179 <- anti_join(wellDates179,f179w_sum , by = "datetime")
# Left join
testwelldf179 <- left_join(wellDates179, f179w_sum , by="datetime")

# Using na.approx for imputing missing values 
testwelldf179$wellheight <- na.approx(testwelldf179$wellheight) 

# Capturing total missing for tide
nottesttidedf179 <- anti_join(tideDates179,f179t_sum , by = "datetime")
# Left join
testtidedf179 <- left_join(tideDates179, f179t_sum , by="datetime")

# Using na.approx for imputing missing values 
testtidedf179$tide <- na.approx(testtidedf179$tide)

# Capturing total missing for rain
nottestraindf179 <- anti_join(rainDates179,f179r_sum , by = "datetime")
# Left join
testraindf179 <- left_join(rainDates179, f179r_sum , by="datetime")

# Using na.approx for imputing missing values 
testraindf179$raininches <- na.approx(testraindf179$raininches)

# Dropping Columns
testwelldf179$x = NULL
testraindf179$x = NULL
testtidedf179$x = NULL

f179_monthly = merge (testwelldf179, testraindf179,  by ="datetime")
f179_monthly = merge (f179_monthly, testtidedf179,  by ="datetime")

#-----------------------------------------------Well F-319---------------------------------------------------------
f319w = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/F-319.xlsx", 
                    sheet = "Well")
f319r = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/F-319.xlsx", 
                    sheet = "Rain")
f319t = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/F-319.xlsx", 
                    sheet = "Tide")
# Calculating to be on a monthly basis for Well Data
f319w_sum <- f319w %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
f319t_sum <- f319t %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Tide_ft))

# Calculating to be on a monthly basis for Well Data
f319r_sum <- f319r %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates319 <- data.frame(datetime=seq.POSIXt(from= min(f319w_sum$datetime),
                                               to = max(f319w_sum$datetime),
                                               by = "month"),x = 1)
# for Tide
tideDates319 <- data.frame(datetime=seq.POSIXt(from= min(f319w_sum$datetime),
                                               to = max(f319w_sum$datetime),
                                               by = "month"),x = 1)

# for Rain
rainDates319 <- data.frame(datetime=seq.POSIXt(from= min(f319w_sum$datetime),
                                               to = max(f319w_sum$datetime),
                                               by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf319 <- anti_join(wellDates319,f319w_sum , by = "datetime")
# Left join
testwelldf319 <- left_join(wellDates319, f319w_sum , by="datetime")

# Using na.approx for imputing missing values
testwelldf319$wellheight <- na.approx(testwelldf319$wellheight)  

# Capturing total missing for tide
nottesttidedf319 <- anti_join(tideDates319,f319t_sum , by = "datetime")
# Left join
testtidedf319 <- left_join(tideDates319, f319t_sum , by="datetime")

# Using na.approx for imputing missing values
testtidedf319$tide <- na.approx(testtidedf319$tide)  

# Capturing total missing for rain
nottestraindf319 <- anti_join(rainDates319,f319r_sum , by = "datetime")
# Left join
testraindf319 <- left_join(rainDates319, f319r_sum , by="datetime")

# Using na.approx for imputing missing values
testraindf319$raininches <- na.approx(testraindf319$raininches)  

# Dropping Columns
testwelldf319$x = NULL
testraindf319$x = NULL
testtidedf319$x = NULL

f319_monthly = merge (testwelldf319, testraindf319,  by ="datetime")
f319_monthly = merge (f319_monthly, testtidedf319,  by ="datetime")

#-----------------------------------------------Well G-561T-------------------------------------------------------------
g561_Tw = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-561_T.xlsx", 
                      sheet = "Well")
g561_Tr = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-561_T.xlsx", 
                      sheet = "Rain")
g561_Tt = read.csv("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/station_8722956.csv")

# Calculating to be on a monthly basis for Well Data
g561w_sum <- g561_Tw %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
g561t_sum <- g561_Tt %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Prediction))

# Calculating to be on a monthly basis for Well Data
g561r_sum <- g561_Tr %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates561 <- data.frame(datetime=seq.POSIXt(from= min(g561w_sum$datetime),
                                               to = max(g561w_sum$datetime),
                                               by = "month"),x = 1)
# for Tide
tideDates561 <- data.frame(datetime=seq.POSIXt(from= min(g561w_sum$datetime),
                                               to = max(g561w_sum$datetime),
                                               by = "month"),x = 1)

# for Rain
rainDates561 <- data.frame(datetime=seq.POSIXt(from= min(g561w_sum$datetime),
                                               to = max(g561w_sum$datetime),
                                               by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf561 <- anti_join(wellDates561,g561w_sum , by = "datetime")
# Left join
testwelldf561 <- left_join(wellDates561, g561w_sum , by="datetime")

# Using na.approx for imputing missing values
testwelldf561$wellheight <- na.approx(testwelldf561$wellheight)  

# Capturing total missing for tide
nottesttidedf561 <- anti_join(tideDates561,g561t_sum , by = "datetime")
# Left join
testtidedf561 <- left_join(tideDates561, g561t_sum , by="datetime")

# Using na.approx for imputing missing values
testtidedf561$tide <- na.approx(testtidedf561$tide)  

# Capturing total missing for rain
nottestraindf561 <- anti_join(rainDates561,g561r_sum , by = "datetime")
# Left join
testraindf561 <- left_join(rainDates561, g561r_sum , by="datetime")

# Using na.approx for imputing missing values
testraindf561$raininches <- na.approx(testraindf561$raininches)  

# Dropping Columns
testwelldf561$x = NULL
testraindf561$x = NULL
testtidedf561$x = NULL

g561t_monthly = merge (testwelldf561, testraindf561,  by ="datetime")
g561t_monthly = merge (g561t_monthly, testtidedf561,  by ="datetime")

#-----------------------------------------------Well G-580A----------------------------------------------------------
g580Aw = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-580A.xlsx", 
                     sheet = "Well")
g580Ar = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-580A.xlsx", 
                     sheet = "Rain")
g580At = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-580A.xlsx", 
                     sheet = "Tide")

# Calculating to be on a monthly basis for Well Data
g580w_sum <- g580Aw %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
g580t_sum <- g580At %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Tide_ft))

# Calculating to be on a monthly basis for Well Data
g580r_sum <- g580Ar %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates580 <- data.frame(datetime=seq.POSIXt(from= min(g580w_sum$datetime),
                                               to = max(g580w_sum$datetime),
                                               by = "month"),x = 1)
# for Tide
tideDates580 <- data.frame(datetime=seq.POSIXt(from= min(g580w_sum$datetime),
                                               to = max(g580w_sum$datetime),
                                               by = "month"),x = 1)

# for Rain
rainDates580 <- data.frame(datetime=seq.POSIXt(from= min(g580w_sum$datetime),
                                               to = max(g580w_sum$datetime),
                                               by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf580 <- anti_join(wellDates580,g580w_sum , by = "datetime")
# Left join
testwelldf580 <- left_join(wellDates580, g580w_sum , by="datetime")

# Using na.approx for imputing missing values
testwelldf580$wellheight <- na.approx(testwelldf580$wellheight)  

# Capturing total missing for tide
nottesttidedf580 <- anti_join(tideDates580,g580t_sum , by = "datetime")
# Left join
testtidedf580 <- left_join(tideDates580, g580t_sum , by="datetime")

# Using na.approx for imputing missing values
testtidedf580$tide <- na.approx(testtidedf580$tide)  

# Capturing total missing for rain
nottestraindf580 <- anti_join(rainDates580,g580r_sum , by = "datetime")
# Left join
testraindf580 <- left_join(rainDates580, g580r_sum , by="datetime")

# Using na.approx for imputing missing values
testraindf580$raininches <- na.approx(testraindf580$raininches)  

# Dropping Columns
testwelldf580$x = NULL
testraindf580$x = NULL
testtidedf580$x = NULL

g580a_monthly = merge (testwelldf580, testraindf580,  by ="datetime")
g580a_monthly = merge (g580a_monthly, testtidedf580,  by ="datetime")

#-----------------------------------------------Well G-852-----------------------------------------------------------
g852w = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-852.xlsx",
                   sheet = "Well")
g852r = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-852.xlsx",
                    sheet = "Rain")
g852t = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-580A.xlsx",
                    sheet = "Tide")

# Tdie data from Well G-852 has a lot of missing values, therefore we will just use the tide data from well G
# G-580A because they are close in proximity

# Calculating to be on a monthly basis for Well Data
 g852w_sum <- g852w %>% 
   filter(Code =="A") %>%
   mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
   group_by(datetime) %>%
   summarise(wellheight=mean(Corrected))
 
# Calculating to be on a monthly basis for Well Data
 g852t_sum <- g852t %>% 
   mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
   group_by(datetime) %>%
   summarise(tide=mean(Tide_ft))
 
# Calculating to be on a monthly basis for Well Data
 g852r_sum <- g852r %>% 
   mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
   group_by(datetime) %>%
   mutate(RAIN_FT = RAIN_FT*12) %>%
   summarise(raininches=sum(RAIN_FT))
 
# Creating a Time Series Sequence to merge to capture any missing values
# For Well
wellDates852 <- data.frame(datetime=seq.POSIXt(from= min(g852w_sum$datetime),
                                               to = max(g852w_sum$datetime),
                                                by = "month"),x = 1)
 # for Tide
 tideDates852 <- data.frame(datetime=seq.POSIXt(from= min(g852w_sum$datetime),
                                                to = max(g852w_sum$datetime),
                                                by = "month"),x = 1)
 
 # for Rain
 rainDates852 <- data.frame(datetime=seq.POSIXt(from= min(g852w_sum$datetime),
                                                to = max(g852w_sum$datetime),
                                                by = "month"),x = 1)
 
 # Capturing total missing for well
 nottestwelldf852 <- anti_join(wellDates852,g852w_sum , by = "datetime")
 # Left join
 testwelldf852 <- left_join(wellDates852, g852w_sum , by="datetime")
 
 # Approximately/ adding in missing values
 testwelldf852$wellheight <- na.approx(testwelldf852$wellheight) 
 
 # Capturing total missing for tide
 nottesttidedf852 <- anti_join(tideDates852, g852t_sum , by = "datetime")
 # Left join
 testtidedf852 <- left_join(tideDates852, g852t_sum , by="datetime")
 
 # Approximately/ adding in missing values
 testtidedf852$tide <- na.approx(testtidedf852$tide) 
 
 # Capturing total missing for rain
 nottestraindf852 <- anti_join(rainDates852,g852r_sum , by = "datetime")
 # Left join
 testraindf852 <- left_join(rainDates852, g852r_sum , by="datetime")
 
 # Approximately/ adding in missing values
 testraindf852$raininches <- na.approx(testraindf852$raininches) 
 
 # Dropping 1 columns
 testwelldf852$x = NULL
 testraindf852$x = NULL
 testtidedf852$x = NULL
 
g852_monthly = merge (testwelldf852, testraindf852,  by ="datetime")
g852_monthly = merge (g852_monthly, testtidedf852,  by ="datetime")

# Note that monthly data starts in 2005 for Well G-852

#-----------------------------------------------Well G-860----------------------------------------------------------
g860w = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-860.xlsx", 
                    sheet = "Well")
g860r = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-860.xlsx", 
                    sheet = "Rain")
g860t = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-860.xlsx", 
                    sheet = "Tide")

# Calculating to be on a monthly basis for Well Data
g860w_sum <- g860w %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
g860t_sum <- g860t %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Tide_ft))

# Calculating to be on a monthly basis for Well Data
g860r_sum <- g860r %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates860 <- data.frame(datetime=seq.POSIXt(from= min(g860w_sum$datetime),
                                               to = max(g860w_sum$datetime),
                                               by = "month"),x = 1)
# for Tide
tideDates860 <- data.frame(datetime=seq.POSIXt(from= min(g860w_sum$datetime),
                                               to = max(g860w_sum$datetime),
                                               by = "month"),x = 1)

# for Rain
rainDates860 <- data.frame(datetime=seq.POSIXt(from= min(g860w_sum$datetime),
                                               to = max(g860w_sum$datetime),
                                               by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf860 <- anti_join(wellDates860,g860w_sum , by = "datetime")
# Left join
testwelldf860 <- left_join(wellDates860, g860w_sum , by="datetime")

# Using na.approx for imputing missing values
testwelldf860$wellheight <- na.approx(testwelldf860$wellheight)  

# Capturing total missing for tide
nottesttidedf860 <- anti_join(tideDates860,g860t_sum , by = "datetime")
# Left join
testtidedf860 <- left_join(tideDates860, g860t_sum , by="datetime")

# Using na.approx for imputing missing values
testtidedf860$tide <- na.approx(testtidedf860$tide)  

# Capturing total missing for rain
nottestraindf860 <- anti_join(rainDates860,g860r_sum , by = "datetime")
# Left join
testraindf860 <- left_join(rainDates860, g860r_sum , by="datetime")

# Using na.approx for imputing missing values
testraindf860$raininches <- na.approx(testraindf860$raininches)  

# Dropping Columns
testwelldf860$x = NULL
testraindf860$x = NULL
testtidedf860$x = NULL

g860_monthly = merge (testwelldf860, testraindf860,  by ="datetime")
g860_monthly = merge (g860_monthly, testtidedf860,  by ="datetime")

#-----------------------------------------------Well G-1220T----------------------------------------------------------------
g1220_Tw = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-1220_T.xlsx",
                       sheet = "Well")
g1220_Tr = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-1220_T.xlsx",
                       sheet = "Rain")
g1220_Tt = read.csv("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/station_8722956.csv")

# Note, we are using tidal data that is close in proximity to this well 

# Calculating to be on a monthly basis for Well Data
g1220w_sum <- g1220_Tw %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
g1220t_sum <- g1220_Tt %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Prediction))

# Calculating to be on a monthly basis for Well Data
g1220r_sum <- g1220_Tr %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values
# For Well
wellDates1220 <- data.frame(datetime=seq.POSIXt(from= min(g1220w_sum$datetime),
                                                to = max(g1220w_sum$datetime),
                                                by = "month"),x = 1)
# for Tide
tideDates1220 <- data.frame(datetime=seq.POSIXt(from= min(g1220w_sum$datetime),
                                                to = max(g1220w_sum$datetime),
                                                by = "month"),x = 1)

# for Rain
rainDates1220 <- data.frame(datetime=seq.POSIXt(from= min(g1220w_sum$datetime),
                                                to = max(g1220w_sum$datetime),
                                                by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf1220 <- anti_join(wellDates1220,g1220w_sum , by = "datetime")
# Left join
testwelldf1220 <- left_join(wellDates1220, g1220w_sum , by="datetime")

# Approximately/ adding in missing values
testwelldf1220$wellheight <- na.approx(testwelldf1220$wellheight) 

# Capturing total missing for tide
nottesttidedf1220 <- anti_join(tideDates1220,g1220t_sum , by = "datetime")
# Left join
testtidedf1220 <- left_join(tideDates1220, g1220t_sum , by="datetime")

# Approximately/ adding in missing values
testtidedf1220$tide <- na.approx(testtidedf1220$tide) 

# Capturing total missing for rain
nottestraindf1220 <- anti_join(rainDates1220,g1220r_sum , by = "datetime")
# Left join
testraindf1220 <- left_join(rainDates1220, g1220r_sum , by="datetime")

# Approximately/ adding in missing values
testraindf1220$raininches <- na.approx(testraindf1220$raininches) 

# Dropping 1 columns
testwelldf1220$x = NULL
testraindf1220$x = NULL
testtidedf1220$x = NULL

g1220_monthly = merge (testwelldf1220, testraindf1220,  by ="datetime")
g1220_monthly = merge (g1220_monthly, testtidedf1220,  by ="datetime")
#View(monthg1220)

#-----------------------------------------------Well G-1260T-----------------------------------------------------------------
g1260_Tw = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-1260_T.xlsx", 
                       sheet = "Well")
g1260_Tr = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-1260_T.xlsx", 
                       sheet = "Rain")
g1260_Tt = read.csv("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/station_8722802.csv")

# Calculating to be on a monthly basis for Well Data
g1260w_sum <- g1260_Tw %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
g1260t_sum <- g1260_Tt %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Prediction))

# Calculating to be on a monthly basis for Well Data
g1260r_sum <- g1260_Tr %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates1260 <- data.frame(datetime=seq.POSIXt(from= min(g1260w_sum$datetime),
                                                to = max(g1260w_sum$datetime),
                                                by = "month"),x = 1)
# for Tide
tideDates1260 <- data.frame(datetime=seq.POSIXt(from= min(g1260w_sum$datetime),
                                                to = max(g1260w_sum$datetime),
                                                by = "month"),x = 1)

# for Rain
rainDates1260 <- data.frame(datetime=seq.POSIXt(from= min(g1260w_sum$datetime),
                                                to = max(g1260w_sum$datetime),
                                                by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf1260 <- anti_join(wellDates1260,g1260w_sum , by = "datetime")
# Left join
testwelldf1260 <- left_join(wellDates1260, g1260w_sum , by="datetime")

# Using na.approx for imputing missing values
testwelldf1260$wellheight <- na.approx(testwelldf1260$wellheight)  

# Capturing total missing for tide
nottesttidedf1260 <- anti_join(tideDates1260,g1260t_sum , by = "datetime")
# Left join
testtidedf1260 <- left_join(tideDates1260, g1260t_sum , by="datetime")

# Using na.approx for imputing missing values
testtidedf1260$tide <- na.approx(testtidedf1260$tide)  

# Capturing total missing for rain
nottestraindf1260 <- anti_join(rainDates1260,g1260r_sum , by = "datetime")
# Left join
testraindf1260 <- left_join(rainDates1260, g1260r_sum , by="datetime")

# Using na.approx for imputing missing values
testraindf1260$raininches <- na.approx(testraindf1260$raininches)  

# Dropping Columns
testwelldf1260$x = NULL
testraindf1260$x = NULL
testtidedf1260$x = NULL

g1260_monthly = merge (testwelldf1260, testraindf1260,  by ="datetime")
g1260_monthly = merge (g1260_monthly, testtidedf1260,  by ="datetime")

#-----------------------------------------------Well G-2147T----------------------------------------------------------------
g2147_Tw = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-2147_T.xlsx", 
                       sheet = "Well")
g2147_Tr = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-2147_T.xlsx", 
                       sheet = "Rain")
g2147_Tt = read.csv("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/station_8722859.csv")

# Calculating to be on a monthly basis for Well Data
g2147w_sum <- g2147_Tw %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
g2147t_sum <- g2147_Tt %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Prediction))

# Calculating to be on a monthly basis for Well Data
g2147r_sum <- g2147_Tr %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates2147 <- data.frame(datetime=seq.POSIXt(from= min(g2147w_sum$datetime),
                                                to = max(g2147w_sum$datetime),
                                                by = "month"),x = 1)
# for Tide
tideDates2147 <- data.frame(datetime=seq.POSIXt(from= min(g2147w_sum$datetime),
                                                to = max(g2147w_sum$datetime),
                                                by = "month"),x = 1)

# for Rain
rainDates2147 <- data.frame(datetime=seq.POSIXt(from= min(g2147w_sum$datetime),
                                                to = max(g2147w_sum$datetime),
                                                by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf2147 <- anti_join(wellDates2147,g2147w_sum , by = "datetime")
# Left join
testwelldf2147 <- left_join(wellDates2147, g2147w_sum , by="datetime")

# Using na.approx for imputing missing values
testwelldf2147$wellheight <- na.approx(testwelldf2147$wellheight)  

# Capturing total missing for tide
nottesttidedf2147 <- anti_join(tideDates2147,g2147t_sum , by = "datetime")
# Left join
testtidedf2147 <- left_join(tideDates2147, g2147t_sum , by="datetime")

# Using na.approx for imputing missing values
testtidedf2147$tide <- na.approx(testtidedf2147$tide)  

# Capturing total missing for rain
nottestraindf2147 <- anti_join(rainDates2147,g2147r_sum , by = "datetime")
# Left join
testraindf2147 <- left_join(rainDates2147, g2147r_sum , by="datetime")

# Using na.approx for imputing missing values
testraindf2147$raininches <- na.approx(testraindf2147$raininches)  

# Dropping Columns
testwelldf2147$x = NULL
testraindf2147$x = NULL
testtidedf2147$x = NULL

g2147_monthly = merge (testwelldf2147, testraindf2147,  by ="datetime")
g2147_monthly = merge (g2147_monthly, testtidedf2147,  by ="datetime")

#-----------------------------------------------Well G-2866T----------------------------------------------------------------
g2866_Tw = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-2866_T.xlsx", 
                       sheet = "Well")
g2866_Tr = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-2866_T.xlsx", 
                       sheet = "Rain")
g2866_Tt = read.csv("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/station_8722859.csv")

# Calculating to be on a monthly basis for Well Data
g2866w_sum <- g2866_Tw %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
g2866t_sum <- g2866_Tt %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Prediction))

# Calculating to be on a monthly basis for Well Data
g2866r_sum <- g2866_Tr %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates2866 <- data.frame(datetime=seq.POSIXt(from= min(g2866w_sum$datetime),
                                                to = max(g2866w_sum$datetime),
                                                by = "month"),x = 1)
# for Tide
tideDates2866 <- data.frame(datetime=seq.POSIXt(from= min(g2866w_sum$datetime),
                                                to = max(g2866w_sum$datetime),
                                                by = "month"),x = 1)

# for Rain
rainDates2866 <- data.frame(datetime=seq.POSIXt(from= min(g2866w_sum$datetime),
                                                to = max(g2866w_sum$datetime),
                                                by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf2866 <- anti_join(wellDates2866,g2866w_sum , by = "datetime")
# Left join
testwelldf2866 <- left_join(wellDates2866, g2866w_sum , by="datetime")

# Using na.approx for imputing missing values
testwelldf2866$wellheight <- na.approx(testwelldf2866$wellheight)  

# Capturing total missing for tide
nottesttidedf2866 <- anti_join(tideDates2866,g2866t_sum , by = "datetime")
# Left join
testtidedf2866 <- left_join(tideDates2866, g2866t_sum , by="datetime")

# Using na.approx for imputing missing values
testtidedf2866$tide <- na.approx(testtidedf2866$tide)  

# Capturing total missing for rain
nottestraindf2866 <- anti_join(rainDates2866,g2866r_sum , by = "datetime")
# Left join
testraindf2866 <- left_join(rainDates2866, g2866r_sum , by="datetime")

# Using na.approx for imputing missing values
testraindf2866$raininches <- na.approx(testraindf2866$raininches)  

# Dropping Columns
testwelldf2866$x = NULL
testraindf2866$x = NULL
testtidedf2866$x = NULL

g2866_monthly = merge (testwelldf2866, testraindf2866,  by ="datetime")
g2866_monthly  = merge (g2866_monthly , testtidedf2866,  by ="datetime")

#-----------------------------------------------Well G-3549T----------------------------------------------------------
g3549w = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-3549.xlsx", 
                     sheet = "Well")
g3549r = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-3549.xlsx", 
                     sheet = "Rain")
g3549t = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/G-3549.xlsx", 
                     sheet = "Tide")

# Calculating to be on a monthly basis for Well Data
g3549w_sum <- g3549w %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
g3549t_sum <- g3549t %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Tide_ft))

# Calculating to be on a monthly basis for Well Data
g3549r_sum <- g3549r %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))

# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates3549 <- data.frame(datetime=seq.POSIXt(from= min(g3549w_sum$datetime),
                                                to = max(g3549w_sum$datetime),
                                                by = "month"),x = 1)
# for Tide
tideDates3549 <- data.frame(datetime=seq.POSIXt(from= min(g3549w_sum$datetime),
                                                to = max(g3549w_sum$datetime),
                                                by = "month"),x = 1)

# for Rain
rainDates3549 <- data.frame(datetime=seq.POSIXt(from= min(g3549w_sum$datetime),
                                                to = max(g3549w_sum$datetime),
                                                by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf3549 <- anti_join(wellDates3549,g3549w_sum , by = "datetime")
# Left join
testwelldf3549 <- left_join(wellDates3549, g3549w_sum , by="datetime")

# Using na.approx for imputing missing values
testwelldf3549$wellheight <- na.approx(testwelldf3549$wellheight)  

# Capturing total missing for tide
nottesttidedf3549 <- anti_join(tideDates3549,g3549t_sum , by = "datetime")
# Left join
testtidedf3549 <- left_join(tideDates3549, g3549t_sum , by="datetime")

# Using na.approx for imputing missing values
testtidedf3549$tide <- na.approx(testtidedf3549$tide)  

# Capturing total missing for rain
nottestraindf3549 <- anti_join(rainDates3549,g3549r_sum , by = "datetime")
# Left join
testraindf3549 <- left_join(rainDates3549, g3549r_sum , by="datetime")

# Using na.approx for imputing missing values
testraindf3549$raininches <- na.approx(testraindf3549$raininches)  

# Dropping Columns
testwelldf3549$x = NULL
testraindf3549$x = NULL
testtidedf3549$x = NULL

g3549_monthly = merge (testwelldf3549, testraindf3549,  by ="datetime")
g3549_monthly = merge (g3549_monthly, testtidedf3549,  by ="datetime")

#-----------------------------------------------Well PB-1680T----------------------------------------------------------------
pb1680_Tw = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/PB-1680_T.xlsx", 
                        sheet = "Well")
pb1680_Tr = read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/PB-1680_T.xlsx", 
                        sheet = "Rain")
pb1680_Tt = read.csv("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Well Data/station_8722802.csv")

# Calculating to be on a monthly basis for Well Data
pb1680w_sum <- pb1680_Tw %>% 
  filter(Code =="A") %>%
  mutate(datetime = as.POSIXct(paste(year(date), month(date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(wellheight=mean(Corrected))

# Calculating to be on a monthly basis for Well Data
pb1680t_sum <- pb1680_Tt %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  summarise(tide=mean(Prediction))

# Calculating to be on a monthly basis for Well Data
pb1680r_sum <- pb1680_Tr %>% 
  mutate(datetime = as.POSIXct(paste(year(Date), month(Date),'01', sep='-'))) %>%
  group_by(datetime) %>%
  mutate(RAIN_FT = RAIN_FT*12) %>%
  summarise(raininches=sum(RAIN_FT))



# Creating a Time Series Sequence to merge to capture any missing values 
# For Well 
wellDates1680 <- data.frame(datetime=seq.POSIXt(from= min(pb1680w_sum$datetime),
                                                to = max(pb1680w_sum$datetime),
                                                by = "month"),x = 1)
# for Tide
tideDates1680 <- data.frame(datetime=seq.POSIXt(from= min(pb1680w_sum$datetime),
                                                to = max(pb1680w_sum$datetime),
                                                by = "month"),x = 1)

# for Rain
rainDates1680 <- data.frame(datetime=seq.POSIXt(from= min(pb1680w_sum$datetime),
                                                to = max(pb1680w_sum$datetime),
                                                by = "month"),x = 1)

# Capturing total missing for well
nottestwelldf1680 <- anti_join(wellDates1680,pb1680w_sum , by = "datetime")
# Left join
testwelldf1680 <- left_join(wellDates1680, pb1680w_sum , by="datetime")

# Using na.approx for imputing missing values
testwelldf1680$wellheight <- na.approx(testwelldf1680$wellheight)  

# Capturing total missing for tide
nottesttidedf1680 <- anti_join(tideDates1680,pb1680t_sum , by = "datetime")
# Left join
testtidedf1680 <- left_join(tideDates1680, pb1680t_sum , by="datetime")

# Using na.approx for imputing missing values
testtidedf1680$tide <- na.approx(testtidedf1680$tide)  

# Capturing total missing for rain
nottestraindf1680 <- anti_join(rainDates1680,pb1680r_sum , by = "datetime")
# Left join
testraindf1680 <- left_join(rainDates1680, pb1680r_sum , by="datetime")

# Using na.approx for imputing missing values
testraindf1680$raininches <- na.approx(testraindf1680$raininches)  

# Dropping Columns
testwelldf1680$x = NULL
testraindf1680$x = NULL
testtidedf1680$x = NULL

pb1680_monthly = merge (testwelldf1680, testraindf1680,  by ="datetime")
pb1680_monthly = merge (pb1680_monthly, testtidedf1680,  by ="datetime")

#----------------------------------------------CREATE SPECIFIC TIME PERIOD---------------------------------------------
# Create time frame from 2018/01/01 to 2018/01/01 - 10 Years for each well 
f45_monthly <- f45_monthly[4:124,]
f179_monthly <- f179_monthly[4:124,]
f319_monthly <- f319_monthly[4:124,]
g1220_monthly <- g1220_monthly[4:124,]
g1260_monthly <- g1260_monthly[4:124,]
g2147_monthly <- g2147_monthly[4:124,]
g2866_monthly <- g2866_monthly[4:124,]
g3549_monthly <- g3549_monthly[4:124,]
g561t_monthly <- g561t_monthly[4:124,]
g580a_monthly <- g580a_monthly[4:124,]
g860_monthly <- g860_monthly[4:124,]
pb1680_monthly <- pb1680_monthly[4:124,]
g852_monthly <- g852_monthly[91:211,]

# Reset Index 
rownames(f45_monthly) <- NULL
rownames(f179_monthly ) <- NULL
rownames(f319_monthly) <- NULL
rownames(g1220_monthly) <- NULL
rownames(g1260_monthly) <- NULL
rownames(g2147_monthly) <- NULL
rownames(g2866_monthly) <- NULL
rownames(g3549_monthly) <- NULL
rownames(g561t_monthly) <- NULL
rownames(g580a_monthly) <- NULL
rownames(g860_monthly) <- NULL
rownames(pb1680_monthly) <- NULL
rownames(g852_monthly) <- NULL

#------------------------------------------------------CREATE MODELS---------------------------------------------------


############## NEED TO DECIDE ON MODEL ##################



#---------------------------------------------------------EXPORT---------------------------------------------------------
# Now writing out CSVs for each dataset in order to bring into Tableau
write.csv(f45_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/f45_monthly.csv')
write.csv(f179_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/f179_monthly.csv')
write.csv(f319_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/f319_monthly.csv')
write.csv(g1220_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/g1220_monthly.csv')
write.csv(g1260_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/g1260_monthly.csv')
write.csv(g2147_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/g2147_monthly.csv')
write.csv(g2866_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/g2866_monthly.csv')
write.csv(g3549_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/g3549_monthly.csv')
write.csv(g561t_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/g561t_monthly.csv')
write.csv(g580a_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/g580a_monthly.csv')
write.csv(g860_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/g860_monthly.csv')
write.csv(pb1680_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/bp1680_monthly.csv')
write.csv(g852_monthly, file='/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Visualization Project/Cleaned Well Data/g852_monthly.csv')

