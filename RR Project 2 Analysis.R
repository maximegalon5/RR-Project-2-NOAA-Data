library(tidyverse, warn.conflicts = F)
DlURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists("Noaa.bz2")){
  download.file(DlURL, destfile = "./Noaa.bz2")
}
if(!exists("rawData")){
  rawData <- as.tibble(read.csv(bzfile("./Noaa.bz2")))
}
str(rawData)
##Public Health Subset
PHSubsetData <- rawData %>% select(EVTYPE, FATALITIES, INJURIES) %>% 
  group_by(EVTYPE) %>% summarise_all(sum)
TopInjuries <- PHSubsetData %>% 
  arrange(desc(INJURIES)) %>%
  top_n(10, INJURIES) %>% 
  mutate(INJURIES = INJURIES/1000)
TopFatalities <- PHSubsetData %>% 
  arrange(desc(FATALITIES)) %>%
  top_n(10, FATALITIES) %>% 
  mutate(INJURIES = INJURIES/1000)
TopPHIssues <- union(TopInjuries, TopFatalities) %>% arrange(desc(FATALITIES))
##Plot
par(mar = c(5,11,5,0))
par(mfrow = c(1,2))
barplot(TopPHIssues$FATALITIES, names.arg = TopPHIssues$EVTYPE, las=2, horiz = T, xlim = c(0,6000), main = "Fatalities", col = "pink")
par(mar = c(5,2,5,2))
barplot(TopPHIssues$INJURIES, las=2, horiz = T, main = "Injuries (in 1000's)", xlim = c(0,100), col = "purple")
##Financial Impact
FinSubsetData <- rawData %>% select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
  filter(!(PROPDMGEXP %in% (c("-", "+", "?", "", "0", "5", "3")))) %>%
  filter(!(CROPDMGEXP %in% (c("-", "+", "?", "", "0"))))
##Function to change the damage suffix to numerical amounts (in Thousands)
SufFun <- function(char){
  char <- as.character(char);
  char <- gsub("m|M", 1000000, char);
  char <- gsub("b|B", 1000000000, char);
  char <- gsub("k|K", 1000, char);
  char <- as.numeric(char)
}
FinSubsetData$CROPDMGEXP <- SufFun(FinSubsetData$CROPDMGEXP)
FinSubsetData$PROPDMGEXP <- SufFun(FinSubsetData$PROPDMGEXP)
SumFinImpact <- FinSubsetData %>% 
  mutate(Cost = (CROPDMG*CROPDMGEXP) + (PROPDMG*PROPDMGEXP)) %>%
  group_by(EVTYPE) %>% 
  summarise_all(sum) %>%
  mutate(Cost = Cost/10^9) %>%
  arrange(desc(Cost)) %>%
  top_n(15, Cost)
SumFinImpact$Cost <- round(SumFinImpact$Cost, digits = 2)
##Property Damage
PropDamage <- FinSubsetData %>%
  select(EVTYPE, PROPDMG, PROPDMGEXP) %>%
  mutate(Cost = PROPDMG*PROPDMGEXP) %>%
  group_by(EVTYPE) %>% 
  summarise_all(sum) %>%
  mutate(Cost = Cost/10^9) %>%
  arrange(desc(Cost)) %>%
  top_n(10, Cost)
PropDamage$Cost <- round(PropDamage$Cost, digits = 2)
##CropDamage
CropDamage <- FinSubsetData %>%
  select(EVTYPE, CROPDMG, CROPDMGEXP) %>%
  mutate(Cost = CROPDMG*CROPDMGEXP) %>%
  group_by(EVTYPE) %>% 
  summarise_all(sum) %>%
  mutate(Cost = Cost/10^9) %>%
  arrange(desc(Cost)) %>%
  top_n(10, Cost)
CropDamage$Cost <- round(CropDamage$Cost, digits = 2)
##Plot
par(mfrow = c(1,2))
par(mar = c(5,11,5,0))
barplot(PropDamage$Cost, names.arg = PropDamage$EVTYPE, las=2, horiz = T, xlim = c(0,150), main = "Propety Damage Cost in $B", col = "Magenta")
par(mar = c(5,11,5,5))
barplot(CropDamage$Cost, names.arg = CropDamage$EVTYPE, las=2, horiz = T, main = "Crop Damage Cost in $B", xlim = c(0,6), col = "Coral")
##Plot
par(mfrow = c(1,1))
par(mar = c(5,14,5,2))
barplot(SumFinImpact$Cost, names.arg = SumFinImpact$EVTYPE, las=2, horiz = T, main = "Top 15 Costliest Forms of Natural Disasters (NOAA Data)", xlim = c(0,150), col = "Orange", sub = "Cost in Billions")

