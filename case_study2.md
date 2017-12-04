---
title: "Report"
output:
  html_document: default


Introduction:

The data project seeks to explore the market research incorporated with the Human Development Index (HDI).  This report analyzes two datasets: Procrastination data and a list of countries classified based on the Human Development Index (HDI). This value comes from the Human Development Report released on March 21,2017 by the United Nations Development Program. HDI combines indicators of life expectancy, education, and income. Countries fall into four categories: Very High Human Development, High Human Development, Medium Human Development, Low Human Development.

The Procrastination is a quantitative and categorical data by Qualtrics.The quantitative data is assessed on a numerical scale (age,income,..) and categorical data is assessed on a nominal scale (gender, marital status, occupation,..). Most data is collected in a survey response which includes: question responses, participant's information and scoring result.The scoring result is a sum of the points the respondent earned in the category, not an average.The question responses's columns in the data set display the answers provided for each survey question. The participant information includes information about each respondant and their responses, such as age, education, work status, employment, country of residence, etc.

original datasets are:
Procrastination.csv
https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index#Complete_list_of_countries

This report will include Cleaning datasets, Analysing datasets, Visualizing datasets.

To begin we will load the required package.
```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
library(graphics)
library(plyr)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(dplyr)
library(XML)
library(RCurl)
library(stringr)
library(rvest)
library(tibble)

```
Data Cleaning
Read the files into dataframe, using the "read.csv" from readr package

```{r}
my_data <- read.csv("~/School/2017-09/MSDS6306/CaseStudyTwoMaterials/Procrastination.csv",header=T,sep=",")
my_wiki <- read.csv("~/School/2017-09/MSDS6306/ft3.csv", header = T ,sep = ",")
```
Rename the column name and converted the factor columns to  characters.(2b)

```{r}
my_wiki$HDI.categories <- sapply(my_wiki$HDI.categories, as.character)
colnames(my_data)[1] <- c('Age')
```
Some columns have impossible data values." country of residence"" column the "0" is treated as missing value. The "number of sons" was labeled with Male(1), Female(2) and chaneged it to integers. "current occupation" column has "0","please specify" which would treated as missing. (2c)
```{r}
str(my_data$How.long.have.you.held.this.position...Years)
summary(my_data$How.long.have.you.held.this.position...Years)
summary(my_data$How.long.have.you.held.this.position...Months)
###In order to correct the number of years I was thinking of using number of months but it appears that these fields should be  x years and y months since max(month) is 11. and this part is judgmental/subjective) from what I can see in the data I can create a function and update the year field. below I put  some stats that can be usefull and some example function but based on stasts I may want to modify it to appear more appropriate values such that all 999 years are transform to mean/median (any meaningful number that can represent my data). 
str(my_data$Work.Status)
my_data$Work.Status <- sapply(my_data$Work.Status, as.character)
hist(my_data$Age)
hist(my_data$How.long.have.you.held.this.position...Years)
length(my_data$How.long.have.you.held.this.position...Years[my_data$How.long.have.you.held.this.position...Years > 30])
length(my_data$How.long.have.you.held.this.position...Years[my_data$How.long.have.you.held.this.position...Years == 999])

length(my_data$How.long.have.you.held.this.position...Years[my_data$How.long.have.you.held.this.position...Years > 30 & my_data$Work.Status == 'student'])
length(my_data$How.long.have.you.held.this.position...Years[my_data$How.long.have.you.held.this.position...Years > 30 & my_data$Work.Status == 'unemployed'])
length(my_data$How.long.have.you.held.this.position...Years[my_data$How.long.have.you.held.this.position...Years > 30 & my_data$Work.Status == 'full-time'])
length(my_data$How.long.have.you.held.this.position...Years[my_data$How.long.have.you.held.this.position...Years > 30 & my_data$Work.Status == 'part-time'])
length(my_data$How.long.have.you.held.this.position...Years[my_data$How.long.have.you.held.this.position...Years > 30 & my_data$Work.Status == 'retired'])
length(my_data$How.long.have.you.held.this.position...Years[my_data$How.long.have.you.held.this.position...Years > 30 & is.na(my_data$Work.Status)])
summary(my_data$How.long.have.you.held.this.position...Years[my_data$How.long.have.you.held.this.position...Years !=999])
summary(my_data$How.long.have.you.held.this.position...Years[my_data$Work.Status == 'full-time' & my_data$How.long.have.you.held.this.position...Years !=999])
summary(my_data$How.long.have.you.held.this.position...Years[my_data$Work.Status == 'student'])
summary(my_data$How.long.have.you.held.this.position...Years[my_data$Work.Status == 'student' & my_data$How.long.have.you.held.this.position...Years !=999])


  year_correctin <- function(year,age,workStat){
  if (is.na(year)) {return(year)}
  else if (year == 999 & (is.na(age) | age < 18) & is.na(workStat)) {return(4)}
  else if (year == 999 & (is.na(age) | age < 18) & workStat == 'unemployed') {return(3)}
  else if (year == 999 & (is.na(age) | age < 18) & workStat == 'student') {return(2)}
  else if (year == 999 & (is.na(age) | age < 18) & workStat == 'full-time') {return(5)}
  else if (year == 999 & (is.na(age) | age < 18) & workStat == 'retired') {return(4)}
  else if (year == 999 & (is.na(age) | age < 18) & workStat == 'part-time') {return(4)}
  else if (year == 999 & age < 31 & is.na(workStat)) {return(2)}
  else if (year == 999 & age < 31 & workStat == 'unemployed') {return(1)}
  else if (year == 999 & age < 31 & workStat == 'student') {return(2)}
  else if (year == 999 & age < 31 & workStat == 'full-time') {return(2)}
  else if (year == 999 & age < 31 & workStat == 'part-time') {return(1)}
  else if (year == 999 & age < 31 & workStat == 'retired') {return(1)}
  else if (year == 999 & (age > 30 & age < 51) & is.na(workStat)) {return(5)}
  else if (year == 999 & (age > 30 & age < 51) & workStat == 'unemployed') {return(3)}
  else if (year == 999 & (age > 30 & age < 51) & workStat == 'retired') {return(6)}
  else if (year == 999 & (age > 30 & age < 51) & workStat == 'student') {return(3)}
  else if (year == 999 & (age > 30 & age < 51) & workStat == 'full-time') {return(5)}
  else if (year == 999 & (age > 30 & age < 51) & workStat == 'part-time') {return(5)}
  else if (year == 999 & (age > 50 & age < 65) & is.na(workStat)) {return(8)}
  else if (year == 999 & (age > 50 & age < 65) & workStat == 'unemployed') {return(5)}
  else if (year == 999 & (age > 50 & age < 65) & workStat == 'retired') {return(3)}
  else if (year == 999 & (age > 50 & age < 65) & workStat == 'student') {return(4)}
  else if (year == 999 & (age > 50 & age < 65) & workStat == 'full-time') {return(9)}
  else if (year == 999 & (age > 50 & age < 65) & workStat == 'part-time') {return(6)}
  else if (year == 999 & age > 64 & is.na(workStat)) {return(2)}
  else if (year == 999 & age > 64 & workStat == 'unemployed') {return(6)}
  else if (year == 999 & age > 64 & workStat == 'full-time') {return(12)}
  else if (year == 999 & age > 64 & workStat == 'part-time') {return(9)}
  else if (year == 999 & age > 64 & workStat == 'retired') {return(4)}
  else if (year == 999 & age == 67.5 & workStat == 'student') {return(4)}
  else if (year < 1) { return(0) }	
  else {return(year)}
	}

my_data$How.long.have.you.held.this.position...Years <- mapply(year_correctin, my_data$How.long.have.you.held.this.position...Years, my_data$Age, my_data$Work.Status)


str(my_data$Number.of.sons) 
my_data$Number.of.sons <- sapply(my_data$Number.of.sons, as.character)
my_data$Number.of.sons[my_data$Number.of.sons == "Male"] <- "1"
my_data$Number.of.sons[my_data$Number.of.sons == "Female"] <- "2"
my_data$Number.of.sons <- sapply(my_data$Number.of.sons, as.numeric)
summary(my_data$Number.of.sons) 


str(my_data$Country.of.residence) 
summary(my_data$Country.of.residence) 
my_data$Country.of.residence <- sapply(my_data$Country.of.residence, as.character)
my_data$Country.of.residence[my_data$Country.of.residence == "0"] <- NA


str(my_data$Current.Occupation) 
summary(my_data$Current.Occupation) 
my_data$Current.Occupation <- sapply(my_data$Current.Occupation, as.character)
my_data$Current.Occupation[my_data$Current.Occupation == "0" | my_data$Current.Occupation == "" | my_data$Current.Occupation == "please specify"] <- NA
```
Columns have been converted to the proper data types
(2d)

```{r}
 my_data$Gender<- sapply(my_data$Gender, as.character)
 my_data$Kids<- sapply(my_data$Kids, as.character)
 my_data$Edu<- sapply(my_data$Edu, as.character)
 my_data$Marital.Status<- sapply(my_data$Marital.Status, as.character)
 my_data$Marital.Status[my_data$Marital.Status == "0"] <- NA
 my_data$Community.size<- sapply(my_data$Community.size, as.character)
 my_data$Community.size[my_data$Community.size =="0" | my_data$Community.size =="8"] <- NA
 my_data$Kids[my_data$Kids == "Yes Kids"]<- "Yes"
 my_data$Kids[my_data$Kids == "No Kids"]<- "No"
 my_data$Do.others.consider.you.a.procrastinator.<- sapply(my_data$Do.others.consider.you.a.procrastinator., as.character)
 my_data$Do.you.consider.yourself.a.procrastinator.<- sapply(my_data$Do.you.consider.yourself.a.procrastinator., as.character)
 my_data$Do.others.consider.you.a.procrastinator.[my_data$Do.others.consider.you.a.procrastinator. == "0" | my_data$Do.others.consider.you.a.procrastinator.== "4" ] <- NA
````
Generated the individual's average of decisional procrastination(DP), adult inventory procrastination(AIP), generalized procrastination(GP) and satisfaction with life scale(SWLS) 
2(e)

```{r}
my_data$DPMean<- rowMeans(my_data[,15:19])
 my_data$AIPMean<- rowMeans(my_data[,20:34])
 my_data$GPMean<- rowMeans(my_data[,35:54])
 my_data$SWLSMean<- rowMeans(my_data[,55:59])
````
Scraping the Wikipedia website for the "List of countries by Human Development Index". Total of 8 tables were cleaned and bind into one singular table. The scraped table (ft3) has 3 columns "country", "HDI", "HDI categories". The output table (ft3) is csv file 
3(a), 3(b)

```{r}
url<- 'https://en.wikipedia.org/wiki/List_of_countries_by_Human_Development_Index#Complete_list_of_countries'
appData <- getURL(url, ssl.verifypeer = FALSE)
doc <- htmlParse(appData)
appData <- doc['//table[@class="wikitable"]']
table1 <- readHTMLTable(appData[[1]], stringsAsFactors = F, skip.rows = 1)
table2 <- readHTMLTable(appData[[2]], stringsAsFactors = F, skip.rows = 1)
table3 <- readHTMLTable(appData[[3]], stringsAsFactors = F, skip.rows = 1)
table4 <- readHTMLTable(appData[[4]], stringsAsFactors = F, skip.rows = 1)
table5 <- readHTMLTable(appData[[5]], stringsAsFactors = F, skip.rows = 1)
table6 <- readHTMLTable(appData[[6]], stringsAsFactors = F, skip.rows = 1)
table7 <- readHTMLTable(appData[[7]], stringsAsFactors = F, skip.rows = 1)
table8 <- readHTMLTable(appData[[8]], stringsAsFactors = F, skip.rows = 1)
t1<- add_column(table1, V6 = "Very high huaman development", .after= "V3")
t2<- add_column(table2, V6 = "Very high huaman development", .after= "V3")
t3<- add_column(table3, V6 = "High huaman development", .after= "V3")
t4<- add_column(table4, V6 = "High huaman development", .after= "V3")
t5<- add_column(table5, V6 = "Medium huaman development", .after= "V3")
t6<- add_column(table6, V6 = "Medium huaman development", .after= "V3")
t7<- add_column(table7, V6 = "Low huaman development", .after= "V3")
t8<- add_column(table8, V6 = "Low huaman development", .after= "V3")
ft<- do.call("rbind",list(t1,t2,t3,t4,t5,t6,t7,t8))
names(ft)<- c("rank1","rank2","country","HDI categories","HDI","HDI2")
str(ft)
 
 ft1<- select(ft, "country","HDI categories", "HDI")
 ###ft1$x<- paste(ft1$country, ft1$`HDI categories`)
 write.csv(ft1, file = "ft3.csv", row.names = FALSE)
````
Merge the ft3 data frame with the procrastination data frame so that the new daa frame has an HDI column and HDI categories. 
3(c)

```{r}
my_wiki$country<- sapply(my_wiki$country, as.character)
 my_wiki<- select(my_wiki, "country", "HDI.categories", "HDI")
 mg<- merge(my_data,my_wiki, by.x = "Country.of.residence", by.y = "country", all.x = TRUE)
 
````
Preliminary Analysis
Removed all observations where the participant is under age 18. (4a)

Created a simple histogram for Age and DPMean.4(b). Based on the plot it is a normal distribution for DPMean and a left skewd for Age plot.

Created seprate tables for "Gender", "Work Status" and "occupation". (4c)

counts of participants per country in descending order:    (4d)

Clarfy these two variable in the set : "Do you consider yourself a procrastinator?" and "Do others consider you a procrastinator?" (yes/yes) or (no/no)   (4e).
(no/no = 1100)   (yes/yes= 2358)


  
```{r}
#### 4A
my_data2<- mg[mg$Age> 18, ]
####4B
pft<- summary(my_data2$Age)
pft_Income<- summary(my_data2$Annual.Income)
pft_DP<- summary(my_data2$DPMean)
pft_GP<- summary(my_data2$GPMean)
pft_AIP<- summary(my_data2$AIPMean)
pft_Swl<- summary(my_data2$SWLSMean)
pft_HDI<- summary(my_data2$HDI)
pft_all<- do.call("rbind",list(pft,pft_Income,pft_DP,pft_GP,pft_AIP,pft_Swl,pft_HDI))
row.names(pft_all)<- c("Age", "Income", "MeanDP","MeanGP", "MeanAIP", "MeanSwl", "HDI")
hist(my_data2$Age)
hist(my_data2$DPMean)
####4C
 table(my_data2$Gender)
 table(my_data2$Work.Status)
 table(my_data2$Current.Occupation)
#### 4d
data.frame( rev(sort(table(my_data2$Country.of.residence))))
###4e
table(my_data2$Do.you.consider.yourself.a.procrastinator., my_data2$Do.others.consider.you.a.procrastinator.)
```
Deeper Analysis
Created a barchart of top 15 nations in average procrastination scores using DPMean measure. (5b)

Created another barchart of top 15 nations in average procrasination score using AIPMean measure. (5c). Total of 9 nations show up in both (5c and 5b) charts.Taiwan, Macao, Panama, puerto Rico, Qatar, Columbia, Ecuador, Sirlanka, Uruguay.

Created a scatter plot of Age and Income based on Gender of the participants. (5d). The below coefficient is insignificant, therfore there is no relationship between age and incime.
Coefficients:
 (Intercept)  my_data2$Age  
     4.09577       0.01314  

Created a scatterplot of Life Satisfaction and HDI. (5e). Based on the coefficient value the relationship between LS and HDI is perceptible.
Coefficients:
 (Intercept)  my_data2$HDI  
      2.4053        0.7046  
Also based on the barplot result the relationship between HDI categories and Life Satisfaction is visble.


```{r}
#### 5B
test<- data.frame(aggregate(my_data2$DPMean,by=list(Country=my_data2$Country.of.residence),FUN=mean))
nation<- test[order(-test$x),][0:15,]
mg1<- merge(nation,my_wiki, by.x = "Country", by.y = "country", all.x = TRUE)
mg1<- mg1[order(-mg1$x),]
p<- ggplot(mg1, aes(mg1$Country, mg1$x))
p+ geom_bar(stat = "identity", aes(fill= mg1$HDI.categories), position = "dodge")+ xlab("nations") + ylab("DPMean") + ggtitle("Top 15 nations by DPMean") +theme_bw() +theme(axis.text.x = element_text(angle = 90, hjust = 1)) +scale_x_discrete(limits=mg1$Country)

###5C
test<- data.frame(aggregate(my_data2$AIPMean,by=list(Country=my_data2$Country.of.residence),FUN=mean))
nation<- test[order(-test$x),][0:15,]
mg1<- merge(nation,my_wiki, by.x = "Country", by.y = "country", all.x = TRUE)
mg1<- mg1[order(-mg1$x),]
p<- ggplot(mg1, aes(mg1$Country, mg1$x))
p+ geom_bar(stat = "identity", aes(fill= mg1$HDI.categories), position = "dodge")+ xlab("nations") + ylab("AIPMean") + ggtitle("Top 15 nations by AIPMean") +theme_bw() +theme(axis.text.x = element_text(angle = 90, hjust = 1)) +scale_x_discrete(limits=mg1$Country)

####5d
p<- ggplot(my_data2, aes(my_data2$Age, log10(my_data2$Annual.Income)))
p+ geom_point(stat = "identity", aes(color= my_data2$Gender),  position=position_jitter())+ xlab("Age") + ylab("Income") + ggtitle("Age versus Income") +theme_bw()
lm(log10(my_data2$Annual.Income)~my_data2$Age)

####5e
p<- ggplot(my_data2, aes(my_data2$HDI, my_data2$SWLSMean))
p+ geom_point(stat = "identity", aes(color= my_data2$Gender),  position=position_jitter())+ xlab("HDI") + ylab("Life Satisfction") + ggtitle("HDI versus Life Satisfaction") +theme_bw()
lm(my_data2$SWLSMean~my_data2$HDI)

p<- ggplot(my_data2, aes(my_data2$HDI.categories,my_data2$SWLSMean))
p+ geom_bar(stat = "identity", position = "dodge")+ xlab("HDI categories") + ylab("Life Satisfaction") + ggtitle("HDI categories versus Life Satisfaction") +theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
Output:

Top_15_nations_by_averageAIPMean.csv
Top_15_nations_by_averageDPMean.csv
ft3.csv
Clean_Procrastination.csv
```{r}
####### OUTPUT 6 b
write.csv(my_data2, "Clean_Procrastination.csv", row.names = FALSE)
####### OUTPUT 6 c
test<- data.frame(aggregate(my_data2$DPMean,by=list(Country=my_data2$Country.of.residence),FUN=mean))
nation<- test[order(-test$x),][0:15,]
mg1<- merge(nation,my_wiki, by.x = "Country", by.y = "country", all.x = TRUE)
mg1<- mg1[order(-mg1$x),]
colnames(mg1)[2]<- c('AverageDPMean')
write.csv(mg1, "Top_15_nations_by_averageDPMean.csv", row.names = FALSE)

test<- data.frame(aggregate(my_data2$AIPMean,by=list(Country=my_data2$Country.of.residence),FUN=mean))
nation<- test[order(-test$x),][0:15,]
mg1<- merge(nation,my_wiki, by.x = "Country", by.y = "country", all.x = TRUE)
mg1<- mg1[order(-mg1$x),]
colnames(mg1)[2]<- c('AverageAIPMean')
write.csv(mg1, "Top_15_nations_by_averageAIPMean.csv", row.names = FALSE)

````
Coclusion:

This report can be summarized by the results obtained from the plots. Based on the barchart's display, there is a normal distribution between HDI, HDI categories and Satisfaction with Life Scale(SWLS) and the mean of the Decisional Procrastination Scale (DPMean). It produces most potential countries for investments based on the chosen criterias.

