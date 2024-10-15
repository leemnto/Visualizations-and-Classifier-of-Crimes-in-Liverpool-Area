#Install Packages
install.packages("tidyverse")
install.packages("tmap")
install.packages("caret")
install.packages("rpart")
install.packages("cartogram")
install.packages("missForest")
install.packages("randomForest")
install.packages("dplyr")
install.packages("base")
install.packages("GGally")

library(tidyverse)
library(tmap)
library(caret)
library(rpart)
library(cartogram)
library(missForest)
library(randomForest)
library(dplyr)
library(base)
library(GGally)


#Read Liverpool mapfile
liverpoolshape<-readOGR(dsn="./BoundaryData", layer="england_lsoa_2011")
qtm(liverpoolshape)


#Read Population Data
deprivation2015<-read.csv("2015ranks.csv")

#Transform Population Data and Rename Columns
deprivation2015code<-deprivation2015 %>% select(LSOA.name..2011.,
                                                LSOA.code..2011., 
                                                Total.population..mid.2012..excluding.prisoners.)
names(deprivation2015code)[names(deprivation2015code)=="LSOA.name..2011."]<-"LSOA_name"
names(deprivation2015code)[names(deprivation2015code)=="LSOA.code..2011."]<-"LSOA_code"
names(deprivation2015code)[names(deprivation2015code)=="Total.population..mid.2012..excluding.prisoners."]<-"Total_population"

#Match and Combine Population Data and Map File Data
liverpoolshape@data<-left_join(liverpoolshape@data, deprivation2015code,
                               by=c('code'='LSOA_code'))

#Create Population Distribution Map
qtm(liverpoolshape, fill="Total_population", fill.palette="Blues", borders="White")

#Read Crime Data
crimes<-read.csv("1911street.csv", header=TRUE)

#Create Bar Chart of Crime Types
typecrimes<-crimes %>%
  select(LSOA.code, Crime.type) %>%
  group_by(Crime.type) %>%
  summarise(typecrimes=n())
ggplot(typecrimes, aes( x = reorder(Crime.type, typecrimes), 
                        y = typecrimes)) + geom_col() +
  labs(x="Crime Type", y="Number of Crimes",title = "Crimes in 2019-11") +
  coord_flip() +
  theme_light()

#Transform Crime Data
noofcrimes<-crimes %>%
  select(LSOA.code, LSOA.name, Crime.type) %>%
  group_by(LSOA.code) %>%
  summarise(nocrimes=n())

#Match and Combine Crime Data and Map File Data
liverpoolshape@data<-left_join(liverpoolshape@data, noofcrimes,
                               by=c('code'='LSOA.code'))
#Transform Missing Data
liverpoolshape[is.na(liverpoolshape@data$nocrimes)]<-0

#Create Crime Distribution Map
tm_shape(liverpoolshape) +
  tm_fill("nocrimes", style="kmeans", border.col = "black", title = "Number of Crimes") +
  tm_borders(alpha=0.2)

#Create Crime Distribution Cartogram
livercar<-cartogram(liverpoolshape, weight="nocrimes", itermax=10, prepare="adjust")
tm_shape(livercar) +
  tm_fill("nocrimes", style="jenks", title = "Number of Crimes") +
  tm_borders() + tm_layout(frame=F)

#Test for Normality - Number of Crimes
noofcrimes<-crimes %>%
  select(LSOA.code, LSOA.name, Crime.type) %>%
  group_by(LSOA.code) %>%
  summarise(nocrimes=n())
shapiro.test(noofcrimes$nocrimes)

#Test for Normality - Population
shapiro.test(liverpoolshape@data$Total_population)

#Correlation Scatter Plot
ggplot(liverpoolshape@data, aes(Total_population, nocrimes)) +
  geom_point()+
labs(x="Total Population",y="Number of Crimes") 
#Correlation Test
pop = liverpoolshape@data$Total_population
crime = liverpoolshape@data$nocrimes
cor(pop, crime, method="kendall")

######### Machine Learning ############
#Read Liverpool mapfile
liverpoolshape<-readOGR(dsn="./BoundaryData", layer="england_lsoa_2011")
#Read Population Data
de2015<-read.csv("deprivationdata.csv")
#Match and Combine Data
liverpoolshape@data<-left_join(liverpoolshape@data, de2015,
                               by=c('code'='LSOA.code..2011.'))
liverpoolshape@data<-left_join(liverpoolshape@data, noofcrimes,
                               by=c('code'='LSOA.code'))

#Rename Columns
names(liverpoolshape@data)[names(liverpoolshape@data)=="Crime.Score"]<-"Crime_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Education..Skills.and.Training.Score"]<-"Education_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Index.of.Multiple.Deprivation..IMD..Score"]<-"IMD_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Living.Environment.Score"]<-"Live_Environ_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Adult.Skills.Sub.domain.Score"]<-"Adult_Skill_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Barriers.to.Housing.and.Services.Score"]<-"Barriers_HS_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Dependent.Children.aged.0.15..mid.2012..excluding.prisoners."]<-"De_Children"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Employment.Score..rate."]<-"Employment_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Geographical.Barriers.Sub.domain.Score"]<-"Geo_Bar_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Health.Deprivation.and.Disability.Score"]<-"Health_Dis_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Income.Deprivation.Affecting.Older.People..IDAOPI..Score..rate."]<-"Income_Aff_Old"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Income.Score..rate."]<-"Income_Score_Rate"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Indoors.Sub.domain.Score"]<-"Indoors_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Older.population.aged.60.and.over..mid.2012..excluding.prisoners."]<-"Older_Pop"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Outdoors.Sub.domain.Score"]<-"Outdoors_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Population.aged.16.59..mid.2012..excluding.prisoners."]<-"Middle_Pop"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Working.age.population.18.59.64..for.use.with.Employment.Deprivation.Domain..excluding.prisoners."]<-"Working_Pop"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Children.and.Young.People.Sub.domain.Score"]<-"Young_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Income.Deprivation.Affecting.Children.Index..IDACI..Score..rate."]<-"Income_Aff_Child"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Wider.Barriers.Sub.domain.Score"]<-"Wider_Bar_Score"
names(liverpoolshape@data)[names(liverpoolshape@data)=="Total.population..mid.2012..excluding.prisoners."]<-"Total_population"

#Independent Values Correlation Plot
livercor = subset(liverpoolshape@data, select = c(7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,56,57,58,59) )
ggpairs(livercor)

#Deal with Missing Value
randomcrime = subset(liverpoolshape@data, select = c(7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,56,57,58,59,60))
anyNA(randomcrime)
#Using Random Forest to Predict Missing Values
filter.randomcrime<-missForest(randomcrime)
str(filter.randomcrime)
pima.filter.crime<-filter.randomcrime$ximp
print(table(randomcrime$nocrimes))
print(table(pima.filter.crime$nocrimes))

#Scale and Normalize Data
trans<-pima.filter.crime %>% select(nocrimes)
pima.scale.data<-scale(pima.filter.crime[,-22])
pima.scale.data<-cbind(pima.scale.data, trans)

#Break the Dependent Value (Number of Crimes) into 3 types - high, medium, low
summary(pima.scale.data$nocrimes)
brks=c(0,8,25,466)
pima.scale.data$nocrimes=cut(pima.scale.data$nocrimes, breaks=brks)
str(pima.scale.data$nocrimes)
is.na(pima.scale.data$nocrimes)

#Split the data into Training Dataset and Test Dataset
set.seed(203)
splitdata<-pima.scale.data$nocrimes %>%
  createDataPartition(p=0.8, list=FALSE)
train.data<-pima.scale.data[splitdata,]
test.data<-pima.scale.data[-splitdata,]
#Dataset Overview
summary(train.data)
summary(test.data)
print(table(train.data$nocrimes))
print(table(test.data$nocrimes))

####### Decision Tree #######
#Create Decision Tree
rpart.model<-rpart(nocrimes~., data=train.data, method = "class")
rpart.predictions<-rpart.model %>% predict(test.data, type = "class")
mean(rpart.predictions == test.data$nocrimes)
#Create Decision Tree Plot
plot(rpart.model, main="Classification Tree for the Number of Crimes (Training Data)")
text(rpart.model, digits=45, cex=0.9)

######## Random Forest Classification #######
rfmodel<-randomForest(nocrimes ~.,ntree=2000, data = train.data)
print(rfmodel)

#Variables Importance Sequence
varImpPlot(rfmodel, sort=T, n.var = 21, main = "Variable Importance Plot")

#Train the dataset
rf.predictions<-predict(rfmodel, train.data)
print(caret::confusionMatrix(data = rf.predictions,
                             reference = train.data$nocrimes))
#Predict the Number of Crimes
rf.predictions <- predict(rfmodel, test.data)
print(caret::confusionMatrix(data = rf.predictions, reference = test.data$nocrimes))