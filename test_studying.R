library(readxl)
Example_Data <- read_excel("Example_Data.xlsx", 
                           sheet = "Feuil1")
View(Example_Data)

##########################################################################################
View(Example_Data)
head(Example_Data, n=20)
fix(Example_Data) #Change manually variable "Age..13" to "NoAge" and "Age..12" to "Age"
names(Example_Data)[names(Example_Data) == "Age...13"] <- "NoAge"
#########################################################################################
dim(Example_Data)
nrow(Example_Data)
ncol(Example_Data)
########################################################################################

favor_col<-c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,1,1,2)
favor_col
Example_Data<-cbind(Example_Data,favor_col)
View(Example_Data)
dim(Example_Data)
#######################################################################################
names(Example_Data)
#######################################################################################
Example_Data$...13 <-NULL
Example_Data$...14 <-NULL
Example_Data$...15 <-NULL
View(Example_Data)
#######################################################################
setwd("~/Downloads")
save(Example_Data, file = "Example_Data.RData")
#######################################################################
Example_Data45 <- read_excel("Example_Data45.xlsx",sheet = "Feuil1")
save(Example_Data45, file = "Example_Data45.RData")
names(Example_Data)
names(Example_Data45)

dim(Example_Data45)
favor_col <- c(1,2,3,4,1,2,3,4,1)
length(favor_col)
Example_Data45$favor_col <-NULL
#########################################################################
Example_Data45<-cbind(Example_Data45, favor_col)
View(Example_Data45)

View(Example_Data)
Example_Data<-rbind(Example_Data,Example_Data45)
View(Example_Data)

dim(Example_Data)
########################################################################

names(Example_Data)[names(Example_Data) == "Age...12"] <- "Age"
View(Example_Data)


write.csv(Example_Data, "Example_DataF.csv")

Example_DataYoung<-Example_Data[Example_Data$Age<25,]
View(Example_DataYoung)

Example_DataYoungMales<-Example_Data[(Example_Data$Age<25 & Example_Data$Gender==1),]
View(Example_DataYoungMales)
Example_DataYoungMalesvar123<-Example_Data[(Example_Data$Age<25 & Example_Data$Gender==1),c(1,2,3)]
View(Example_DataYoungMalesvar123)
Example_DataMale<-Example_Data[Example_Data$Gender==1,]
View(Example_DataMale)
Example_DataSUB<-subset(Example_Data,select=c(1,2,4)) #data with only variable 1 and 3
View(Example_DataSUB) 
Example_DataSUB[3,2]<-NA
Example_DataSUB[3,2]<-10
Example_DataSUB[3,]<-NA #delete an observation
######################################################################################
is.data.frame(Example_Data)
is.numeric(Example_Data$Age)
is.numeric(Example_Data$Gender)
is.factor(Example_Data$Gender)
Example_Data$Gender<-as.factor(Example_Data$Gender)
is.factor(Example_Data$Gender)
class(Example_Data$Gender)
class(Example_Data$Age)
######################################################################################
Example_Data$age_group[Example_Data$Age<22]<-1
Example_Data$age_group[Example_Data$Age>=22 & Example_Data$Age<25]<-2
Example_Data$age_group[Example_Data$Age>=25]<-3
View(Example_Data)

save(Example_Data, file = "Example_Data.RData")
#########################################################################################
summary(Age)
summary(Example_Data$Age)
mean(Example_Data$Age)
mean(Example_Data$Age,na.rm=TRUE)
median(Example_Data$Age,na.rm=TRUE)
is.na(Example_Data$Age)
table(is.na(Example_Data$Age))

sd(Example_Data$Age,na.rm=TRUE)   #If numeric is normally distributed
var(Example_Data$Age,na.rm=TRUE)  
IQR(Example_Data$Age,na.rm=TRUE)  #If numeric is not normally distributed

boxplot(Example_Data$Age) #for numeric
boxplot(Example_Data$Age,Example_Data$Weight_Kg)
hist(Example_Data$Age, col="red",xlab="Age",ylab="Frequencies",main="Histogram of Age")
qqnorm(Example_Data$Age)
qqline(Example_Data$Age, col = "blue", lwd = 2)
d <- density(Example_Data$Age, na.rm=T)  #density plot or distribution
plot(d) 

par(mfrow=c(1,3)) #several graphs in one
boxplot(Example_Data$Age)
hist(Example_Data$Age, col="red",xlab="Age",ylab="Frequencies",main="Histogram of Age")
qqnorm(Example_Data$Age)
qqline(Example_Data$Age, col ="blue", lwd = 2)

shapiro.test(Example_Data$Age) #Test for normality
#Kolmogorov smirnov test  for normality

t.test(Example_Data$Age, mu=25)#Ho: mu=25 One sample t-test HYPOTHESIS TESTING
??t.test
t.test(Example_Data$Age, mu=25,alternative = c("less"))#Ho: mu=25 One sample t-test HYPOTHESIS TESTING

dim(Example_Data)

#####################################################################################

table(Example_Data$Country_region)
Example_Data$Country_region <- factor(Example_Data$Country_region,
                                      levels = c(1,2,3,4,5,8),
                                      labels = c("North Africa", "East Africa", "Southern Africa", "West Africa", "Central Africa","No region"))  
table(Example_Data$Country_region)

#Ex: Label: favor_col (1=black 2=pink 3=orange 4=white)
par(mfrow=c(1,1))
pie(table(Example_Data$Country_region))
barplot(table(Example_Data$Country_region),horiz=F,main="Country region")

Gender_Table<-table(Example_Data$Gender)
binom.test(Gender_Table, p=0.5) #exact binomial test: Only when two categories

Country_region_Table<-table(Example_Data$Country_region)
pt <- c(.02,.02, .25, .20,.5, .01)
chisq.test(Country_region_Table,p=pt)
chisq.test(Country_region_Table) #Chisquare goodness-of-fit test: More than 2 categories allowed
########################################################################################################
tapply(Example_Data$Age, Example_Data$Last_12_months_Sickness, mean, na.rm=TRUE)
tapply(Example_Data$Age, Example_Data$Last_12_months_Sickness, median, na.rm=T)

with(Example_Data, tapply(Age, Last_12_months_Sickness, shapiro.test))
#######################################################################################

#Independant with 2 group
t.test(Age ~ Last_12_months_Sickness, Example_Data, var.equal=TRUE) #Numerical is normally distributed and categorical with 2 categories

#Independant with 3+ group
result_aov <- aov(Age ~ Field, data=Example_Data) #One way ANOVA Numerical is normally distributed and categorical with 3 or more categories
summary(result_aov)
model.tables(result_aov, "means")
TukeyHSD(result_aov)   #Tukey HSD post-hoc test

################################################################################
############### Numeric (NOT Normally distributed) and categorical

library(FSA)
Summarize(Age ~ Gender, data = Example_Data) 




library(lattice)
histogram(~ Age | Gender, data=Example_Data, layout=c(1,3))      #  Histograms for each group columns and rows of individual plots

boxplot(Age ~ Gender,data = Example_Data ,ylab="xlab",xlab="ylab")

#Independant with 2 group
wilcox.test(Example_Data$Age~Example_Data$Gender) # independent 2-group Mann-Whitney U Test when the numerical variable is not normally distributed and the categorical got only 2 categories. 

#Independant with 3+ group
kruskal.test(Example_Data$Age~Example_Data$Country_region) # # Kruskal Wallis Test One Way Anova by Ranks where y is numeric and A is a factor with more than 2 categories

############################################################################################

