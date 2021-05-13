### Starter script for analyzing the Eurobarometer Trend data (1970-2002).

### Important: For this script to work, make sure to first set your working directory
### to a directory containing the Eurobarometer Trend dataset,
### named "eurobarometer_trends.dta." You can obtain this from their website or download
### the data file here: https://www.dropbox.com/s/5bdhel8l7c5r59z/eurobarometer_trends.dta?dl=0

### Outline:
### 1. Identify variables of interest and clean them up
### 2. Estimate a regression model of what makes people turnout for EP elections

### Download packages we're going to use
# install.packages("ggplot2")
# install.packages("foreign")
library(ggplot2)
library(reshape2)
library(foreign)

### Read the dataset straight from the web, name the dataframe "data"
data <- read.dta("Data/eurobarometer_trends.dta")

### Variable "mediause" asks how much the respondent relies on the media.
summary(data$mediause) # Get a simple summary
data$mediause[data$mediause=="dk"]<-NA # Let's consider all the "dk" answerers as missing (NA)
data$mediause[data$mediause=="na"]<-NA # Let's consider all the "dk" as missing (NA)
data$mediause[data$mediause=="inap"]<-NA # Let's consider all the "inap" as missing (NA)
data$mediause<-factor(data$mediause) # this removes unused levels
levels(data$mediause)<-c("Very high", "High", "Low", "Very low") # Clean up the messy names of the levels
data$medianum<-as.numeric(data$mediause) # convert the categories to a numerical scale (cheating a bit!)
data$medianum<-(4 - data$medianum) # Subtract from 4 to make it more intuitive (higher number = more likely to vote)

### Variable "particip" asks respondent how likely they are to vote in the EP elections.
summary(data$particip)
data$particip[data$particip=="DK,NA"]<-NA
data$particip[data$particip=="depends"]<-NA
data$particip[data$particip=="inap"]<-NA
data$particip<-factor(data$particip)
levels(data$particip)<-c("Certainly yes", "Probably yes", "Probably not", "Certainly not")
data$participnum<-as.numeric(data$particip) # convert the categories to a numerical scale (cheating a bit!)
data$participnum<-(4 - data$participnum) # Subtract from 4 to make it more intuitive (higher number = more likely to vote)
summary(data$participnum)

### Variable "income" asks the respondent's income level.
summary(data$income)

### Variable "polint" asks the respondent's political interest generally.
summary(data$polint)
data$polint[data$polint=="DK,NA"]<-NA
data$polint[data$polint=="inap"]<-NA
data$polint<-factor(data$polint)
levels(data$polint)<-c("A great deal", "To some extent", "Not much", "Not at all")
data$polintnum<-as.numeric(data$polint) # convert the categories to a numerical scale (cheating a bit!)
data$polintnum<-(4 - data$polintnum) # Subtract from 4 to make it more intuitive (higher number = more likely to vote)
summary(data$polintnum)

### Variable "ecint3" asks the respondent's interest in EU politics.
summary(data$ecint3)
data$ecint3[data$ecint3=="DK, NA"]<-NA
data$ecint3[data$ecint3=="inap"]<-NA
data$ecint3<-factor(data$ecint3)
levels(data$ecint3)<-c("Very interested", "A little", "Not at all")
data$ecint3num<-as.numeric(data$ecint3) # convert the categories to a numerical scale (cheating a bit!)
data$ecint3num<-(3 - data$ecint3num) # Subtract from 4 to make it more intuitive (higher number = more likely to vote)
summary(data$ecint3num)

### Variable "ecint4" asks the respondent's interest in EU politics.
summary(data$ecint4)
data$ecint4[data$ecint4=="DK, NA"]<-NA
data$ecint4[data$ecint4=="inap"]<-NA
data$ecint4<-factor(data$ecint4)
levels(data$ecint4)<-c("A great deal", "To some extent", "Not much", "Not at all")
data$ecint4num<-as.numeric(data$ecint4) # convert the categories to a numerical scale (cheating a bit!)
data$ecint4num<-(4 - data$ecint4num) # Subtract from 4 to make it more intuitive (higher number = more likely to vote)
summary(data$ecint4num)

### Variable "relimp" asks the respondent's importance given to religion.
summary(data$relimp)
data$relimp[data$relimp=="DK,NA"]<-NA
data$relimp[data$relimp=="inap"]<-NA
data$relimp<-factor(data$relimp)
levels(data$relimp)<-c("Great", "Some", "Little")
summary(data$relimp)

data$relimpnum<-as.numeric(data$relimp) # convert the categories to a numerical scale (cheating a bit!)
data$relimpnum<-(3 - data$relimpnum) # Subtract from 4 to make it more intuitive (higher number = more likely to vote)
summary(data$relimpnum)

### Variable "nation1" captures the respondent's nation
summary(data$nation1)

### Make a subset for Great Britain only
gb<-subset(data, nation1=="GREAT BRITAIN")

### Variable "year" captures the year of the survey
summary(data$year)

### Convert country names to all caps for when we plot

data$nation1<-toupper(data$nation1)

### Aggregate the data at the country-year level

agg<-data[c("relimpnum", "nation1", "year")] # make a subset with only the religion variable

# Aggregate the mean importance of religion for each country in each year
attach(agg)
agg<-aggregate(relimpnum, by=list(nation1, year), FUN=mean, na.rm=TRUE)
detach(agg)

# Melt the data with nation and year as id variables (aggregate names them Group1 and Group2)
all.molten <- melt(agg,
                   id = c("Group.1", "Group.2"), na.rm=TRUE)

# Plot
ggplot(all.molten) +
  geom_line(aes(x=Group.2, y=value, colour=variable)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  facet_wrap(~Group.1)

# Re-plot cleaning things up
ggplot(subset(all.molten, Group.1!=c("PORTUGAL", "SPAIN"))) + # removing Portugal and Spain because they have no observations
  geom_line(aes(x=Group.2, y=value, colour=variable)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  facet_wrap(~Group.1) +
  labs(x="Year", y="Importance of Religion",
       title="Religion in European Countries, 1972-2002") + # Clean up axes and add a title
  theme(legend.position="none") # hide legend because there is only one

### Estimate a regression model of intention to turnout in Great Britain
model<-lm(participnum ~ income + polintnum + medianum + year, data=gb)

summary(model)
