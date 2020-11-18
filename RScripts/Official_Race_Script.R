library(plotly)
library(hrbrthemes)
library(dplyr)
library(readr)
library(ggplot2)
library(readr)
library(MASS)
library(gridExtra)
library(psych)
library(Hmisc)
library(car)
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(ggthemes)
library(cowplot)
library(gganimate)
library(gapminder)
library(reprex)
# library(questionr)
library(scales)
library(reshape2)

data_online <- read_csv("https://data.ct.gov/api/views/7rne-efic/rows.csv?accessType=DOWNLOAD")
data <- read.csv("C:/Users/brian/OneDrive - Eastern Connecticut State University/Independent Study/COVID-19_Cases_and_Deaths_by_Race_Ethnicity.csv")

#############################################################################################
# STANDARDIZING THE DATA USING THE CARET PACKAGE FUNCTION PREPROCESS AND PREDICT
#############################################################################################

# sData <- preProcess(data[1:9],method=c("center", "scale"))
# sData
# zdata <- predict(sData, data[1:9])
# zdata$Date.updated <- as.Date(zdata$Date.updated)

#############################################################################################
# LINE GRAPHS OF AGE ADJUSTED CASES AND DEATHS BY RACE OVER TIME
#############################################################################################

data$Date.updated <- as.Date(data$Date.updated)


min <- as.Date("2020-05-25")
max <- NA

# Age adjusted case rate (aacr) by race/ethnicity over time

aaCr_race_ot <- data %>%
  ggplot( aes(x= Date.updated, y=Age.adjusted.case.rate.per.100k, color=Race.ethnicity)) +
  geom_line() +
  ggtitle("Age adjusted case rate per 100k by Race/Ethnicity") + scale_x_date(limits = c(min,max)) +
  theme_classic() + 
  ylab("Age adjusted case rate per 100k") + xlab("Date") + labs(color="Race/Ethnicity")
aaCr_race_ot <- ggplotly(aaCr_race_ot)
aaCr_race_ot

# Age adjusted death rate (aadr) by race/ethnicity over time

aaDr_race_ot <- data %>%
  ggplot( aes(x= Date.updated, y= Age.adjusted.death.rate.per.100k, color=Race.ethnicity)) +
  geom_line() +
  ggtitle("Age adjusted death rate per 100k by Race/Ethnicity") + scale_x_date(limits = c(min,max)) +
  theme_stata() + 
  ylab("Age adjusted death rate per 100k") + xlab("Date") + labs(color="Race/Ethnicity")
aaDr_race_ot <- ggplotly(aaDr_race_ot)
aaDr_race_ot

# Total Cases by Ethnicity over time

tc_race_ot <- data %>%
  ggplot( aes(x= Date.updated, y=Total.cases, color=Race.ethnicity)) +
  geom_line() +
  ggtitle("Total cases by Race/Ethnicity") +
  theme_wsj() +
  ylab("Total Cases") + xlab("Date") + labs(color="Race/Ethnicity")
tc_race_ot <- ggplotly(tc_race_ot)
tc_race_ot


#############################################################################################
# Processing data into new data frame in order to melt function for stacked race bar graph
#############################################################################################
newd <- data %>% select(Date.updated,Race.ethnicity,Age.adjusted.case.rate.per.100k,Age.adjusted.death.rate.per.100k)

# Two different ways to sort the data in decreasing order. rev = reverse 
sort(newd$Date.updated,decreasing = TRUE)
newd <- newd[rev(order(newd$Date.updated)),]
newdata <- newd %>% filter(Date.updated == Date.updated[1])
data1 <- newdata %>% select(Race.ethnicity,Age.adjusted.case.rate.per.100k,Age.adjusted.death.rate.per.100k)

data1$Race.ethnicity
dd <- data1 %>% filter(data1$Age.adjusted.case.rate.per.100k > 0)
dd <- data1 %>% filter(data1$Age.adjusted.death.rate.per.100k > 0)
NHwhite <- dd[1,2]/sum(dd[,2])# 0.1316019
NHblack <- dd[2,2]/sum(dd[,2])# 0.3622102
NHa/pi <- dd[3,2]/sum(dd[,2])
Hispanic <- dd[4,2]/sum(dd[,2])

props <- dd[,-1] %>% as.matrix() %>% prop.table(margin = 2)
df.race <- data.frame(Race = as.character(dd$Race.ethnicity), props)

colnames(df.race) <- c("Race", "Cases", "Deaths")

m <- melt(df.race, id.vars = "Race")

prop.aa.cases.deaths <- ggplot(m, aes(variable, value, fill = Race)) + geom_col() + 
  xlab("") + ylab("Proportion") + theme_classic() + ggtitle("Proportion of age adjusted case and death rates")
prop.aa.cases.deaths
ggplotly(prop.aa.cases.deaths, dynamicTicks = TRUE) %>% 
  config(displayModeBar = F) %>%
  layout(xaxis=list(autorange=TRUE, yaxis=list(autorange=TRUE)))

##################################################################################
##################################################################################
##################################################################################
#               EIDA_R LIBRARY LOAD AND PLOT FUNCTION
##################################################################################
##################################################################################
##################################################################################

library(devtools)
install_github('gdancik/eidaR')
#
library(eidaR)

df <- data.frame(gender = c('M', 'M', 'F','F'), group = c("A","B","A","B"),
                 count = c(3,3,3,6))

g <- ggplot(df, aes(x= gender, y = count, fill = group)) + geom_col() + 
  ggtitle('This is a title')

g %>% display_plot(formatDate = FALSE)
#

df <- data

plotRaceData <- function(df, racePanel = "Case rates") {
    selected <- 'Age.adjusted.case.rate.per.100k'
  ylab <- 'Age adjusted case rate'
  if (racePanel == 'Death rates') {
    selected <- 'Age.adjusted.death.rate.per.100k'
    ylab <- 'Age adjusted death rate'
  }
  txt <- paste0(selected, ': ')
  y <- df[[selected]]
  
  g1 <- ggplot(df, aes(x = Date.updated, y = y, color = Race.ethnicity)) +
    geom_line(aes(group = 1,
                  text = paste('date:', Date.updated, "<br>",
                               'race:', Race.ethnicity, "<br>",
                               txt, y)
    )) + labs(x = "Date", y = paste0(ylab, " (per 100,000)"),
              title = paste0(ylab," by race/ethnicity")) + theme_classic() + my_theme() 
  
  g1 %>% display_plot('text')
}

eida_Crate_ot <- plotRaceData(df, racePanel = "Case rates")
eida_Drate_ot <- plotRaceData(df, racePanel = "Death rates")
eida_Crate_ot
eida_Drate_ot
subplot(eida_Crate_ot,eida_Drate_ot,nrows = 1)
covid$dateupdated

        