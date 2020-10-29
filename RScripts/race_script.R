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

data_online <- read_csv("https://data.ct.gov/api/views/7rne-efic/rows.csv?accessType=DOWNLOAD")
data <- read.csv("C:/Users/brian/OneDrive - Eastern Connecticut State University/Independent Study/COVID-19_Cases_and_Deaths_by_Race_Ethnicity.csv")

#############################################################################################
# STANDARDIZING THE DATA USING THE CARET PACKAGE FUNCTION PREPROCESS AND PREDICT
#############################################################################################

sData <- preProcess(data[1:9],method=c("center", "scale"))
sData
zdata <- predict(sData, data[1:9])

#############################################################################################
# LINE GRAPHS OF AGE ADJUSTED CASES AND DEATHS BY RACE OVER TIME
#############################################################################################

data$Date.updated <- as.Date(data$Date.updated)
zdata$Date.updated <- as.Date(zdata$Date.updated)

min <- as.Date("2020-05-25")
max <- NA

# Age adjusted case by race/ethnicity over time

don <- data %>%
        ggplot( aes(x= Date.updated, y=Age.adjusted.case.rate.per.100k, color=Race.ethnicity)) +
        geom_line() +
        ggtitle("Age adjusted case rate per 100k by Race/Ethnicity") + scale_x_date(limits = c(min,max)) +
        theme_classic() + 
        ylab("Age adjusted case rate per 100k") + xlab("Date") + labs(color="Race/Ethnicity")
don <- ggplotly(don)
don

# Age adjusted case by race/ethnicity over time

don <- data %>%
  ggplot( aes(x= Date.updated, y= Age.adjusted.death.rate.per.100k, color=Race.ethnicity)) +
  geom_line() +
  ggtitle("Age adjusted death rate per 100k by Race/Ethnicity") + scale_x_date(limits = c(min,max)) +
  theme_stata() + 
  ylab("Age adjusted death rate per 100k") + xlab("Date") + labs(color="Race/Ethnicity")
don <- ggplotly(don)
don

# Total Cases by Ethnicity over time
don <- data %>%
  ggplot( aes(x= Date.updated, y=Total.cases, color=Race.ethnicity)) +
  geom_line() +
  ggtitle("Age adjusted case rate per 100k by Race/Ethnicity") +
  theme_wsj() +
  ylab("Total Cases") + xlab("Date") + labs(color="Race/Ethnicity")
don <- ggplotly(don)
don

#############################################################################################
# DENSITY PLOTS OF AGE ADJUSTED CASES AND DEATHS BY RACE OVER TIME
#############################################################################################

# Density plot of Ethnicity vs Age Case Rate
g <- data %>%
  ggplot(aes(Age.adjusted.case.rate.per.100k))+
  geom_density(aes(fill= Race.ethnicity), alpha=0.8) + 
  labs(title="Density plot of Age adjusted case rate by Race/Ethnicity", 
       subtitle="Age adjusted case rate per 100k",
       caption="Source: https://data.ct.gov/",
       x="Age adjusted case rate per 100k ")+theme_clean()+ labs(fill = "Race/Ethnicity")
ggplotly(g)

# Density plot of Ethnicity vs Age Death Rate
g <- data %>%
  ggplot(aes(Age.adjusted.death.rate.per.100k))+
  geom_density(aes(fill= Race.ethnicity), alpha=0.8) + 
  labs(title="Density plot of Age adjusted death rate by Race/Ethnicity", 
       subtitle="Age adjusted death rate per 100k",
       caption="Source: https://data.ct.gov/",
       x="Age adjusted death rate per 100k ", 
       fill=("Race/Ethnicity")+ theme_classic())
ggplotly(g)

hispanic <- data[data$Race.ethnicity=="Hispanic",]
white <- data[data$Race.ethnicity=="NH White",]
black <- data[data$Race.ethnicity=="NH Black",]
Asianpacific <- data[data$Race.ethnicity=="NH Asian or Pacific Islander",]
fourrace <- rbind(hispanic,white,black,Asianpacific)

g <- fourrace %>%
  ggplot( aes(x=Race.ethnicity, y=Age.adjusted.death.rate.per.100k, fill=Race.ethnicity)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot with Jitter: Age adjusted death rate vs Race/Ethnicity") + 
  xlab("Race/Ethnicity")+ylab("Age adjusted death rate per 100k")
g <- ggplotly(g)
g




#############################################################################################
# BOX PLOTS OF AGE ADJUSTED CASES AND DEATHS BY RACE OVER TIME
#############################################################################################

# g <- zdata %>%
#   ggplot(aes(Race.ethnicity, Total.cases)) +
# geom_boxplot(varwidth=T,fill = "plum") + 
#   labs(title="Box plot", 
#        subtitle="City Mileage grouped by Class of vehicle",
#        caption="Source: mpg",
#        x="Class of Vehicle",
#        y="City Mileage") + theme_ipsum()
# g


# g <- data %>%
#   ggplot(aes(Date.updated,Age.adjusted.death.rate.per.100k, size = Total.population, color = Race.ethnicity)) +
#   geom_point() +
#   scale_x_date() +
#   theme_bw() +
#   # gganimate specific bits:
#   labs(title = 'Month: {frame_time}', x = 'Date Updated', y = 'Age Adjusted Death Rate per 100k') +
#   transition_time(Date.updated) +
#   ease_aes('linear')
# Save at gif:
# anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")



# race.lm <- lm(data$Age.adjusted.death.rate.per.100k ~ movies$budget)
# q9_lm
# y.int <- q9_lm$coefficients[1]/1000000
# slope <- round(q9_lm$coefficients[2],2)
# 
# # create a string containing the equation of the regression line
# equation <- paste0('gross = ', y.int, ' + ', slope, '(budget)') 
# 
# ggplot(movies, aes(budget_millions, gross_millions)) + 
#   geom_point() +
#   theme_linedraw() + xlab('Budget, millions of dollars') + 
#   ylab ('Gross, millions of dollars') + 
#   ggtitle('Budget vs gross earnings') +
#   annotate(geom="text", x = 0, xmin=50, y=650, 
#            label= equation, color="blue", size = 7, hjust = 0) + 
#   geom_smooth(method = 'lm')

# g <- data %>%
#   ggplot(aes(fill= Race.ethnicity, y=Age.adjusted.case.rate.per.100k, x=Race.ethnicity)) + 
#   geom_bar(position="dodge", stat="identity") +
#   ggtitle("Studying 4 species..") +
#   facet_wrap(~ Race.ethnicity) +
#   theme_ipsum() +
#   theme(legend.position="none") +
#   xlab("")
# g

# g <- fourrace %>%
#   ggplot(aes(x = Race.ethnicity , y = Age.adjusted.case.rate.per.100k,color = Race.ethnicity)) +
#   geom_quasirandom(alpha = 0.7,
#                    size = 1.5) + 
#    labs(title = "Age adjusted case rate by Race/Ethnicity", 
#        x = "",
#        y = "Age adjusted case rate per 100k") +
#   theme_minimal() +
#   theme(legend.position = "none")
# g <- ggplotly(g)
# g



# Stacked + percent

# 
# fourrace
# 
# g <- data %>%
#   ggplot(aes(fill= Race.ethnicity, y=Age.adjusted.case.rate.per.100k, x=Race.ethnicity)) +
#   geom_bar(position="dodge", stat="identity") +
#   ggtitle("Studying 4 species..") +
#   facet_wrap(~ Race.ethnicity) +
#   theme_ipsum() +
#   theme(legend.position="none") +
#   xlab("")
# g
# 
# mtcars, barplot(ftable(vs, am, gear), beside = TRUE)



