library(dplyr)
library(readr)
library(ggplot2)
library(knitr)
covid <- read_csv("https://data.ct.gov/resource/bfnu-rgqt.csv")
View(covid)
head(covid)
colnames(covid)

covid <- covid %>% arrange(dateupdated)

# plot cases over time
covid_plot <- ggplot(covid, aes(dateupdated, totalcases, color = county)) +
  geom_line() + theme_classic() + xlab('')
covid_plot

# get the first date
beginDate <- first(covid$dateupdated)
beginDate

# formats the date 
format.Date(beginDate, '%m/%d/%Y')

# get the most recent date
lastDate <- last(covid$dateupdated)
lastDate

format.Date(lastDate, '%m/%d/%Y')

colnames(covid)

filteredData <- covid %>% filter(dateupdated == lastDate) 
filteredData

colnames(filteredData)
colnames(filteredData) <- c("Date Updated", "County Code", "County", 
                            "Total Cases", "Confirmed Cases", "Probable Cases",
                            "Total Case Rate", "Hospitalization", "Total Deaths",
                            "Confirmed Deaths", "Probable Deaths")

ggplot(data = filteredData, aes(x= County, y = `Total Cases`, fill = County)) +
  geom_bar(stat = "identity") + ggtitle("Total Cases with County") +
  theme(legend.position = "none") + labs(x = "County", y = "Total Cases")

kable(filteredData[1:8, ], caption = "Recent Cases")
