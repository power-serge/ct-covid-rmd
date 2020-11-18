library(dplyr)
library(readr)

covidnames_order <- c("Fairfield","Hartford","Litchfield","Middlesex","NewHaven","NewLondon","Tolland","Windham")
cnames <- c("Fairfield","Hartford","NewHaven","NewLondon","Litchfield","Middlesex","Tolland","Windham")
cpop <- c(943332,891720,854757,265206,180333,162436,150721,116782)
populations <- list(county = cnames,population = cpop) 
popsdf <- data.frame(populations$county,populations$population)
colnames(popsdf) <- c("county","population")

#*************************************************************************************#
#*************************************************************************************#
#                 CREATING DATA FRAME FOR EACH COUNTY USING NORMALIZED DATA
#*************************************************************************************#
#*************************************************************************************#
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

poppy <- function(df,populations){
for(i in 1:length(popsdf)){  
if(df$county == "Fairfield"){
  filtered <- df %>% filter(county == "Fairfield")
  popsdf$popsdf[i]
}
if(df$county == "Hartford"){
  filtered <- df %>% filter(county == "Hartford")
}
if(df$county == "Litchfield"){

  filtered <- df %>% filter(county == "Litchfield")
}
county.pop <- 100000  
}
}
poppy(covid)
#######################################################################################
#                                Fairfield
#######################################################################################
testingFarfield <- covid %>% filter(county == "Fairfield")
farpop <- 100000 / popsdf$population[1]
Farfieldper100 <- (testingFarfield$totalcases * farpop) %>% data.frame()
Farfieldper100
#######################################################################################
#                                Hartford
#######################################################################################
testingHartford<- covid %>% filter(county == "Hartford")
htfdpop <- 100000 / popsdf$population[2]
htfdper100 <- (testingHartford$totalcases * htfdpop) %>% data.frame()
htfdper100
#######################################################################################
#                                New Haven
#######################################################################################
testingNewhaven <- covid %>% filter(county == "New Haven")
nhpop <- 100000 / popsdf$population[3]
nhper100 <- (testingNewhaven$totalcases * nhpop) %>% data.frame()
nhper100
#######################################################################################
#                                New London
#######################################################################################
testingNewLondon <- covid %>% filter(county == "New London")
nlpop <- 100000 / popsdf$population[4]
nlper100 <- (testingNewLondon$totalcases * nlpop) %>% data.frame()
nlper100
#######################################################################################
#                                Litchfield     
#######################################################################################
testingLitchfield <- covid %>% filter(county == "Litchfield")
lfldpop <- 100000 / popsdf$population[5]
lfldper100 <- (testingLitchfield$totalcases * lfldpop) %>% data.frame()
lfldper100
#######################################################################################
#                                Middlesex
#######################################################################################
testingMiddlesex <- covid %>% filter(county == "Middlesex")
msexpop <- 100000 / popsdf$population[6]
msexper100 <- (testingMiddlesex$totalcases * msexpop) %>% data.frame()
msexper100
#######################################################################################
#                                Tolland
#######################################################################################
testingTolland <- covid %>% filter(county == "Tolland")
tlndpop <- 100000 / popsdf$population[7]
tlndper100 <- (testingTolland$totalcases * tlndpop) %>% data.frame()
tlndper100
#######################################################################################
#                                Windham
#######################################################################################
testingWindham <- covid %>% filter(county == "Windham")
wdhmpop <- 100000 / popsdf$population[8]
wdhmper100 <- (testingWindham$totalcases * wdhmpop) %>% data.frame()
wdhmper100

#*************************************************************************************#
#*************************************************************************************#
#                     CREATING COUNTY POPULATIONS DATA FRAME 
#*************************************************************************************#
#*************************************************************************************#

testcovid <- covid
testcovid <- testcovid[order(testcovid$county),]
testcovid

TC.norm.pop.df <- data.frame(Farfieldper100,htfdper100,lfldper100,
                             msexper100,nhper100,nlper100,
                             tlndper100,wdhmper100)
colnames(TC.norm.pop.df) <- covidnames_order

head(TC.norm.pop.df)
TC.pop.melted <- melt(TC.norm.pop.df)
colnames(TC.pop.melted) <- c("county","totalcases/per100k")
head(TC.pop.melted)
new.covid.data <- cbind(testcovid,TC.pop.melted$`totalcases/per100k`)
colnames(new.covid.data)[length(new.covid.data)] <- "totalcases/per100k"

new.covid.data %>%
  ggplot(aes(dateupdated,`totalcases/per100k` , fill = county)) + geom_line()+theme_classic()

cot <- ggplot(new.covid.data, aes(dateupdated, `totalcases/per100k`, color = county)) +
  geom_line() + theme_classic() + xlab('') + ylab('Total Cases / County') + ggtitle("")

ggplotly(cot, dynamicTicks = TRUE) %>% 
  config(displayModeBar = F) %>%
  layout(xaxis=list(autorange=TRUE, yaxis=list(autorange=TRUE)))
