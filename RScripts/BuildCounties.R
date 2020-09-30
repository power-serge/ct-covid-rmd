# This script uses relative paths !!!
# Make sure your working directory is set to location of this source file

library(readr)
library(dplyr)

# reference to the markdown template
template  <- "../RMarkdown/Template.Rmd"

# reference to output directory and extension
htmldir <- "../html/"
ext <- ".html"

# get the data
covid <- read_csv("https://data.ct.gov/resource/bfnu-rgqt.csv")

# save data in a file
save(covid, file = "../data/covid.RData")

# make sure data is in the right order
covid <- covid %>% arrange(dateupdated)

# get distinct counties
counties <- unique(covid$county)
#counties <- c("New London", "New Haven")

for (i in counties) {
  
  # build the file name and path
  htmlfile <- paste(htmldir, i, ext, sep="")
  
  #print(htmlfile)
  
  # render the document
  rmarkdown::render(template, output_file = htmlfile, params = list(data = i))
}


