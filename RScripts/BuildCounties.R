# This script uses relative paths !!!
# Make sure your working directory is set to location of this source file

library(readr)
library(dplyr)

# reference to data and html dirs and extension value
datadir <- "../data"
htmldir <- "../html"
ext     <- ".html"

# create the data directory if it doesn't exist
if (!file.exists(datadir)){
  dir.create(datadir)
}

# create the html directory if it doesn't exist
if (!file.exists(htmldir)){
  dir.create(htmldir)
}

# reference to the markdown template
template  <- "../RMarkdown/Template.Rmd"

# get the data
covid <- read_csv("https://data.ct.gov/resource/bfnu-rgqt.csv")

# make sure data is in the right order
covid <- covid %>% arrange(dateupdated)

# save data in a file
save(covid, file = paste(datadir, "/covid.RData", sep = ""))

# get distinct counties
counties <- unique(covid$county)

for (i in counties) {
  
  # build the file name and path
  htmlfile <- paste(htmldir, "/", i, ext, sep="")
  
  #print(htmlfile)
  
  # render the document
  rmarkdown::render(template, output_file = htmlfile, params = list(data = i))
}
