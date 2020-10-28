# This script uses relative paths !!!
# Make sure your working directory is set to location of this source file

library(readr)
library(dplyr)

# reference to data and html dirs and extension value
datadir <- "../data"
htmldir <- "../docs"
ext     <- ".html"

# create the data directory if it doesn't exist
if (!file.exists(datadir)){
  dir.create(datadir)
}

# create the html directory if it doesn't exist
if (!file.exists(htmldir)){
  dir.create(htmldir)
}

# reference to the county pages template
template  <- "../RMarkdown/Template.Rmd"

# reference to the Home page template
home  <- "../RMarkdown/Home.Rmd"

# get the data
covid <- read_csv("https://data.ct.gov/resource/bfnu-rgqt.csv")

# make sure data is in the right order
covid <- covid %>% arrange(dateupdated)

# save data in a file
save(covid, file = paste(datadir, "/covid.RData", sep = ""))

# build the home page file name and path
homepage <- paste(htmldir, "/", "Home", ext, sep="")

# build the Home page
rmarkdown::render(home, output_file = homepage, params = list(devp = "n"))

# get distinct counties
counties <- unique(covid$county)

# build the county pages
for (i in counties) {

  # build the file name and path
  htmlfile <- paste(htmldir, "/", i, ext, sep="")
  
  #print(htmlfile)
  
  # render the document, to suppress under construction message use devp="n"
  rmarkdown::render(template, output_file = htmlfile, params = list(data = i, devp = "n"))
}
