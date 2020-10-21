# This script uses relative paths !!!
# Make sure your working directory is set to location of this source file

library(readr)
library(dplyr)

# reference to data and html dirs and extension value
htmldir <- "../docs"
ext     <- ".html"

# create the html directory if it doesn't exist
if (!file.exists(htmldir)){
  dir.create(htmldir)
}

# reference to the markdown template
template  <- "../RMarkdown/county.Rmd"


# build the file name and path
htmlfile <- paste(htmldir, "/", "county", ext, sep="")
  
#print(htmlfile)
  
# render the document, to suppress under construction message use devp="n"
rmarkdown::render(template, output_file = htmlfile, params = list(devp = "n"))
