
unlink("*.html")                  # delete all HTML files in root folder
rmarkdown::render('index.Rmd')    # render index.html in root folder
source("R/copy_files.R")          # copy all HTML files from its project folders into root folder
source("R/html_injections.R")     # add footer and navbar to all html files
