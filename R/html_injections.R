# inject lines of code into HTML document

library(stringr)

# from htmltools
HTML <- function (text, ...) 
{
  htmlText <- c(text, as.character(list(...)))
  htmlText <- paste(htmlText, collapse = " ")
  attr(htmlText, "html") <- TRUE
  class(htmlText) <- c("html", "character")
  htmlText
}


#### inject footer ####

# add footer section after final body tag
inject_code_after_body_tag <- function(file, include, tag="</body>") 
{
  lookbehind <- paste0("(?<=", tag, ")")    # "(?<=<body>)" or "(?<=</body>)"
  file.lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  include.lines <- readLines(include, warn = FALSE, encoding = "UTF-8")
  inc <- HTML(paste(include.lines, collapse = "\r\n"))
  
  l <- str_replace(file.lines,
                   perl(lookbehind),
                   inc)
  HTML(paste(l, collapse = "\r\n"))
}




#files <- c("index.html")   # currently only inject in index.html
# files <- list.files("./notes", pattern=".html", recursive = T, full.names = TRUE)
# files <- c("index.html", files)
files <- list.files(".", pattern = ".html")
for (f in files) {
  code <- inject_code_after_body_tag(file = f, include = "include/footer.html", tag="</body>")  
  writeLines(code, f)
}



#### inject navbar before body ####

for (f in files) {
  code <- inject_code_after_body_tag(file = f, include = "include/before_body.html", tag="<body>")  
  writeLines(code, f)
}



# str_replace("your  <!--  TAG1  --> stuff", 
#             "[.]*<!--[- ]*TAG1[- ]*-->[.]*",
#             "replacement")
