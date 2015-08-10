# install.packages("RWordPress", repos = "http://www.omegahat.org/R", type="source")

library(RWordPress)
options(WordpressLogin = c(markheckmann = '..,-fimgw'),
        WordpressURL = 'http://ryouready.wordpress.com/xmlrpc.php')

# process is shitty. use upload_imgur for images
setwd("/Users/mark/_mh/programming/R/r_teaching/writings/collection/colorize_png_pictograms/rmd")
knitr::knit2wp('colorize_png_pictograms.Rmd',         
        title = 'Using colorized PNG pictograms in R base plots', 
        shortcode=TRUE, publish=FALSE)
