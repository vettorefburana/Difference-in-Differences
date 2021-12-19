

.libPaths(new = "/packages")
library(markdown)

source("./R/CardKrueger.R")
rmarkdown::render("./R/Presentation.Rmd")