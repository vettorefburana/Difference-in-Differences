

.libPaths(new = "/packages")
library(rmarkdown)

source("./R/CardKrueger.R")
rmarkdown::render("./R/Presentation.Rmd")