## check_packages.R

#Run this script to check for packages that the other R scripts will use. If missing, try to install.
#code borrowed from here:
#http://www.vikram-baliga.com/blog/2015/7/19/a-hassle-free-way-to-verify-that-r-packages-are-installed-and-loaded

#add new packages to the chain here
packages = c(
  "here", # absolute requirement always
  "knitr", # for processing quarto
  "readr","haven", # I/O
  "tidyverse","lubridate","broom","tidytext", "patchwork", #tidyverse and friend
  "modelsummary","gt", # for table output
  "lubridate",
  "tidytext","readr","stringr","tidyr","cld3","quanteda", "quanteda.textstats", "quanteda.textmodels", # for text analysis
  "quanteda.textplots", "igraph", "knitr", # for text visualization
  "ggvenn", "ggVennDiagram", # venn diagrams
  "here"
)

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
