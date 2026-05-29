# This script will install any of the required packages that may not already exist
# in your R environment.
# 
# Please run this script **prior** to the workshop, as some of these packages can take
# several minutes to install

if(!requireNamespace("rmarkdown")){
  install.packages("rmarkdown")
}
if(!requireNamespace("tidyverse")){
  install.packages("tidyverse")
}
if(!requireNamespace("cowplot")){
  install.packages("cowplot")
}
if(!requireNamespace("scales")){
  install.packages("scales")
}
if(!requireNamespace("gt")){
  install.packages("gt")
}
if(!requireNamespace("glue")){
  install.packages("glue")
}
if(!requireNamespace("testthat")){
  install.packages("testthat")
}
if(!requireNamespace("readxl")){
  install.packages("readxl")
}
if(!requireNamespace("checkmate")){
  install.packages("checkmate")
}

# Check that installed packages all load without error.
# You may get some warning messages about conflicts/masking functions - 
# these are fine! We are only concerned with errors like 
# " Error in library(rmarkdown): there is no package called 'rmarkdown' "
library(rmarkdown)
library(tidyverse)
library(scales)
library(cowplot)
library(gt)
library(glue)
library(testthat)
library(readxl)
library(checkmate)
