#
# Define all required packages in the following vector.
# 
# Make sure, that you keep the comma in front of each new line. 
# This is a better approach to let git identify what is actually new.
#

local({
  
  packages <- c(
    "reshape2"
  , "tidyr"
  , "data.table"
  , "caret"
  , "e1071"
  , "fasttime"
  , "plm"
  , "ggplot2"
  , "forecast"
  , "tseries"
  , "FSelector"
  , "svMisc"
  , "arulesViz"
  , "pscl"
  , "randomForest"
  , "openair"
  , "plyr"
  , "dplyr"
  , "xlsx"
  , "mRMRe"
  , "fastDummies"
  , "RWeka"
  , "MASS"
  , "car"
  , "quantregForest"
  , "vcd"
  , "MASS"
  )
  
  require.package <- function (package) {
    if (!package %in% installed.packages()) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
  
  for (package in packages) {
    require.package(package)
  }
})

source('03_modeling/utils.R')
