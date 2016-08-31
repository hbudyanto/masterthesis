#########################################
## ========== BATCH
#########################################

# Enable Bellow Packages
require(data.table)
require(plyr)

# Set Appropriate Path to the link
setwd("~/PycharmProjects/dissertation/raw_data")

# Running the Whole Process
source('~/PycharmProjects/dissertation/src/processing_v0.R') #data cleansing
