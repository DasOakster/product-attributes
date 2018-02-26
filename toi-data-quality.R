# Data Quality processing for Toiletries (TOI)
# A FALSE flag means the data field has failed the Data Quality test
# A TRUE flag means the data field has passed the Data Quality test
# A NA flag means the data field does not require the Data Quality test

# The script conducts 3 types of DQ test
# 1. Conformity - does the data conform to a predefined pattern or reference data set
# 2. Whole data Set rules - e.g. Products must have either a Pack Qty or a Size
# 3. Critical Success Factor test - CSF - these are web product types where a key attribute must be fully populated with no NAs


# The source data is imported and flags are created for the different tests

# Set up working directory, source directories and source scripts

# Select file

#file.to.cleanse <- "WIP/TOI_WIP_V9.csv"
file.to.cleanse <- "Original Data/TOI_Original.csv"

# Environment

e <- "Laptop" #'R Drive', 'C Drive'

if(e == 'Laptop') {
      
      setwd("D:/OneDrive/R Projects/product-attributes")
      toi.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/TOI/"
}

if(e == 'C Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      toi.dir <- "C:/Users/oakleya/Desktop/Data Cleanse/TOI/"
}

if(e == 'R Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      toi.dir <- "R:/Data Quality Reports/Data Cleanse/TOI/"
}

source("regular-expressions.R")
source("data-checking-functions.R")

#------------------------------------------------------------------------------------------------------------------------------

# Web Product Types listed require 100% completion for the attribute

Colour.Required <- read.csv(paste(toi.dir,"CSF Files/CSF_Colour.csv",sep = ""))
csf.toi.type.colour <- Colour.Required[,1]

Size.Required <- read.csv(paste(toi.dir,"CSF Files/CSF_Size.csv",sep = ""))
csf.toi.type.size <- Size.Required[,1]

#------------------------------------------------------------------------------------------------------------------------------
# Read in data set for scoring

toi.products <- read.csv(paste(toi.dir,file.to.cleanse,sep=""))


# Add columns to flag the check status of the product for size

# Format Checking Fields

toi.products$Size.Format <-NA
toi.products$Size.Format.Score <-0
toi.products$Pack.Qty.Format <- NA
toi.products$Pack.Qty.Format.Score <- 0
toi.products$Colour.Format <- NA
toi.products$Colour.Format.Score <- 0

# Required Fields

toi.products$Colour.Required <- NA
toi.products$Colour.Required.Score <- 0
toi.products$Size.Required <- NA
toi.products$Size.Required.Score <- 0

# Data Integrity Fields

toi.products$Title.Spelling <- NA
toi.products$Title.Spelling.Score <- 0
toi.products$Pack.Or.Size <-NA
toi.products$Pack.Or.Size.Score <- 0

# Title and Attribute Consistency Fields

toi.products$Title.Size <- NA
toi.products$Title.Size.Score <- 0
toi.products$Title.Pack.Qty <- NA
toi.products$Title.Pack.Qty.Score <- 0

toi.products$Title.Brand <- NA
toi.products$Title.Brand.Score <- 0
#------------------------------------------------------------------------------------------------------------------------------

#******************************************************************************************************************************
# Product Attribute Formats
#******************************************************************************************************************************

#  Attributes must conform to patterns defined by regular expressions or reference data

toi.products <- dq.score.colour.format(toi.products,check.colour)
toi.products <- dq.score.pack.qty.format(toi.products,check.pack.qty)
toi.products <- dq.score.size.format(toi.products,paste(check.size.all))

#******************************************************************************************************************************
#  Data Integrity
#******************************************************************************************************************************

toi.products <- dq.score.pack.or.size(toi.products)
toi.products <- dq.score.title.brand(toi.products)
toi.products <- dq.score.title.pack.qty(toi.products)
toi.products <- dq.score.title.size(toi.products)
toi.products <- dq.score.web.description(toi.products)

#******************************************************************************************************************************
#  Data Completeness for Critical Attributes
#******************************************************************************************************************************

toi.products <- dq.score.colour.required(toi.products)
toi.products <- dq.score.size.required(toi.products)

#------------------------------------------------------------------------------------------------------------------------------

dq.scores <- c("Colour.Format.Score",
                        "Size.Format.Score",
                        "Pack.Qty.Format.Score",
                        "Size.Required.Score",
                        "Colour.Required.Score",
                        "Pack.Or.Size.Score",
                        "Title.Size.Score",
                        "Title.Size.Score",
                        "Title.Brand.Score",
                        "Title.Spelling.Score")

toi.products$Data.Quality.Score <- 100 + rowSums(toi.products[,dq.scores]) 
toi.products$Data.Quality.Score[toi.products$Data.Quality.Score < 0] <- 0

#------------------------------------------------------------------------------------------------------------------------------

# Create output files:  Any data failing the test and the full WIP file with flags

output.cols <- c("PSA_1",
"PSA_2",
"Type",
"Article",
"Web.Description",
"Data.Quality.Score",
"Size",
"Size.Format",
"Size.Required",
"Title.Size",
"Pack.Qty",
"Pack.Qty.Format",
"Pack.Or.Size",
"Title.Pack.Qty",
"Colour",
"Colour.Format",
"Colour.Required",
"Title.Spelling",
"Brand",
"Title.Brand",
"Age",
"Assembly",
"Capacity",
"Coverage",
"Material",
"Model.Number",
"Power",
"Washable",
"Size.Format.Score",
"Pack.Qty.Format.Score",
"Colour.Format.Score",
"Size.Required.Score",
"Colour.Required.Score",
"Pack.Or.Size.Score",
"Title.Spelling.Score",
"Title.Size.Score",
"Title.Pack.Qty.Score",
"Title.Brand.Score"
)

write.csv(toi.products[,output.cols],paste(toi.dir,"TOI_DQ_Data.csv",sep = ""),row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------------------

rm(list= ls()[!(ls() %in% c("toi.products"))]) 