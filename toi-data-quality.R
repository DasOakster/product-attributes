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

file.to.score <- "WIP/TOI_WIP.csv"
file.to.compare <- "Original Data/TOI_Original.csv"

# Environment

e <- "Laptop" #'R Drive', 'C Drive'

if(e == 'Laptop') {
      
      setwd("D:/OneDrive/R Projects/product-attributes")
      wip.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/TOI/WIP/"
      csf.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/TOI/CSF Files/"
      toi.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/TOI/"
      }

if(e == 'C Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      wip.dir <- "C:/Users/oakleya/Desktop/Data Cleanse/TOI/"
}

if(e == 'R Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      wip.dir <- "R:/Data Quality Reports/Data Cleanse/TOI/"
}

source("regular-expressions.R")
source("data-checking-functions.R")
source("split-files.R")

# Read in data set for scoring

toi.products <- read.csv(paste(toi.dir,file.to.score,sep=""))
toi.original <- read.csv(paste(toi.dir,file.to.compare,sep=""))

#------------------------------------------------------------------------------------------------------------------------------

# Web Product Types listed require 100% completion for the attribute

csf.colour <- paste(csf.dir,"CSF_Colour.csv",sep = "")
csf.size <- paste(csf.dir,"CSF_Size.csv",sep = "")
csf.material <- paste(csf.dir,"CSF_Material.csv",sep = "")
csf.power <- paste(csf.dir,"CSF_Power.csv",sep = "")
csf.packqty <- paste(csf.dir,"CSF_PackQty.csv",sep = "")
csf.modelnumber <- paste(csf.dir,"CSF_ModelNumber.csv",sep = "")
csf.age <- paste(csf.dir,"CSF_Age.csv",sep = "")
csf.assembly <- paste(csf.dir,"CSF_Assembly.csv",sep = "")
csf.capacity <- paste(csf.dir,"CSF_Capacity.csv",sep = "")
csf.coverage <- paste(csf.dir,"CSF_Coverage.csv",sep = "")
csf.washable <- paste(csf.dir,"CSF_Washable.csv",sep = "")


Colour.Required <- read.csv(csf.colour)
csf.type.colour <- Colour.Required[,1]

Size.Required <- read.csv(csf.size)
csf.type.size <- Size.Required[,1]

PackQty.Required <- read.csv(csf.packqty)
csf.type.pack <- PackQty.Required[,1]

Age.Required <- read.csv(csf.age)
csf.type.age <- Age.Required[,1]

Assembly.Required <- read.csv(csf.assembly)
csf.type.assembly <- Assembly.Required[,1]

Material.Required <- read.csv(csf.material)
csf.type.material <- Material.Required[,1]

Washable.Required <- read.csv(csf.washable)
csf.type.washable <- Washable.Required[,1]

Coverage.Required <- read.csv(csf.coverage)
csf.type.coverage <- Coverage.Required[,1]

Capacity.Required <- read.csv(csf.capacity)
csf.type.capacity <- Capacity.Required[,1]

Power.Required <- read.csv(csf.power)
csf.type.power <- Power.Required[,1]

ModelNumber.Required <- read.csv(csf.modelnumber)
csf.type.modelnumber <- ModelNumber.Required[,1]
#------------------------------------------------------------------------------------------------------------------------------


# Add columns to flag the check status of the product for size

# Format Checking Fields

toi.products$Size.Format <-NA
toi.products$Size.Format.Score <-0
toi.products$Pack.Qty.Format <- NA
toi.products$Pack.Qty.Format.Score <- 0
toi.products$Colour.Format <- NA
toi.products$Colour.Format.Score <- 0
toi.products$Material.Format <- NA
toi.products$Material.Format.Score <- 0
toi.products$Capacity.Format <- NA
toi.products$Capacity.Format.Score <- 0
toi.products$Coverage.Format <- NA
toi.products$Coverage.Format.Score <- 0
toi.products$Age.Format <- NA
toi.products$Age.Format.Score <- 0
toi.products$Assembly.Format <- NA
toi.products$Assembly.Format.Score <- 0
toi.products$Model.Number.Format <- NA
toi.products$Model.Number.Format.Score <- 0
toi.products$Power.Format <- NA
toi.products$Power.Format.Score <- 0
toi.products$Washable.Format <- NA
toi.products$Washable.Format.Score <- 0

# Required Fields

toi.products$Colour.Required <- NA
toi.products$Colour.Required.Score <- 0
toi.products$Size.Required <- NA
toi.products$Size.Required.Score <- 0
toi.products$Material.Required <- NA
toi.products$Material.Required.Score <- 0
toi.products$Power.Required <- NA
toi.products$Power.Required.Score <- 0
toi.products$Coverage.Required <- NA
toi.products$Coverage.Required.Score <- 0
toi.products$Capacity.Required <- NA
toi.products$Capacity.Required.Score <- 0
toi.products$Age.Required <- NA
toi.products$Age.Required.Score <- 0
toi.products$Assembly.Required <- NA
toi.products$Assembly.Required.Score <- 0
toi.products$Washable.Required <- NA
toi.products$Washable.Required.Score <- 0
toi.products$Model.Number.Required <- NA
toi.products$Model.Number.Required.Score <- 0
toi.products$Pack.Qty.Required <- NA
toi.products$Pack.Qty.Required.Score <- 0


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
toi.products <- dq.score.assembly.format(toi.products,check.assembly)
toi.products <- dq.score.age.format(toi.products,check.age)
toi.products <- dq.score.capacity.format(toi.products,check.capacity)
toi.products <- dq.score.coverage.format(toi.products,check.coverage)
toi.products <- dq.score.power.format(toi.products,check.power)
toi.products <- dq.score.washable.format(toi.products,check.washable)
toi.products <- dq.score.material.format(toi.products,check.material)

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
toi.products <- dq.score.pack.required(toi.products)
toi.products <- dq.score.assembly.required(toi.products)
toi.products <- dq.score.age.required(toi.products)
toi.products <- dq.score.modelnumber.required(toi.products)
toi.products <- dq.score.material.required(toi.products)
toi.products <- dq.score.coverage.required(toi.products)
toi.products <- dq.score.capacity.required(toi.products)
toi.products <- dq.score.power.required(toi.products)
toi.products <- dq.score.washable.required(toi.products)

#------------------------------------------------------------------------------------------------------------------------------

dq.scores <- c("Colour.Format.Score",
                        "Size.Format.Score",
                        "Pack.Qty.Format.Score",
                        "Size.Required.Score",
                        "Colour.Required.Score",
                        "Pack.Or.Size.Score",
                        "Material.Format.Score",
                        "Title.Size.Score",
                        "Title.Pack.Qty.Score",
                        "Title.Brand.Score",
                        "Title.Spelling.Score")

toi.products$Data.Quality.Score <- 100 + rowSums(toi.products[,dq.scores]) 
toi.products$Data.Quality.Score[toi.products$Data.Quality.Score < 0] <- 0

#------------------------------------------------------------------------------------------------------------------------------

# Create output files:  Any data failing the test and the full WIP file with flags

df.columns <- colnames(toi.products)
output.cols <- c(df.columns[grepl("Score",df.columns)==FALSE])
dq.data.file <- toi.products[,output.cols]
#dq.data.file <- dq.data.file[,c(4,1,2,3,5,6,7,8,9,10,11,13,14,15,16,18,19,24,28,26,34,20,31,21,32,22,29,23,30,25,33,27,35,17,12)]

#------------------------------------------------------------------------------------------------------------------------------
write.csv(dq.data.file,paste(wip.dir,"TOI_DQ_Data.csv",sep = ""),row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------------------

rm(list= ls()[!(ls() %in% c("toi.products","toi.original","toi.compare","compare.attributes","split.files","brand.files"))]) 
#rm(list=ls())

#------------------------------------------------------------------------------------------------------------------------------
compare.attributes()
#------------------------------------------------------------------------------------------------------------------------------
#