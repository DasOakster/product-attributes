# Data Quality processing for Garden (GAR)
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

file.to.score <- "WIP/GAR WIP 7.csv"
file.to.compare <- "Original Data/GAR_Original.csv"

# Environment

e <- "Laptop" #'R Drive', 'C Drive'

if(e == 'Laptop') {
      
      setwd("D:/OneDrive/R Projects/product-attributes")
      wip.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/GAR/WIP/"
      csf.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/GAR/CSF Files/"
      gar.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/GAR/"
      }

if(e == 'C Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      wip.dir <- "C:/Users/oakleya/Desktop/Data Cleanse/GAR/"
}

if(e == 'R Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      wip.dir <- "R:/Data Quality Reports/Data Cleanse/GAR/"
}

source("regular-expressions.R")
source("data-checking-functions.R")
source("split-files.R")

# Read in data set for scoring

gar.products <- read.csv(paste(gar.dir,file.to.score,sep=""))
gar.original <- read.csv(paste(gar.dir,file.to.compare,sep=""))

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

gar.products$Size.Format <-NA
gar.products$Size.Format.Score <-0
gar.products$Pack.Qty.Format <- NA
gar.products$Pack.Qty.Format.Score <- 0
gar.products$Colour.Format <- NA
gar.products$Colour.Format.Score <- 0
gar.products$Material.Format <- NA
gar.products$Material.Format.Score <- 0
gar.products$Capacity.Format <- NA
gar.products$Capacity.Format.Score <- 0
gar.products$Coverage.Format <- NA
gar.products$Coverage.Format.Score <- 0
gar.products$Age.Format <- NA
gar.products$Age.Format.Score <- 0
gar.products$Assembly.Format <- NA
gar.products$Assembly.Format.Score <- 0
gar.products$Model.Number.Format <- NA
gar.products$Model.Number.Format.Score <- 0
gar.products$Power.Format <- NA
gar.products$Power.Format.Score <- 0
gar.products$Washable.Format <- NA
gar.products$Washable.Format.Score <- 0

# Required Fields

gar.products$Colour.Required <- NA
gar.products$Colour.Required.Score <- 0
gar.products$Size.Required <- NA
gar.products$Size.Required.Score <- 0
gar.products$Material.Required <- NA
gar.products$Material.Required.Score <- 0
gar.products$Power.Required <- NA
gar.products$Power.Required.Score <- 0
gar.products$Coverage.Required <- NA
gar.products$Coverage.Required.Score <- 0
gar.products$Capacity.Required <- NA
gar.products$Capacity.Required.Score <- 0
gar.products$Age.Required <- NA
gar.products$Age.Required.Score <- 0
gar.products$Assembly.Required <- NA
gar.products$Assembly.Required.Score <- 0
gar.products$Washable.Required <- NA
gar.products$Washable.Required.Score <- 0
gar.products$Model.Number.Required <- NA
gar.products$Model.Number.Required.Score <- 0
gar.products$Pack.Qty.Required <- NA
gar.products$Pack.Qty.Required.Score <- 0


# Data Integrity Fields

gar.products$Title.Spelling <- NA
gar.products$Title.Spelling.Score <- 0
gar.products$Pack.Or.Size <-NA
gar.products$Pack.Or.Size.Score <- 0

# Title and Attribute Consistency Fields

gar.products$Title.Size <- NA
gar.products$Title.Size.Score <- 0
gar.products$Title.Pack.Qty <- NA
gar.products$Title.Pack.Qty.Score <- 0

gar.products$Title.Brand <- NA
gar.products$Title.Brand.Score <- 0
#------------------------------------------------------------------------------------------------------------------------------

#******************************************************************************************************************************
# Product Attribute Formats
#******************************************************************************************************************************

#  Attributes must conform to patterns defined by regular expressions or reference data

gar.products <- dq.score.colour.format(gar.products,check.colour)
gar.products <- dq.score.pack.qty.format(gar.products,check.pack.qty)
gar.products <- dq.score.size.format(gar.products,paste(check.size.all))
gar.products <- dq.score.assembly.format(gar.products,check.assembly)
gar.products <- dq.score.age.format(gar.products,check.age)
gar.products <- dq.score.capacity.format(gar.products,check.capacity)
gar.products <- dq.score.coverage.format(gar.products,check.coverage)
gar.products <- dq.score.power.format(gar.products,check.power)
gar.products <- dq.score.washable.format(gar.products,check.washable)
gar.products <- dq.score.material.format(gar.products,check.material)

#******************************************************************************************************************************
#  Data Integrity
#******************************************************************************************************************************

gar.products <- dq.score.pack.or.size(gar.products)
gar.products <- dq.score.title.brand(gar.products)
gar.products <- dq.score.title.pack.qty(gar.products)
gar.products <- dq.score.title.size(gar.products)
gar.products <- dq.score.web.description(gar.products)

#******************************************************************************************************************************
#  Data Completeness for Critical Attributes
#******************************************************************************************************************************

gar.products <- dq.score.colour.required(gar.products)
gar.products <- dq.score.size.required(gar.products)
gar.products <- dq.score.pack.required(gar.products)
gar.products <- dq.score.assembly.required(gar.products)
gar.products <- dq.score.age.required(gar.products)
gar.products <- dq.score.modelnumber.required(gar.products)
gar.products <- dq.score.material.required(gar.products)
gar.products <- dq.score.coverage.required(gar.products)
gar.products <- dq.score.capacity.required(gar.products)
gar.products <- dq.score.power.required(gar.products)
gar.products <- dq.score.washable.required(gar.products)

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

gar.products$Data.Quality.Score <- 100 + rowSums(gar.products[,dq.scores]) 
gar.products$Data.Quality.Score[gar.products$Data.Quality.Score < 0] <- 0

#------------------------------------------------------------------------------------------------------------------------------

# Create output files:  Any data failing the test and the full WIP file with flags

df.columns <- colnames(gar.products)
output.cols <- c(df.columns[grepl("Score",df.columns)==FALSE])
dq.data.file <- gar.products[,output.cols]
#dq.data.file <- dq.data.file[,c(4,1,2,3,5,6,7,8,9,10,11,13,14,15,16,18,19,24,28,26,34,20,31,21,32,22,29,23,30,25,33,27,35,17,12)]

#------------------------------------------------------------------------------------------------------------------------------
write.csv(dq.data.file,paste(wip.dir,"GAR_DQ_Data.csv",sep = ""),row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------------------

rm(list= ls()[!(ls() %in% c("gar.products","gar.original","gar.compare","compare.attributes","split.files"))]) 
#rm(list=ls())

#------------------------------------------------------------------------------------------------------------------------------
#compare.attributes()
#------------------------------------------------------------------------------------------------------------------------------
#