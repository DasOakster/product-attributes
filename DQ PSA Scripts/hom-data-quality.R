# Data Quality processing for Home (HOM)
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

file.to.score <- "WIP/HOM_WIP.csv"
file.to.compare <- "Original Data/HOM_Original.csv"

# Environment

e <- "Laptop" #'R Drive', 'C Drive'

if(e == 'Laptop') {
      
      setwd("D:/OneDrive/R Projects/product-attributes")
      wip.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/HOM/WIP/"
      csf.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/HOM/CSF Files/"
      hom.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/HOM/"
      }

if(e == 'C Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      wip.dir <- "C:/Users/oakleya/Desktop/Data Cleanse/HOM/"
}

if(e == 'R Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      wip.dir <- "R:/Data Quality Reports/Data Cleanse/HOM/"
}

source("regular-expressions.R")
source("data-checking-functions.R")
source("split-files.R")

# Read in data set for scoring

psa1.products <- read.csv(paste(hom.dir,file.to.score,sep=""))
psa1.original <- read.csv(paste(hom.dir,file.to.compare,sep=""))

#------------------------------------------------------------------------------------------------------------------------------

# Web Product Types listed require 100% completion for the attribute

colour.required <- read.csv(paste(csf.dir,"CSF_Colour.csv",sep = ""))[,1]
size.required <- read.csv(paste(csf.dir,"CSF_Size.csv",sep = ""))[,1]
pack.qty.required <- read.csv(paste(csf.dir,"CSF_PackQty.csv",sep = ""))[,1]
age.required <- read.csv(paste(csf.dir,"CSF_Age.csv",sep = ""))[,1]
assembly.required <- read.csv(paste(csf.dir,"CSF_Assembly.csv",sep = ""))[,1]
material.required <- read.csv(paste(csf.dir,"CSF_Material.csv",sep = ""))[,1]
washable.required <- read.csv(paste(csf.dir,"CSF_Washable.csv",sep = ""))[,1]
coverage.required <- read.csv(paste(csf.dir,"CSF_Coverage.csv",sep = ""))[,1]
capacity.required <- read.csv(paste(csf.dir,"CSF_Capacity.csv",sep = ""))[,1]
power.required <- read.csv(paste(csf.dir,"CSF_Power.csv",sep = ""))[,1]
model.number.required <- read.csv(paste(csf.dir,"CSF_ModelNumber.csv",sep = ""))[,1]
#------------------------------------------------------------------------------------------------------------------------------


# Add columns to flag the check status of the product for size

# Format Checking Fields

psa1.products$Size.Format <-NA
psa1.products$Size.Format.Score <-0
psa1.products$Pack.Qty.Format <- NA
psa1.products$Pack.Qty.Format.Score <- 0
psa1.products$Colour.Format <- NA
psa1.products$Colour.Format.Score <- 0
psa1.products$Material.Format <- NA
psa1.products$Material.Format.Score <- 0
psa1.products$Capacity.Format <- NA
psa1.products$Capacity.Format.Score <- 0
psa1.products$Coverage.Format <- NA
psa1.products$Coverage.Format.Score <- 0
psa1.products$Age.Format <- NA
psa1.products$Age.Format.Score <- 0
psa1.products$Assembly.Format <- NA
psa1.products$Assembly.Format.Score <- 0
psa1.products$Model.Number.Format <- NA
psa1.products$Model.Number.Format.Score <- 0
psa1.products$Power.Format <- NA
psa1.products$Power.Format.Score <- 0
psa1.products$Washable.Format <- NA
psa1.products$Washable.Format.Score <- 0

# Required Fields

psa1.products$Colour.Required <- NA
psa1.products$Colour.Required.Score <- 0
psa1.products$Size.Required <- NA
psa1.products$Size.Required.Score <- 0
psa1.products$Material.Required <- NA
psa1.products$Material.Required.Score <- 0
psa1.products$Power.Required <- NA
psa1.products$Power.Required.Score <- 0
psa1.products$Coverage.Required <- NA
psa1.products$Coverage.Required.Score <- 0
psa1.products$Capacity.Required <- NA
psa1.products$Capacity.Required.Score <- 0
psa1.products$Age.Required <- NA
psa1.products$Age.Required.Score <- 0
psa1.products$Assembly.Required <- NA
psa1.products$Assembly.Required.Score <- 0
psa1.products$Washable.Required <- NA
psa1.products$Washable.Required.Score <- 0
psa1.products$Model.Number.Required <- NA
psa1.products$Model.Number.Required.Score <- 0
psa1.products$Pack.Qty.Required <- NA
psa1.products$Pack.Qty.Required.Score <- 0


# Data Integrity Fields

psa1.products$Title.Spelling <- NA
psa1.products$Title.Spelling.Score <- 0
psa1.products$Pack.Or.Size <-NA
psa1.products$Pack.Or.Size.Score <- 0

# Title and Attribute Consistency Fields

psa1.products$Title.Size <- NA
psa1.products$Title.Size.Score <- 0
psa1.products$Title.Pack.Qty <- NA
psa1.products$Title.Pack.Qty.Score <- 0

psa1.products$Title.Brand <- NA
psa1.products$Title.Brand.Score <- 0
#------------------------------------------------------------------------------------------------------------------------------

#******************************************************************************************************************************
# Product Attribute Formats
#******************************************************************************************************************************

#  Attributes must conform to patterns defined by regular expressions or reference data

psa1.products <- dq.score.colour.format(psa1.products,check.colour)
psa1.products <- dq.score.pack.qty.format(psa1.products,check.pack.qty)
psa1.products <- dq.score.size.format(psa1.products,paste(check.size.all))
psa1.products <- dq.score.assembly.format(psa1.products,check.assembly)
psa1.products <- dq.score.age.format(psa1.products,check.age)
psa1.products <- dq.score.capacity.format(psa1.products,check.capacity)
psa1.products <- dq.score.coverage.format(psa1.products,check.coverage)
psa1.products <- dq.score.power.format(psa1.products,check.power)
psa1.products <- dq.score.washable.format(psa1.products,check.washable)
psa1.products <- dq.score.material.format(psa1.products,check.material)

#******************************************************************************************************************************
#  Data Integrity
#******************************************************************************************************************************

psa1.products <- dq.score.pack.or.size(psa1.products)
psa1.products <- dq.score.title.brand(psa1.products)
psa1.products <- dq.score.title.pack.qty(psa1.products)
psa1.products <- dq.score.title.size(psa1.products)
psa1.products <- dq.score.web.description(psa1.products)

#******************************************************************************************************************************
#  Data Completeness for Critical Attributes
#******************************************************************************************************************************

psa1.products <- dq.score.colour.required(psa1.products)
psa1.products <- dq.score.size.required(psa1.products)
psa1.products <- dq.score.pack.required(psa1.products)
psa1.products <- dq.score.assembly.required(psa1.products)
psa1.products <- dq.score.age.required(psa1.products)
psa1.products <- dq.score.modelnumber.required(psa1.products)
psa1.products <- dq.score.material.required(psa1.products)
psa1.products <- dq.score.coverage.required(psa1.products)
psa1.products <- dq.score.capacity.required(psa1.products)
psa1.products <- dq.score.power.required(psa1.products)
psa1.products <- dq.score.washable.required(psa1.products)

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

psa1.products$Data.Quality.Score <- 100 + rowSums(psa1.products[,dq.scores]) 
psa1.products$Data.Quality.Score[psa1.products$Data.Quality.Score < 0] <- 0

#------------------------------------------------------------------------------------------------------------------------------

# Create output files:  Any data failing the test and the full WIP file with flags

df.columns <- colnames(psa1.products)
output.cols <- c(df.columns[grepl("Score",df.columns)==FALSE])
dq.data.file <- psa1.products[,output.cols]
#dq.data.file <- dq.data.file[,c(4,1,2,3,5,6,7,8,9,10,11,13,14,15,16,18,19,24,28,26,34,20,31,21,32,22,29,23,30,25,33,27,35,17,12)]

#------------------------------------------------------------------------------------------------------------------------------
write.csv(dq.data.file,paste(wip.dir,"HOM_DQ_Data.csv",sep = ""),row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------------------

rm(list= ls()[!(ls() %in% c("psa1.products","psa1.original","hom.compare","compare.attributes","split.files","brand.files"))]) 
#rm(list=ls())

#------------------------------------------------------------------------------------------------------------------------------
#compare.attributes(psa1.original,psa1.products,"HOM")
#------------------------------------------------------------------------------------------------------------------------------
#