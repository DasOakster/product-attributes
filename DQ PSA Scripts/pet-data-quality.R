# Data Quality processing for Pet (PET)
# A FAIL flag means the data field has failed the Data Quality test
# A PASS flag means the data field has passed the Data Quality test

# The script conducts 4 types of DQ test
# 1. Conformity tests - does the data conform to a predefined pattern or reference data set
# 2. Data integrity rules - e.g. Products must have either a Pack Qty or a Size
# 3. Critical Success Factor tests - CSF - these are web product types where a key attribute must be fully populated with no NAs
# 4. Checks on the consistency of the web description and attribute fields

# The source data is imported and flags are created for the different tests

#----------------------------------------------------------------------------------------------------------------------------------------

# Set up working directory, source directories and source scripts

# Select file

file.to.score <- "WIP/PET_WIP.csv"
file.to.compare <- "Original Data/PET_Original.csv"

# Environment

e <- "Laptop" #'R Drive' 'C Drive'

if(e == 'Laptop') {
      
      setwd("D:/OneDrive/R Projects/product-attributes")
      wip.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/PET/WIP/"
      csf.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/PET/CSF Files/"
      pet.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/PET/"
      }

if(e == 'C Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      wip.dir <- "C:/Users/oakleya/Desktop/Data Cleanse/PET/"
}

if(e == 'R Drive') {
      
      setwd("C:/Users/oakleya/Desktop/R Projects/wilko.com/Scripts")
      wip.dir <- "R:/Data Quality Reports/Data Cleanse/PET/"
}

source("regular-expressions.R")
source("data-checking-functions.R")
source("split-files.R")

#------------------------------------------------------------------------------------------------------------------------------
# Read in data set for scoring
#------------------------------------------------------------------------------------------------------------------------------

psa1.products <- read.csv(paste(pet.dir,file.to.score,sep=""))
psa1.original <- read.csv(paste(pet.dir,file.to.compare,sep=""))

#------------------------------------------------------------------------------------------------------------------------------
# Web Product Types listed in these files require 100% completion for the attribute
#------------------------------------------------------------------------------------------------------------------------------

csf.colour <- paste(csf.dir,"CSF_Colour.csv",sep = "")
Colour.Required <- read.csv(csf.colour)
csf.type.colour <- Colour.Required[,1]

csf.size <- paste(csf.dir,"CSF_Size.csv",sep = "")
Size.Required <- read.csv(csf.size)
csf.type.size <- Size.Required[,1]

csf.packqty <- paste(csf.dir,"CSF_PackQty.csv",sep = "")
PackQty.Required <- read.csv(csf.packqty)
csf.type.pack <- PackQty.Required[,1]

csf.age <- paste(csf.dir,"CSF_Age.csv",sep = "")
Age.Required <- read.csv(csf.age)
csf.type.age <- Age.Required[,1]

csf.assembly <- paste(csf.dir,"CSF_Assembly.csv",sep = "")
Assembly.Required <- read.csv(csf.assembly)
csf.type.assembly <- Assembly.Required[,1]

csf.material <- paste(csf.dir,"CSF_Material.csv",sep = "")
Material.Required <- read.csv(csf.material)
csf.type.material <- Material.Required[,1]

csf.washable <- paste(csf.dir,"CSF_Washable.csv",sep = "")
Washable.Required <- read.csv(csf.washable)
csf.type.washable <- Washable.Required[,1]

csf.coverage <- paste(csf.dir,"CSF_Coverage.csv",sep = "")
Coverage.Required <- read.csv(csf.coverage)
csf.type.coverage <- Coverage.Required[,1]

csf.capacity <- paste(csf.dir,"CSF_Capacity.csv",sep = "")
Capacity.Required <- read.csv(csf.capacity)
csf.type.capacity <- Capacity.Required[,1]

csf.power <- paste(csf.dir,"CSF_Power.csv",sep = "")
Power.Required <- read.csv(csf.power)
csf.type.power <- Power.Required[,1]

csf.modelnumber <- paste(csf.dir,"CSF_ModelNumber.csv",sep = "")
ModelNumber.Required <- read.csv(csf.modelnumber)
csf.type.modelnumber <- ModelNumber.Required[,1]

#------------------------------------------------------------------------------------------------------------------------------
# Add columns to flag the check status of the product for size
#------------------------------------------------------------------------------------------------------------------------------

# Format Check Fields
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

# Required Check Fields
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

# Data Integrity Check Fields
psa1.products$Pack.Or.Size <-NA
psa1.products$Pack.Or.Size.Score <- 0

# Title and Attribute Consistency Check Fields
psa1.products$Title.Spelling <- NA
psa1.products$Title.Spelling.Score <- 0
psa1.products$Title.Size <- NA
psa1.products$Title.Size.Score <- 0
psa1.products$Title.Pack.Qty <- NA
psa1.products$Title.Pack.Qty.Score <- 0
psa1.products$Title.Brand <- NA
psa1.products$Title.Brand.Score <- 0

#******************************************************************************************************************************
# Product Attribute Formats
#******************************************************************************************************************************
check.coverage <<- "[0-9]{1,3}(m²)"
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
write.csv(dq.data.file,paste(wip.dir,"PET_DQ_Data.csv",sep = ""),row.names = FALSE, quote = TRUE)
#------------------------------------------------------------------------------------------------------------------------------

rm(list= ls()[!(ls() %in% c("psa1.products","psa1.original","compare.attributes","split.files","brand.files"))]) 
#rm(list=ls())

#------------------------------------------------------------------------------------------------------------------------------
#compare.attributes(psa1.original,psa1.products,"PET")
#------------------------------------------------------------------------------------------------------------------------------
#