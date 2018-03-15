

# Select file

files.to.score <- c("TEST_WIP.csv","TEST_Original.csv")
files.to.score.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/TEST/Files to Score/"

# Environment

setwd("D:/OneDrive/R Projects/product-attributes")

source("regular-expressions.R")
source("data-checking-functions.R")
source("split-files.R")


wip.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/TEST/WIP/"
csf.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/TEST/CSF Files/"
psa.dir <- "D:/OneDrive/Work Files/Wilko/Data Cleanse/TEST/"


for(i in 1:NROW(files.to.score)) {

# Read in data set for scoring

      product.data <- read.csv(paste(files.to.score.dir,files.to.score[i],sep=""))

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

product.data$Size.Format <-NA
product.data$Size.Format.Score <-0
product.data$Pack.Qty.Format <- NA
product.data$Pack.Qty.Format.Score <- 0
product.data$Colour.Format <- NA
product.data$Colour.Format.Score <- 0
product.data$Material.Format <- NA
product.data$Material.Format.Score <- 0
product.data$Capacity.Format <- NA
product.data$Capacity.Format.Score <- 0
product.data$Coverage.Format <- NA
product.data$Coverage.Format.Score <- 0
product.data$Age.Format <- NA
product.data$Age.Format.Score <- 0
product.data$Assembly.Format <- NA
product.data$Assembly.Format.Score <- 0
product.data$Model.Number.Format <- NA
product.data$Model.Number.Format.Score <- 0
product.data$Power.Format <- NA
product.data$Power.Format.Score <- 0
product.data$Washable.Format <- NA
product.data$Washable.Format.Score <- 0

# Required Fields

product.data$required.colour<- NA
product.data$Colour.Required.Score <- 0
product.data$Size.Required <- NA
product.data$Size.Required.Score <- 0
product.data$Material.Required <- NA
product.data$Material.Required.Score <- 0
product.data$Power.Required <- NA
product.data$Power.Required.Score <- 0
product.data$Coverage.Required <- NA
product.data$Coverage.Required.Score <- 0
product.data$Capacity.Required <- NA
product.data$Capacity.Required.Score <- 0
product.data$Age.Required <- NA
product.data$Age.Required.Score <- 0
product.data$Assembly.Required <- NA
product.data$Assembly.Required.Score <- 0
product.data$Washable.Required <- NA
product.data$Washable.Required.Score <- 0
product.data$Model.Number.Required <- NA
product.data$Model.Number.Required.Score <- 0
product.data$Pack.Qty.Required <- NA
product.data$Pack.Qty.Required.Score <- 0


# Data Integrity Fields

product.data$Title.Spelling <- NA
product.data$Title.Spelling.Score <- 0
product.data$Pack.Or.Size <-NA
product.data$Pack.Or.Size.Score <- 0

# Title and Attribute Consistency Fields

product.data$Title.Size <- NA
product.data$Title.Size.Score <- 0
product.data$Title.Pack.Qty <- NA
product.data$Title.Pack.Qty.Score <- 0

product.data$Title.Brand <- NA
product.data$Title.Brand.Score <- 0
#------------------------------------------------------------------------------------------------------------------------------

#******************************************************************************************************************************
# Product Attribute Formats
#******************************************************************************************************************************

#  Attributes must conform to patterns defined by regular expressions or reference data

product.data <- dq.score.colour.format(product.data,check.colour)
product.data <- dq.score.pack.qty.format(product.data,check.pack.qty)
product.data <- dq.score.size.format(product.data,paste(check.size.all))
product.data <- dq.score.assembly.format(product.data,check.assembly)
product.data <- dq.score.age.format(product.data,check.age)
product.data <- dq.score.capacity.format(product.data,check.capacity)
product.data <- dq.score.coverage.format(product.data,check.coverage)
product.data <- dq.score.power.format(product.data,check.power)
product.data <- dq.score.washable.format(product.data,check.washable)
product.data <- dq.score.material.format(product.data,check.material)

#******************************************************************************************************************************
#  Data Integrity
#******************************************************************************************************************************

product.data <- dq.score.pack.or.size(product.data)
product.data <- dq.score.title.brand(product.data)
product.data <- dq.score.title.pack.qty(product.data)
product.data <- dq.score.title.size(product.data)
product.data <- dq.score.web.description(product.data)

#******************************************************************************************************************************
#  Data Completeness for Critical Attributes
#******************************************************************************************************************************

product.data <- dq.score.colour.required(product.data)
product.data <- dq.score.size.required(product.data)
product.data <- dq.score.pack.required(product.data)
product.data <- dq.score.assembly.required(product.data)
product.data <- dq.score.age.required(product.data)
product.data <- dq.score.modelnumber.required(product.data)
product.data <- dq.score.material.required(product.data)
product.data <- dq.score.coverage.required(product.data)
product.data <- dq.score.capacity.required(product.data)
product.data <- dq.score.power.required(product.data)
product.data <- dq.score.washable.required(product.data)

#------------------------------------------------------------------------------------------------------------------------------

# Create output files:  Any data failing the test and the full WIP file with flags

df.columns <- colnames(product.data)
score.cols <- c(df.columns[grepl("Score",df.columns)==TRUE])
output.cols <- c(df.columns[grepl("Score",df.columns)==FALSE])

#------------------------------------------------------------------------------------------------------------------------------

product.data$Data.Quality.Score <- 100 + rowSums(product.data[,score.cols]) 
product.data$Data.Quality.Score[product.data$Data.Quality.Score < 0] <- 0

#------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------------------
dq.data.file <- product.data[,output.cols]
dq.score.file <- product.data[,c("PSA_1","PSA_2","Data.Quality.Score",score.cols)]

psa2.score <- aggregate(product.data$`Data.Quality.Score`,list(product.data$PSA_2),mean)
psa2.score <- cbind(psa2.score,aggregate(product.data$`Data.Quality.Score`,list(product.data$PSA_2),length))
psa2.score <- psa2.score[,c(1,2,4)]
colnames(psa2.score) <- c("PSA 2","Mean DQ Score", "Num Articles")
psa2.score$'Mean DQ Score' <- signif(psa2.score$'Mean DQ Score',2) 
psa2.score <- psa2.score[order(-psa2.score$`Mean DQ Score`),]

write.csv(dq.score.file,paste(files.to.score.dir,i,"_TEST_DQ_Scores.csv",sep = ""),row.names = FALSE)
write.csv(dq.data.file,paste(files.to.score.dir,i,"_TEST_DQ_Data.csv",sep = ""),row.names = FALSE)
write.csv(psa2.score,paste(files.to.score.dir,files.to.score[i],"_SCORE_Data.csv",sep = ""),row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------------------


}

#rm(list= ls()[!(ls() %in% c("files.to.score","product.data","psa1.original","psa.dir","TEST.compare","compare.attributes","split.files","brand.files"))]) 
