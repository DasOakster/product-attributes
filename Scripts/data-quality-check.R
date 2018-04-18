
attribute.data.quality <- function(psa1) {

# Set up Working Directories, Source Scripts and Target files

      setwd("D:/OneDrive/Work Files/Wilko/Data Cleanse/SAP Extracts")
      
      source("D:/OneDrive/R Projects/web-data-quality/DQ Functions/regular-expressions.R")
      source("D:/OneDrive/R Projects/web-data-quality/DQ Functions/data-checking-functions.R")
      source("D:/OneDrive/R Projects/web-data-quality/DQ Functions/split-files.R")
      source("D:/OneDrive/R Projects/web-data-quality/DQ Functions/create-summary-columns.R")
      source("D:/OneDrive/R Projects/web-data-quality/DQ Functions/summarise-changes.R")
      source("D:/OneDrive/R Projects/web-data-quality/DQ Functions/dq-scoring.R")
      
      files.to.score <- list.files()
      
      
      
      
      
      csf.dir <- paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/",psa1,"/CSF Files/",sep = "")
      
   
# Read in data set for scoring
      
      psa1.update.data<- read.csv(paste(toi.dir,file.to.score,sep=""))
      psa1.source.data <- read.csv(paste(toi.dir,file.to.compare,sep=""))
      
# Create RegEx for Attribute Columns checks by PSA1
      
      
      if(psa1 == "TOI") {
            
            check.size <- paste(check.weight,
                                check.hygiene,
                                check.nappy,
                                check.bandage,
                                check.tissue,
                                check.measure,
                                check.fit,
                                sep="|")
            
      }
      
      else if(psa1 == "CLE") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                sep="|")
            
      }
      
      
      else if(psa1 == "DIS") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                sep="|")
            
      }
      
      else if(psa1 == "GAR") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                check.dimension,
                                check.fit,
                                check.engine.size,
                                check.electrical,
                                check.shoe.size,
                                sep="|")
            
      }
      
      else if(psa1 == "KIT") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                check.dimension,
                                check.fit,
                                check.drawer,
                                check.electrical,
                                sep="|")
            
      }
      
      else if(psa1 == "HOM") {
            
            check.size <- paste(check.weight,
                                check.measure,
                                check.dimension,
                                check.fit,
                                check.drawer,
                                check.bedding,
                                check.electrical,
                                sep="|")
            
      }
      
      else {
      
            check.size <- paste(check.weight,
                          check.measure,
                          check.dimension,
                          sep="|")
      
      }
      
      check.pack.qty <- check.pack.qty
      check.material <- check.material
      check.age <- check.age
      check.capacity <- check.capacity
      check.coverage <- check.coverage
      check.assembly <- check.assembly
      check.washable <- check.washable
      check.power <- check.power
      check.colour <- check.colour
      
      
      
# Read in Web Product Types that require 100% completion
      
      colour.required <<- read.csv(paste(csf.dir,"CSF_Colour.csv",sep = ""))[,1]
      size.required <<- read.csv(paste(csf.dir,"CSF_Size.csv",sep = ""))[,1]
      pack.qty.required <<- read.csv(paste(csf.dir,"CSF_PackQty.csv",sep = ""))[,1]
      age.required <<- read.csv(paste(csf.dir,"CSF_Age.csv",sep = ""))[,1]
      assembly.required <<- read.csv(paste(csf.dir,"CSF_Assembly.csv",sep = ""))[,1]
      material.required <<- read.csv(paste(csf.dir,"CSF_Material.csv",sep = ""))[,1]
      washable.required <<- read.csv(paste(csf.dir,"CSF_Washable.csv",sep = ""))[,1]
      coverage.required <<- read.csv(paste(csf.dir,"CSF_Coverage.csv",sep = ""))[,1]
      capacity.required <<- read.csv(paste(csf.dir,"CSF_Capacity.csv",sep = ""))[,1]
      power.required <<- read.csv(paste(csf.dir,"CSF_Power.csv",sep = ""))[,1]
      model.number.required <<- read.csv(paste(csf.dir,"CSF_ModelNumber.csv",sep = ""))[,1]

# Add columns to flag the check status of the product for size
      
# Format Checking Fields
      
      psa1.update.data$Size.Format <-NA
      psa1.update.data$Size.Format.Score <-0
      psa1.update.data$Pack.Qty.Format <- NA
      psa1.update.data$Pack.Qty.Format.Score <- 0
      psa1.update.data$Colour.Format <- NA
      psa1.update.data$Colour.Format.Score <- 0
      psa1.update.data$Material.Format <- NA
      psa1.update.data$Material.Format.Score <- 0
      psa1.update.data$Capacity.Format <- NA
      psa1.update.data$Capacity.Format.Score <- 0
      psa1.update.data$Coverage.Format <- NA
      psa1.update.data$Coverage.Format.Score <- 0
      psa1.update.data$Age.Format <- NA
      psa1.update.data$Age.Format.Score <- 0
      psa1.update.data$Assembly.Format <- NA
      psa1.update.data$Assembly.Format.Score <- 0
      psa1.update.data$Model.Number.Format <- NA
      psa1.update.data$Model.Number.Format.Score <- 0
      psa1.update.data$Power.Format <- NA
      psa1.update.data$Power.Format.Score <- 0
      psa1.update.data$Washable.Format <- NA
      psa1.update.data$Washable.Format.Score <- 0
      
# Required Fields
      
      psa1.update.data$Colour.Required <- NA
      psa1.update.data$Colour.Required.Score <- 0
      psa1.update.data$Size.Required <- NA
      psa1.update.data$Size.Required.Score <- 0
      psa1.update.data$Material.Required <- NA
      psa1.update.data$Material.Required.Score <- 0
      psa1.update.data$Power.Required <- NA
      psa1.update.data$Power.Required.Score <- 0
      psa1.update.data$Coverage.Required <- NA
      psa1.update.data$Coverage.Required.Score <- 0
      psa1.update.data$Capacity.Required <- NA
      psa1.update.data$Capacity.Required.Score <- 0
      psa1.update.data$Age.Required <- NA
      psa1.update.data$Age.Required.Score <- 0
      psa1.update.data$Assembly.Required <- NA
      psa1.update.data$Assembly.Required.Score <- 0
      psa1.update.data$Washable.Required <- NA
      psa1.update.data$Washable.Required.Score <- 0
      psa1.update.data$Model.Number.Required <- NA
      psa1.update.data$Model.Number.Required.Score <- 0
      psa1.update.data$Pack.Qty.Required <- NA
      psa1.update.data$Pack.Qty.Required.Score <- 0
      
# Data Integrity Fields
      
      psa1.update.data$Title.Spelling <- NA
      psa1.update.data$Title.Spelling.Score <- 0
      psa1.update.data$Pack.Or.Size <-NA
      psa1.update.data$Pack.Or.Size.Score <- 0
      
# Title and Attribute Consistency Fields
      
      psa1.update.data$Title.Size <- NA
      psa1.update.data$Title.Size.Score <- 0
      psa1.update.data$Title.Pack.Qty <- NA
      psa1.update.data$Title.Pack.Qty.Score <- 0
      psa1.update.data$Title.Brand <- NA
      psa1.update.data$Title.Brand.Score <- 0

#  Attributes must conform to patterns defined by regular expressions or reference data
      
      psa1.update.data<- dq.score.colour.format(psa1.update.data,check.colour)
      psa1.update.data<- dq.score.pack.qty.format(psa1.update.data,check.pack.qty)
      psa1.update.data<- dq.score.size.format(psa1.update.data,paste(check.size))
      psa1.update.data<- dq.score.assembly.format(psa1.update.data,check.assembly)
      psa1.update.data<- dq.score.age.format(psa1.update.data,check.age)
      psa1.update.data<- dq.score.capacity.format(psa1.update.data,check.capacity)
      psa1.update.data<- dq.score.coverage.format(psa1.update.data,check.coverage)
      psa1.update.data<- dq.score.power.format(psa1.update.data,check.power)
      psa1.update.data<- dq.score.washable.format(psa1.update.data,check.washable)
      psa1.update.data<- dq.score.material.format(psa1.update.data,check.material)

      psa1.update.data<- dq.score.pack.or.size(psa1.update.data)
      psa1.update.data<- dq.score.title.brand(psa1.update.data)
      psa1.update.data<- dq.score.title.pack.qty(psa1.update.data)
      psa1.update.data<- dq.score.title.size(psa1.update.data)
      psa1.update.data<- dq.score.web.description(psa1.update.data)
      
      psa1.update.data<- dq.score.colour.required(psa1.update.data)
      psa1.update.data<- dq.score.size.required(psa1.update.data)
      psa1.update.data<- dq.score.pack.required(psa1.update.data)
      psa1.update.data<- dq.score.assembly.required(psa1.update.data)
      psa1.update.data<- dq.score.age.required(psa1.update.data)
      psa1.update.data<- dq.score.modelnumber.required(psa1.update.data)
      psa1.update.data<- dq.score.material.required(psa1.update.data)
      psa1.update.data<- dq.score.coverage.required(psa1.update.data)
      psa1.update.data<- dq.score.capacity.required(psa1.update.data)
      psa1.update.data<- dq.score.power.required(psa1.update.data)
      psa1.update.data<- dq.score.washable.required(psa1.update.data)
      
# Create output files:  Any data failing the test and the full WIP file with flags
      
      psa1.update.data.columns <- colnames(psa1.update.data)
      score.cols <- c(psa1.update.data.columns[grepl("Score",psa1.update.data.columns)==TRUE])
      score.cols <- score.cols[!score.cols %in% "Title.Spelling.Score"]
      output.cols <- c(psa1.update.data.columns[grepl("Score",psa1.update.data.columns)==FALSE])
      
      psa1.update.data$Data.Quality.Score <- 100 + rowSums(psa1.update.data[,score.cols]) 
      psa1.update.data$Data.Quality.Score[psa1.update.data$Data.Quality.Score < 0] <- 0
      
      dq.data.file <- psa1.update.data[,output.cols]
      dq.score.file <- psa1.update.data[,c("PSA_1","PSA_2","Article","Web.Description","Type","Data.Quality.Score",score.cols)]
      
# Create Attribute Level Summary Flags for DQ Reports
      
      dq.score.file <- score.size(dq.score.file)
      dq.score.file <- score.pack.qty(dq.score.file)
      dq.score.file <- score.brand(dq.score.file)
      dq.score.file <- score.age(dq.score.file)
      dq.score.file <- score.assembly(dq.score.file)
      dq.score.file <- score.model(dq.score.file)
      dq.score.file <- score.colour(dq.score.file)
      dq.score.file <- score.capacity(dq.score.file)
      dq.score.file <- score.coverage(dq.score.file)
      dq.score.file <- score.material(dq.score.file)
      dq.score.file <- score.power(dq.score.file)
      dq.score.file <- score.washable(dq.score.file)
      
# Create Report Data for Attribute Graphs
      
attribute.report.data <- rbind(s, p, cr, m ,ag, ay, ca, co, mo, b, w, po)      

# Output Master DQ Files
      
      write.csv(dq.score.file,paste(wip.dir,"DQ_Scores.csv",sep = ""),row.names = FALSE)
      write.csv(dq.data.file,paste(wip.dir,"DQ_Data.csv",sep = ""),row.names = FALSE)
      write.csv(attribute.report.data,paste(wip.dir,"DQ_Report.csv",sep = ""),row.names = FALSE)

# Create update files
      
      #psa1.update.data <<- psa1.update.data
      #compare.attributes(psa1.source.data,psa1.update.data,psa1)

# Create Summary of Changes
      
      #count.changes(psa1)
}

