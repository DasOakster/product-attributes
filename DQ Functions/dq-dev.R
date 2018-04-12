
data.quality.reports <- function() {
      
      # Set up Working Directories, Source Scripts and Target files
      
      # Source files for Data Quality scoring and update file creation
      source("D:/OneDrive/R Projects/product-attributes/DQ Functions/regular-expressions.R")
      source("D:/OneDrive/R Projects/product-attributes/DQ Functions/data-checking-functions.R")
      source("D:/OneDrive/R Projects/product-attributes/DQ Functions/split-files.R")
      source("D:/OneDrive/R Projects/product-attributes/DQ Functions/create-summary-columns.R")
      source("D:/OneDrive/R Projects/product-attributes/DQ Functions/summarise-changes.R")
      source("D:/OneDrive/R Projects/product-attributes/DQ Functions/dq-scoring.R")
      
      # Source Directory for the SAP extracts
      setwd("D:/OneDrive/Work Files/Wilko/Data Cleanse/SAP Extracts")
      files.to.score <- list.files()
      
      # Target Directories for DQ Reporting Data Files
      attribute.report.dir <- "D:/OneDrive/Work Files/Wilko/Data Quality Reports/Data/Attribute Files/"
      score.report.dir <- "D:/OneDrive/Work Files/Wilko/Data Quality Reports/Data/Score Files/"
      summary.report.dir <- "D:/OneDrive/Work Files/Wilko/Data Quality Reports/Data/Summary Files/"
      flag.report.dir <- "D:/OneDrive/Work Files/Wilko/Data Quality Reports/Data/DQ Flag Files/"
      
      for(i in 1:NROW(files.to.score)) {
            
            # Identify PSA 1 and Set up CSF Directory
            psa1 <- substr(files.to.score[i],1,3)
            csf.dir <- paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/PSA Folders/",psa1,"/CSF Files/",sep = "")
            message(psa1)
            psa1.site.data <- read.csv(files.to.score[i])
            psa1.site.data <- psa1.site.data[,c(1:4,6,10:21)]
  
      
            # Allocate correct Regular Expressions for PSA1
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
            
            # Format Checking Fields
            psa1.site.data$Size.Format <-NA
            psa1.site.data$Size.Format.Score <-0
            psa1.site.data$Pack.Qty.Format <- NA
            psa1.site.data$Pack.Qty.Format.Score <- 0
            psa1.site.data$Colour.Format <- NA
            psa1.site.data$Colour.Format.Score <- 0
            psa1.site.data$Material.Format <- NA
            psa1.site.data$Material.Format.Score <- 0
            psa1.site.data$Capacity.Format <- NA
            psa1.site.data$Capacity.Format.Score <- 0
            psa1.site.data$Coverage.Format <- NA
            psa1.site.data$Coverage.Format.Score <- 0
            psa1.site.data$Age.Format <- NA
            psa1.site.data$Age.Format.Score <- 0
            psa1.site.data$Assembly.Format <- NA
            psa1.site.data$Assembly.Format.Score <- 0
            psa1.site.data$Model.Number.Format <- NA
            psa1.site.data$Model.Number.Format.Score <- 0
            psa1.site.data$Power.Format <- NA
            psa1.site.data$Power.Format.Score <- 0
            psa1.site.data$Washable.Format <- NA
            psa1.site.data$Washable.Format.Score <- 0
            
            # Required Fields
            psa1.site.data$Colour.Required <- NA
            psa1.site.data$Colour.Required.Score <- 0
            psa1.site.data$Size.Required <- NA
            psa1.site.data$Size.Required.Score <- 0
            psa1.site.data$Material.Required <- NA
            psa1.site.data$Material.Required.Score <- 0
            psa1.site.data$Power.Required <- NA
            psa1.site.data$Power.Required.Score <- 0
            psa1.site.data$Coverage.Required <- NA
            psa1.site.data$Coverage.Required.Score <- 0
            psa1.site.data$Capacity.Required <- NA
            psa1.site.data$Capacity.Required.Score <- 0
            psa1.site.data$Age.Required <- NA
            psa1.site.data$Age.Required.Score <- 0
            psa1.site.data$Assembly.Required <- NA
            psa1.site.data$Assembly.Required.Score <- 0
            psa1.site.data$Washable.Required <- NA
            psa1.site.data$Washable.Required.Score <- 0
            psa1.site.data$Model.Number.Required <- NA
            psa1.site.data$Model.Number.Required.Score <- 0
            psa1.site.data$Pack.Qty.Required <- NA
            psa1.site.data$Pack.Qty.Required.Score <- 0
            
            # Data Integrity Fields
            psa1.site.data$Title.Spelling <- NA
            psa1.site.data$Title.Spelling.Score <- 0
            psa1.site.data$Pack.Or.Size <-NA
            psa1.site.data$Pack.Or.Size.Score <- 0
            
            # Title and Attribute Consistency Fields
            psa1.site.data$Title.Size <- NA
            psa1.site.data$Title.Size.Score <- 0
            psa1.site.data$Title.Pack.Qty <- NA
            psa1.site.data$Title.Pack.Qty.Score <- 0
            psa1.site.data$Title.Brand <- NA
            psa1.site.data$Title.Brand.Score <- 0
            
            #  Attributes must conform to patterns defined by regular expressions or reference data
            psa1.site.data <- dq.score.colour.format(psa1.site.data,check.colour)
            psa1.site.data <- dq.score.pack.qty.format(psa1.site.data,check.pack.qty)
            psa1.site.data <- dq.score.size.format(psa1.site.data,paste(check.size))
            psa1.site.data <- dq.score.assembly.format(psa1.site.data,check.assembly)
            psa1.site.data <- dq.score.age.format(psa1.site.data,check.age)
            psa1.site.data <- dq.score.capacity.format(psa1.site.data,check.capacity)
            psa1.site.data <- dq.score.coverage.format(psa1.site.data,check.coverage)
            psa1.site.data <- dq.score.power.format(psa1.site.data,check.power)
            psa1.site.data <- dq.score.washable.format(psa1.site.data,check.washable)
            psa1.site.data <- dq.score.material.format(psa1.site.data,check.material)
            psa1.site.data <- dq.score.pack.or.size(psa1.site.data)
            psa1.site.data <- dq.score.title.brand(psa1.site.data)
            psa1.site.data <- dq.score.title.pack.qty(psa1.site.data)
            psa1.site.data <- dq.score.title.size(psa1.site.data)
            psa1.site.data <- dq.score.web.description(psa1.site.data)
            psa1.site.data <- dq.score.colour.required(psa1.site.data)
            psa1.site.data <- dq.score.size.required(psa1.site.data)
            psa1.site.data <- dq.score.pack.required(psa1.site.data)
            psa1.site.data <- dq.score.assembly.required(psa1.site.data)
            psa1.site.data <- dq.score.age.required(psa1.site.data)
            psa1.site.data <- dq.score.modelnumber.required(psa1.site.data)
            psa1.site.data <- dq.score.material.required(psa1.site.data)
            psa1.site.data <- dq.score.coverage.required(psa1.site.data)
            psa1.site.data <- dq.score.capacity.required(psa1.site.data)
            psa1.site.data <- dq.score.power.required(psa1.site.data)
            psa1.site.data <- dq.score.washable.required(psa1.site.data)
            
            # Create output files:  Any data failing the test and the full WIP file with flags
            
            psa1.site.data.columns <- colnames(psa1.site.data)
            score.cols <- c(psa1.site.data.columns[grepl("Score",psa1.site.data.columns)==TRUE])
            score.cols <- score.cols[!score.cols %in% "Title.Spelling.Score"]
            output.cols <- c(psa1.site.data.columns[grepl("Score",psa1.site.data.columns)==FALSE])
            
            psa1.site.data$Data.Quality.Score <- 100 + rowSums(psa1.site.data[,score.cols]) 
            psa1.site.data$Data.Quality.Score[psa1.site.data$Data.Quality.Score < 0] <- 0
            
            dq.data.file <- psa1.site.data[,output.cols]
            dq.score.file <- psa1.site.data[,c("PSA_1","PSA_2","Article","Web.Description","Type","Data.Quality.Score",score.cols)]
            
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
            
            write.csv(dq.score.file,paste(score.report.dir,psa1,"_Score.csv",sep = ""),row.names = FALSE)
            write.csv(dq.data.file,paste(flag.report.dir,psa1,"_Flag.csv",sep = ""),row.names = FALSE)
            write.csv(attribute.report.data,paste(attribute.report.dir,psa1,"_Attribute.csv",sep = ""),row.names = FALSE)
            
            }
      
      # Merge PSA Level files and Output Summary
      setwd(attribute.report.dir)
      attribute.files <- list.files()
      attribute.tables <- lapply(attribute.files, read.csv, header = TRUE)
      attribute.report <- do.call(rbind , attribute.tables)
      
      setwd(score.report.dir)
      score.files <- list.files()
      score.tables <- lapply(score.files, read.csv, header = TRUE)
      score.report <- do.call(rbind , score.tables)
      
      setwd(flag.report.dir)
      flag.files <- list.files()
      flag.tables <- lapply(flag.files, read.csv, header = TRUE)
      flag.report <- do.call(rbind , flag.tables)
      
      setwd(summary.report.dir)
      write.csv(attribute.report,"AttributeData.csv",row.names = FALSE)
      write.csv(score.report,"ScoreData.csv",row.names = FALSE)
      write.csv(flag.report,"FlagData.csv",row.names = FALSE)
      
      message("Process Completed")
      
      }