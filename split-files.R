# Function to create output files of changes by attribute
compare.attributes <- function(original.df,updated.df,psa1) {
      
      attribute.file <- merge(original.df,updated.df,by.x = "Article",by.y = "Article")
      attributes <- c("Size",
                      "Colour",
                      "Pack.Qty",
                      "Material",
                      "Assembly",
                      "Washable",
                      "Power",
                      "Capacity",
                      "Coverage",
                      "Age",
                      "Model.Number",
                      "Web.Description")
      
      attribute.file$Change <- NA

      for(a in 1:NROW(attributes)) {
      
            # Create a df with the two attribute columns to be compared
            attribute <- attributes[a]
            attribute.x <- paste(attributes[a],".x",sep = "")
            attribute.y <- paste(attributes[a],".y",sep = "")
            attribute.compare <- attribute.file[,c("PSA_1.x","PSA_2.x","Article","Web.Description.x",attribute.x,attribute.y,"Change")]

            for(i in 1:NROW(attribute.compare)) {
            
                  original.value <- as.character(attribute.compare[i,attribute.x])
                  update.value <- as.character(attribute.compare[i,attribute.y])
                  
                  # Orignal NA replaced by a value
                  if(is.na(original.value) && !is.na(update.value)) { 
                        
                        attribute.compare$Change[i] <- "Infill"
                  }
                  
                  # Original and Updated are both NA
                  else if (is.na(original.value) && is.na(update.value)) { 
                        
                        attribute.compare$Change[i] <- "Keep"
                        
                  }
                  
                  # Original has a value but update removes it
                  else if (!is.na(original.value) && is.na(update.value)) {
                        
                        attribute.compare$Change[i] <- "Blank"     
                  }
                  
                  # Original and update are the same value
                  else if (as.character(original.value) == as.character(update.value)) {
                        
                        attribute.compare$Change[i] <- "Keep"     
                  }
                  
                  # Otherwise update the value
                  else  {
                        
                        attribute.compare$Change[i] <- "Update"     
                  }     
                  
                  
            } # End sub-loop
      
      # Split files by PSA2 and save them as CSV files
      split.files(attribute.compare,psa1.products,attribute,psa1)
      # Split file by PSA2 and save the ones requiring a Brand check as CSV files
      brand.files(psa1)
            
      } # End main loop

} # End function


split.files <- function(df1, df2, colatr,psa1) {
      
      psa2 <- unique(df1$PSA_2.x)
      
      for(q in 1:NROW(psa2)) {
            
            x <- psa2[q]
            psa2.file <- subset(df1, PSA_2.x == x)
            update.file <- subset(psa2.file,Change ==  "Infill" | Change == "Update" | Change == "Blank")
            check.file <- subset(df2,Title.Spelling == "CHECK" | Title.Brand == "CHECK" | Title.Size == "CHECK" | Title.Pack.Qty == "CHECK")
            check.file <- subset(check.file[,c("PSA_1","PSA_2","Article","Web.Description","Brand","Size","Pack.Qty","Title.Spelling","Title.Brand","Title.Size","Title.Pack.Qty")])
            
            psa1.dir <- paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/",psa1, "/",sep = "")
            psa2.dir <- paste(psa1.dir,"PSA2/",x, sep = "")
            #setwd("D:/OneDrive/R Projects/product-attributes/TOI/")
            
            if(!dir.exists(psa2.dir)) {
            
                        dir.create(psa2.dir,showWarnings = FALSE)
            }                  
                  
            setwd(psa2.dir)
            update.cases <- NROW(update.file)
            check.cases <- NROW(check.file)
            
            if(update.cases > 0) write.csv(update.file,paste(x,"_Update_",colatr," (",update.cases, ")",".csv",sep = ""),row.names = FALSE)
            if(check.cases > 0) write.csv(check.file,paste(x,"_Check_Web_Description (",check.cases, ")",".csv",sep = ""),row.names = FALSE)
    
            setwd("..")
      }
      
}

brand.files <- function(psa1) {
      psa2 <- unique(psa1.products$PSA_2)
      psa1.dir <- paste("D:/OneDrive/Work Files/Wilko/Data Cleanse/",psa1,"/",sep = "")
      
      for(i in 1:NROW(psa2)) {
            
            brand.issue <- subset(psa1.products,(PSA_2 == psa2[i] & Title.Brand == "FAIL"))
            message(paste(psa1.dir,"PSA2/",psa2[i],sep = ""))
            setwd(paste(psa1.dir,"PSA2/",psa2[i],sep = ""))
            num.cases <- NROW(brand.issue)
            brand.issue <- brand.issue[,c("PSA_1","PSA_2","Article","Web.Description","Brand")]
            write.csv(brand.issue,paste(psa2[i],"_Check_Brand_","(",num.cases,").csv",sep = ""),row.names = FALSE)
            
      }
}

