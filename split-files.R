split.files <- function() {
      
      toi.compare <- merge(toi.original,toi.products,by.x = "Article",by.y = "Article")
      toi.compare <- toi.compare[,c("PSA_1.x","PSA_2.x","Article","Web.Description.x","Size.x","Size.y")]
      toi.compare$change <- NA
      
      
      for(i in 1:NROW(toi.compare)) {
            
            original.size <- toi.compare$Size.x
            update.size <- toi.compare$Size.y
            
            
            if(is.na(original.size[i]) && !is.na(update.size[i])) {
                  
                  toi.compare$change[i] <- "Infill"
            }
            
            else if (is.na(original.size[i]) && is.na(update.size[i])) {
                  
                  toi.compare$change[i] <- "Keep"
                  
            }
            else if (!is.na(original.size[i]) && is.na(update.size[i])) {
                  
                  toi.compare$change[i] <- "Blank"     
            }
            
            
            else if (as.character(original.size[i]) == as.character(update.size[i])) {
                  
                  toi.compare$change[i] <- "Keep"     
            }
            
            else  {
                  
                  toi.compare$change[i] <- "Update"     
            }     
            
            
            
      }
      
      size.change <- subset(toi.compare,change == "Infill" | toi.compare$change == "Update")
      blanks <- subset(toi.compare,change == "Blank")
      keeps <- subset(toi.compare,change == "Keep")
      
      
}