#******************************************************************************************************************************************
#
# Andy Oakley - Feb 2018
# These functions run data quality rules on data from Wilko.com
#
#******************************************************************************************************************************************

#------------------------------------------------------------------------------------------------------------------------------------------
# Pack.Qty Format 
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.pack.qty.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Pack.Qty[i], perl = TRUE) == TRUE || is.na(product.data$Pack.Qty[i])) {
                  
                  product.data$Pack.Qty.Format[i] <- "PASS"
                  product.data$Pack.Qty.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Pack.Qty.Format[i] <- "FAil"
                  product.data$Pack.Qty.Format.Score[i] <- dq.rule.2.score
            }
            
            
            
      }
      
 
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Colour Format
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.colour.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Colour[i], perl = TRUE) == TRUE || is.na(product.data$Colour[i])) {
                  
                  product.data$Colour.Format[i] <- "PASS"
                  product.data$Colour.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Colour.Format[i] <- "FAIL"
                  product.data$Colour.Format.Score[i] <- dq.rule.2.score
            }
            
      }
      
      return(product.data)
}


#------------------------------------------------------------------------------------------------------------------------------------------
# Size Format
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.size.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Size[i], perl = TRUE) == TRUE || is.na(product.data$Size[i])) {
                  
                  product.data$Size.Format[i] <- "PASS"
                  product.data$Size.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Size.Format[i] <- "FAIL"
                  product.data$Size.Format.Score[i] <- dq.rule.2.score
            }
            
      }
      
      return(product.data)
}


#------------------------------------------------------------------------------------------------------------------------------------------
# Data Integrity - Size or Pack.Qty must be populated
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.pack.or.size <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            
            if (is.na(product.data$Size[i]) && is.na(product.data$Pack.Qty[i])) {
                  
                  product.data$Pack.Or.Size[i] <- "FAIL"
                  product.data$Pack.Or.Size.Score[i] <- dq.rule.4.score
            }
            
            else {
                  
                  product.data$Pack.Or.Size[i] <- "PASS"
                  product.data$Pack.Or.Size.Score[i] <- 0
            }
            
      }
      
      return(product.data)
}


#------------------------------------------------------------------------------------------------------------------------------------------
# Data Integrity - Spelling and formatting issues in web descriptions
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.web.description <- function(product.data) {

for(i in 1:NROW(product.data)) {
      
      if(grepl(toi.web.desc.err,product.data$Web.Description[i],perl = TRUE) == TRUE) {
            
            product.data$Title.Spelling[i] <- "CHECK"
            product.data$Title.Spelling.Score[i] <- dq.rule.3.score
      }
      
      
      else {
            
            product.data$Title.Spelling[i] <- "PASS"
            product.data$Title.Spelling.Score[i] <- 0
      }
      
}

      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Data Integrity - Inconsistency between Web Description and Pack Qty
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.title.pack.qty <- function(product.data) {

      product.data$Title.Pack.Qty.Score <- 0
      product.data$Title.Pack.Qty <- "PASS"
      
      for(i in 1:NROW(product.data)) {
            
            if(grepl("[0-9]{1,3}pk$",product.data$Web.Description[i],perl = TRUE) == TRUE) {
                  
                  if(grepl("[0-9]{1,3}pk$",product.data$Pack.Qty[i],perl = TRUE) == TRUE) {
                        product.data$Title.Pack.Qty[i] <- "PASS"
                  }
                  
                  else {
                        product.data$Title.Pack.Qty[i] <- "FAIL"
                        product.data$Title.Pack.Qty.Score[i] <- -50
                  }
                  
            }
      }

      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Data Integrity - Inconsistency between Web Description and Size
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.title.size <- function(product.data) {

size.pattern <- "[0-9]{1,4}(\\.[0-9]{1,2})?(g|ml)$"
      
for(i in 1:NROW(product.data)) {
      
      size <- product.data$Size[i]
      
      # Ignore if NA
      if (is.na(product.data$Size[i])) {
            
            product.data$Title.Size.Score[i] <- 0
            product.data$Title.Size[i] <- "PASS"
      }
      
      # Check if values matched
      else if (grepl(size,product.data$Web.Description[i],perl = TRUE) == TRUE) {
            
            product.data$Title.Size[i] <- "PASS"
            product.data$Title.Size.Score[i] <- 0
      }
      
      #  Fail if title ends in ml, L, g, kg and the size attribute doesn't match
      

      
            else if (grepl(size.pattern,product.data$Web.Description[i],perl = TRUE) == TRUE && grepl(size,product.data$Web.Description[i],perl = TRUE) == FALSE){
            
            product.data$Title.Size[i] <- "FAIL"
            product.data$Title.Size.Score[i] <- -50
      }
      
      else {
            
            product.data$Title.Size[i] <- "PASS"
            product.data$Title.Size.Score[i] <- 0
      }
}
      return(product.data)
}


#------------------------------------------------------------------------------------------------------------------------------------------
# Data Integrity - Inconsistency between Web Description and Brand
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.title.brand <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            
            brand <- product.data$Brand[i]
            
            if (is.na(product.data$Brand[i])) {
                  
                  product.data$Title.Brand.Score[i] <- 50
                  product.data$Title.Brand[i] <- "FAIL"
            }
            
            else if (grepl(brand, product.data$Web.Description[i]) == TRUE) {
                  
                  product.data$Title.Brand[i] <- "PASS"
                  product.data$Title.Brand.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Title.Brand[i] <- "FAIL"
                  product.data$Title.Brand.Score[i] <- -50
            }
            
      }
      return(product.data)
}
      
#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Colour
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.colour.required <- function(product.data) {
            
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.toi.type.colour && is.na(product.data$Colour[i])) {
                  
                  product.data$Colour.Required[i] <- "FAIL"
                  product.data$Colour.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Colour.Required[i] <- "PASS"
                  product.data$Colour.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Size
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.size.required <- function(product.data) {
      
      for(i in 1:NROW(product.data)) {
            
            if (product.data$Type[i] %in% csf.toi.type.size && is.na(product.data$Size[i])) {
                  
                  product.data$Size.Required[i] <- "FAIL"
                  product.data$Size.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  product.data$Size.Required[i] <- "PASS"
                  product.data$Size.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#---------------------------------------------------------------------------------------------------------------------------------------------
dq.score.material.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Material[i], perl = TRUE) == TRUE || is.na(product.data$Material[i])) {
                  
                  product.data$Material.Format[i] <- "PASS"
                  product.data$Material.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Material.Format[i] <- "FAIL"
                  product.data$Material.Format.Score[i] <- dq.rule.2.score
            }
            
      }
      
      return(product.data)
}