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
            if (product.data$Type[i] %in% csf.type.colour && is.na(product.data$Colour[i])) {
                  
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
            
            if (product.data$Type[i] %in% csf.type.size && is.na(product.data$Size[i])) {
                  
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

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Material
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.material.required <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.type.material && is.na(product.data$Material[i])) {
                  
                  product.data$Material.Required[i] <- "FAIL"
                  product.data$Material.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Material.Required[i] <- "PASS"
                  product.data$Material.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Pack Qty
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.pack.required <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.type.pack && is.na(product.data$Pack.Qty[i])) {
                  
                  product.data$Pack.Qty.Required[i] <- "FAIL"
                  product.data$Pack.Qty.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Pack.Qty.Required[i] <- "PASS"
                  product.data$Pack.Qty.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Age
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.age.required <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.type.age && is.na(product.data$Age[i])) {
                  
                  product.data$Age.Required[i] <- "FAIL"
                  product.data$Age.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Age.Required[i] <- "PASS"
                  product.data$Age.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Power
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.power.required <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.type.power && is.na(product.data$Power[i])) {
                  
                  product.data$Power.Required[i] <- "FAIL"
                  product.data$Power.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Power.Required[i] <- "PASS"
                  product.data$Power.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Capacity
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.capacity.required <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.type.capacity && is.na(product.data$Capacity[i])) {
                  
                  product.data$Capacity.Required[i] <- "FAIL"
                  product.data$Capacity.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Capacity.Required[i] <- "PASS"
                  product.data$Capacity.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Coverage
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.coverage.required <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.type.coverage && is.na(product.data$Coverage[i])) {
                  
                  product.data$Coverage.Required[i] <- "FAIL"
                  product.data$Coverage.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Coverage.Required[i] <- "PASS"
                  product.data$Coverage.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Model Number
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.modelnumber.required <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.type.modelnumber && is.na(product.data$Model.Number[i])) {
                  
                  product.data$Model.Number.Required[i] <- "FAIL"
                  product.data$Model.Number.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Model.Number.Required[i] <- "PASS"
                  product.data$Model.Number.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Washable
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.washable.required <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.type.washable && is.na(product.data$Washable[i])) {
                  
                  product.data$Washable.Required[i] <- "FAIL"
                  product.data$Washable.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Washable.Required[i] <- "PASS"
                  product.data$Washable.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Completeness - Assembly
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.assembly.required <- function(product.data) {
      
      for (i in 1:NROW(product.data)) {
            if (product.data$Type[i] %in% csf.type.assembly && is.na(product.data$Assembly[i])) {
                  
                  product.data$Assembly.Required[i] <- "FAIL"
                  product.data$Assembly.Required.Score[i] <- dq.rule.1.score
            }
            
            
            else {
                  
                  product.data$Assembly.Required[i] <- "PASS"
                  product.data$Assembly.Required.Score[i] <- 0
            }
            
      }
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Assembly Format 
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.assembly.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Assembly[i], perl = TRUE) == TRUE || is.na(product.data$Assembly[i])) {
                  
                  product.data$Assembly.Format[i] <- "PASS"
                  product.data$Assembly.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Assembly.Format[i] <- "FAil"
                  product.data$Assembly.Format.Score[i] <- dq.rule.2.score
            }
            
            
            
      }
      
      
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Washable Format 
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.washable.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Washable[i], perl = TRUE) == TRUE || is.na(product.data$Washable[i])) {
                  
                  product.data$Washable.Format[i] <- "PASS"
                  product.data$Washable.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Washable.Format[i] <- "FAil"
                  product.data$Washable.Format.Score[i] <- dq.rule.2.score
            }
            
            
            
      }
      
      
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Material Format 
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.material.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Material[i], perl = TRUE) == TRUE || is.na(product.data$Material[i])) {
                  
                  product.data$Material.Format[i] <- "PASS"
                  product.data$Material.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Material.Format[i] <- "FAil"
                  product.data$Material.Format.Score[i] <- dq.rule.2.score
            }
            
            
            
      }
      
      
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Age Format 
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.age.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Age[i], perl = TRUE) == TRUE || is.na(product.data$Age[i])) {
                  
                  product.data$Age.Format[i] <- "PASS"
                  product.data$Age.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Age.Format[i] <- "FAil"
                  product.data$Age.Format.Score[i] <- dq.rule.2.score
            }
            
            
            
      }
      
      
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Power Format 
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.power.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Power[i], perl = TRUE) == TRUE || is.na(product.data$Power[i])) {
                  
                  product.data$Power.Format[i] <- "PASS"
                  product.data$Power.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Power.Format[i] <- "FAil"
                  product.data$Power.Format.Score[i] <- dq.rule.2.score
            }
            
            
            
      }
      
      
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Capacity Format 
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.capacity.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Capacity[i], perl = TRUE) == TRUE || is.na(product.data$Capacity[i])) {
                  
                  product.data$Capacity.Format[i] <- "PASS"
                  product.data$Capacity.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Capacity.Format[i] <- "FAil"
                  product.data$Capacity.Format.Score[i] <- dq.rule.2.score
            }
            
            
            
      }
      
      
      return(product.data)
}

#------------------------------------------------------------------------------------------------------------------------------------------
# Coverage Format 
#------------------------------------------------------------------------------------------------------------------------------------------

dq.score.coverage.format <- function(product.data, reg.ex) {
      
      for (i in 1:NROW(product.data)) {
            if (grepl(reg.ex, product.data$Coverage[i], perl = TRUE) == TRUE || is.na(product.data$Coverage[i])) {
                  
                  product.data$Coverage.Format[i] <- "PASS"
                  product.data$Coverage.Format.Score[i] <- 0
            }
            
            
            else {
                  
                  product.data$Coverage.Format[i] <- "FAil"
                  product.data$Coverage.Format.Score[i] <- dq.rule.2.score
            }
            
            
            
      }
      
      
      return(product.data)
}