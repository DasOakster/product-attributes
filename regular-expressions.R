# These Regular Expressions define the pattern matching for Wilko.com product attributes

# Regular Expressions for Weights

 check.weight <<- "^[0-9]{1,4}(\\.[0-9]{1,2})?(mg|g|kg|ml|L)$"
 
# Regular Expressions for Sizes 

 check.fit <<-"^One Size$|^Small$|^Medium$|^Large$|^X-Large$|^XX-Large|^XXX-Large$"
 check.hygiene <<- "^X-Small$|^Small$|^Normal$|^Mini$|^Regular$|^Super Medium$|^Super$|^Super Plus$|^Long$|^Extra Long$|^Extra Large$|^Thin$|^Ultra$|^Extra$|^Maxi$"
 check.nappy <<- "^Other$|^Size 1$|^Size 2$|^Size 3$|^Size 4$|^Size 4\\+$|^Size 5$|^Size 5\\+$|^Size 6$|^Size 6\\+$"
 check.bandage <<- "^Size C$|^Size D$|^Size E$|^Size F$|^Size G$"
 
 check.size <<- paste(check.fit,"|",check.hygiene,"|",check.nappy,"|",check.bandage,sep = "")
 
# Regular Expressions for Single Measures 
 
 check.measure <<- "^[0-9]{1,3}(\\.[0-9]{1,2})?(mm|cm|m|ft|in)$"
 
# Regular Expressions for Attributes
 
 check.colour <<- "Brown|Multi|Black|Blonde|Green|White|Cream|Grey|Silver|Wood|Red|Blue|Pink|Yellow|Metal|Clear|Orange|Purple|Gold"

 # Reguar Expressions for Dimensions of the same units
 
 check.dim <<- "^([0-9]{1,2}(\\.[0-9]{1})? x ){1,2}[0-9]{1,2}(\\.[0-9]{1})?(mm|cm|m|ft|in)$"

 # Reguar Expressions for Dimensions of different units
 
 check.dimx <<- "^[0-9]{1,2}(\\.[0-9]{1})?(mm|cm|m) x [0-9]{1,2}(\\.[0-9]{1})?(mm|cm|m|ft|in)$"
 
 # Regular Expressions for Ranges of same units
 
 check.range <<- "^[0-9]{1,2}-[0-9]{1,2}(mm|cm|m)$"

 check.dimension <<- paste(check.dim,"|",check.dimx,"|",check.range,sep = "")
 
 # Regular Expressions for Pack Qty
 
 check.pack.qty.pk <<- "^[0-9]{1,3}pk$"
 check.pack.qty.num <<- "^1$"
 
 check.pack.qty <- paste(check.pack.qty.num,"|",check.pack.qty.pk,sep = "")
                          
 check.size.all <- paste(check.size, "|",check.weight,"|", check.measure,"|",check.dimension,sep="")
 
 # Minor web title formatting errors

 toi.web.desc.err <<- "2in1|EDT|\\&|Bodywash|Eyedrops|Bathtime|Bodyspray|Dryskin|Footcare|Lipgloss|Lipliner|Facewash|Coolmint|Nailpolish|Supergel|Supershine|Hayfever|Footcare|Aquaproof|Cremepuff"
 
 # Regular Expressions for Material
 
 check.material.cat <- "^Wood$|^Metal$|^Paper$|^Ceramics$|^Fabric$|^Plastic$|^Other$"
 check.material.value <- "^Ash$|^Chipboard$|^Cork$|^Eucalyptus$|^MDF$|^Oak$|^Particleboard$|^Pine$|^Recycled Board$|^Softwood$|^Wicker$|^Willow$|^Aluminium$|^Brass$|^Chrome$|^Copper$|^Gold$|^Iron$|^Silver$|^Stainless Steel$|^Cardboard$|^Greyboard$|^Paper$|^China$|^Glass$|^Porcelain$|^Cotton$|^Elastane$|^Nylon$|^Polycotton$|^Polyester$|^Viscose$|^Acrylic$|^Fiberglass$|^Graphite$|^Latex$|^Melamine$|^PES$|^Polyamide$|^Polycarbonate$|^Polyethylene$|^Polypropylene$|^PVA$|^PVC$|^Vinyl$|^Bassine$|^Canvas$|^Coir$|^Faux Leather$|^Foam$|^Jute$|^Microfibre$|^Rubber$|"      
 
 check.material <- paste(check.material.cat,"|",check.material.value) 
 
#------------------------------------------------------------------------------------------------------------------------

# Data Quality Classification and Scoring

# An attribute that is deemed a Critical Success Factor for a product type is missing e.g. colour for duvet covers
dq.rule.1.desc <<- "Critical Attribute Missing" 
dq.rule.1.score <<- -100

# An attribute value does not conform to one of the predefned lists of reference data or the correct format
dq.rule.2.desc <<- "Inconsistent Attribute" 
dq.rule.2.score <<- - 50

# There may be a questionable term in the Web Title e.g. use of 'Bodywash' instead of 'Body Wash'
dq.rule.3.desc <<- "Check Web Title Description" 
dq.rule.3.score <<- 0

# The product must have either a Pack Qty or Size attribute completed
dq.rule.4.desc <<- "Pack Qty or Size Required" 
dq.rule.4.score <<- -100

# The product does not have a web title 
dq.rule.5.desc <<- "Web Title Missing" 
dq.rule.5.score <<- -100

# The brand in the Web Title is different to the Brand in the attribute field
dq.rule.6.desc <<- "Inconsistent Brand" 
dq.rule.6.score <<- -50

# The pack qty in the Web Title is different to the Pack Qty in the attribute field
dq.rule.7.desc <<- "Inconsistent Pack Qty" 
dq.rule.7.score <<- -50

# The size in the Web Title is different to the Size in the attribute field
dq.rule.8.desc <<- "Inconsistent Size" 
dq.rule.8.score <<- -50

#------------------------------------------------------------------------------------------------------------------------
