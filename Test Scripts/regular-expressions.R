# These Regular Expressions define the pattern matching for Wilko.com product attributes

# Regular Expressions for weights and volumes (a number up to 4 digits with optional decimal places to 3dp followed by a unit of measure)

reg.ex.weight <<- "^[0-9]{1,4}(\\.[0-9]{1,3})?(mg|g|kg|ml|L|oz|cl|pt|lb)$"

# Regular Expressions for Sizes (various sets of reference data or specific formats)

reg.ex.fit <<-"^One Size$|^Small$|^Medium$|^Large$|^X-Large$|^XX-Large|^XXX-Large$|^Assorted$"
reg.ex.hygiene <<- "^X-Small$|^Small$|^Normal$|^Mini$|^Regular$|^Super Medium$|^Super$|^Super Plus$|^Long$|^Extra Long$|^Extra Large$|^Thin$|^Ultra$|^Extra$|^Maxi$"
reg.ex.nappy <<- "^Other$|^Size 1$|^Size 2$|^Size 3$|^Size 4$|^Size 4\\+$|^Size 5$|^Size 5\\+$|^Size 6$|^Size 6\\+$"
reg.ex.bandage <<- "^Size C$|^Size D$|^Size E$|^Size F$|^Size G$"
reg.ex.shoe <<- "^Size 4|^Size 5|^Size 6$|^Size 7$|^Size 8$|^Size 9$|^Size 10$|^Size 11$"
reg.ex.engine <<- "^[0-9]{1,3}(\\.[0-9]{1})?(cc)$"
reg.ex.tissue <<- "^Regular$|^Extra Large$|^Pocket Pack$|^Mansize$|^Large$|"
reg.ex.electrical <<- "^[0-9]{1,4}W$|^[0-9]{1,2} Slice$"
reg.ex.crockery <<- "^[0-9]{1,2} Piece$|^[0-9]{1,2} Bottle$"
reg.ex.drawer <<- "^[0-9] Drawer$"

# Regular Expressions for colours

reg.ex.colour <<- "Brown|Multi|Black|Blonde|Green|White|Cream|Grey|Silver|Wood|Red|Blue|Pink|Yellow|Metal|Clear|Orange|Purple|Gold"

# Regular Expressions for Sizes (various sets of reference data or specific formats)

# Regular Expressions for a single dimension of length, height, diameter (a number up to 3 digits with optional decimal places to 2dp and a unit of length or height)

reg.ex.measure <<- "^[0-9]{1,3}(\\.[0-9]{1,2})?(mm|cm|m|ft|in)$"

# Reguar Expressions for dimensions of the same units 

reg.ex.dim <<- "^([0-9]{1,3}(\\.[0-9]{1})? x ){1,3}[0-9]{1,3}(\\.[0-9]{1})?(mm|cm|m|ft|in)$"

# Reguar Expressions for Dimensions of different units

reg.ex.dimx <<- "^[0-9]{1,2}(\\.[0-9]{1,2})?(mm|cm|m) x [0-9]{1,2}(\\.[0-9]{1})?(mm|cm|m|ft|in)$"

# Regular Expressions for ranges of same units

reg.ex.range <<- "^[0-9]{1,2}-[0-9]{1,2}(mm|cm|m)$"

# Regular Expressions for Pack Qty

reg.ex.pack.qty <<- "^[0-9]{1,3}$"

# Regular Expressions for Material

reg.ex.material.cat <<- "^Wood$|^Metal$|^Paper$|^Ceramic$|^Fabric$|^Plastic$|^Other$"
reg.ex.material.value <<- "^Reed$|^Bamboo$|^Ash$|^Chipboard$|^Cork$|^Eucalyptus$|^MDF$|^Oak$|^Particleboard$|^Pine$|^Recycled Board$|^Softwood$|^Wicker$|^Willow$|^Aluminium$|^Brass$|^Chrome$|^Copper$|^Gold$|^Iron$|^Silver$|^Stainless Steel$|^Cardboard$|^Greyboard$|^Paper$|^China$|^Glass$|^Porcelain$|^Cotton$|^Elastane$|^Nylon$|^Polycotton$|^Polyester$|^Viscose$|^Acrylic$|^Fiberglass$|^Graphite$|^Latex$|^Melamine$|^PES$|^Polyamide$|^Polycarbonate$|^Polyethylene$|^Polypropylene$|^PVA$|^PVC$|^Vinyl$|^Bassine$|^Canvas$|^Coir$|^Faux Leather$|^Foam$|^Jute$|^Microfibre$|^Rubber$|^Polythene$|^Coco Fibre$|^Fleece$|^Galvanised Steel$|^Steel$"      

reg.ex.material <<- paste(reg.ex.material.cat,"|",reg.ex.material.value,sep = "") 

# Regular Expressions for Washable

reg.ex.washable <<- "^Do Not Wash$|^Wash at 30$|^Wash at 40$|^Wash at 40 Synth$|^Wash at 40 Wool$|^Wash at 60$|^Hand Wash Only$|^Dry Clean Only$|^Spot Clean Only$|^Wipe Clean Only$|^Dishwasher proof$"

# Regular Expressions for Age

reg.ex.age <<- "^From Birth$|^6 Months \\+$|^12 Months \\+$|^18 Months \\+$|^2 Years \\+$|^3 Years \\+$|^4 Years \\+$|^5 Years \\+$|^6 Years \\+$|^7 Years \\+$|^8 Years \\+$|^12 Years \\+$|^Adult Only$"

# Regular Expressions for Assembly

reg.ex.assembly <<- "^Yes$|^No$"

# Regular Expressions for Capacity

reg.ex.capacity <<- "^[0-9]{1,3}(\\.[0-9]{1,2})?(ml|L)$"

# Regular Expressions for Coverage

#reg.ex.coverage <<- "^[0-9]{1,3}(\\.[0-9]{1,2})?(m²)$"

reg.ex.coverage <<- "[0-9]{1,3}(m²)"

# Regular Expressions for Power

reg.ex.power.category <<- "^Mains$|^Gas$|^Petrol$|^Battery$|^Solar$|^Electric$|^Manual$"
reg.ex.power.wattage <<- "^[0-9]{1,4}W$"

#reg.ex.dimension <<- paste(reg.ex.dim,"|",reg.ex.dimx,"|",reg.ex.range,sep = "")
#reg.ex.size <<- paste(reg.ex.fit,"|",reg.ex.hygiene,"|",reg.ex.nappy,"|",reg.ex.bandage,"|",reg.ex.shoe.size,"|",reg.ex.engine.size,sep = "")
#reg.ex.power <<- paste(reg.ex.power.category,"|",reg.ex.power.wattage,sep = "")
#------------------------------------------------------------------------------------------------------------------------

# Data Quality Classification and Scoring

# An attribute that is deemed a Critical Success Factor for a product type is missing e.g. colour for duvet covers
dq.rule.1.desc <<- "Critical Attribute Missing" 
dq.rule.1.score <<- -100

# An attribute value does not conform to one of the predefned lists of reference data or the correct format
dq.rule.2.desc <<- "Inconsistent Attribute" 
dq.rule.2.score <<- - 50

# There may be a questionable term in the Web Title e.g. use of 'Bodywash' instead of 'Body Wash'
dq.rule.3.desc <<- "reg.ex. Web Title Description" 
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
