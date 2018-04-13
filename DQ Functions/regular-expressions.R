# These Regular Expressions define the pattern matching for Wilko.com product attributes

# Regular Expressions for Weights

 check.weight <<- "^[0-9]{1,4}(\\.[0-9]{1,3})?(mg|g|kg|ml|L|oz|cl|pt|lb)$"
 
# Regular Expressions for Sizes 

 check.fit <<-"^One Size$|^Small$|^Medium$|^Large$|^X-Large$|^XX-Large|^XXX-Large$|^Assorted$|^Adult$"
 check.hygiene <<- "^X-Small$|^Small$|^Normal$|^Mini$|^Regular$|^Super Medium$|^Super$|^Super Plus$|^Long$|^Extra Long$|^Extra Large$|^Thin$|^Ultra$|^Extra$|^Maxi$"
 check.nappy <<- "^Other$|^Size 1$|^Size 2$|^Size 3$|^Size 4$|^Size 4\\+$|^Size 5$|^Size 5\\+$|^Size 6$|^Size 6\\+$"
 check.bandage <<- "^Size C$|^Size D$|^Size E$|^Size F$|^Size G$"
 check.shoe.size <<- "^Size 4|^Size 5|^Size 6$|^Size 7$|^Size 8$|^Size 9$|^Size 10$|^Size 11$"
 check.engine.size <<- "^[0-9]{1,3}(\\.[0-9]{1})?(cc)$"
 check.tissue <<- "^Regular$|^Extra Large$|^Pocket Pack$|^Mansize$|^Large$"
 check.electrical <<- "^[0-9]{1,4}W$|^[0-9]{1,2} Slice$"
 check.crockery <<- "^[0-9]{1,2} Piece$|^[0-9]{1,2} Bottle$"
 check.drawer <<- "^[0-9] Drawer$"
 check.bedding <<- "^Single$|^Double$|^Kingsize$|^Super Kingsize$"
 
# Regular Expressions for Single Measures 
 
 check.measure <<- "^[0-9]{1,3}(\\.[0-9]{1,2})?(mm|cm|m|ft|in)$"
 
# Regular Expressions for Attributes
 
 check.colour <<- "Brown|Multi|Black|Blonde|Green|White|Cream|Grey|Silver|Wood|Red|Blue|Pink|Yellow|Metal|Clear|Orange|Purple|Gold"

 # Reguar Expressions for Dimensions of the same units
 
 check.dim <<- "^([0-9]{1,3}(\\.[0-9]{1})? x ){1,3}[0-9]{1,3}(\\.[0-9]{1})?(mm|cm|m|ft|in)$"

 # Reguar Expressions for Dimensions of different units
 
 check.dimx <<- "^[0-9]{1,2}(\\.[0-9]{1,2})?(mm|cm|m) x [0-9]{1,2}(\\.[0-9]{1})?(mm|cm|m|ft|in)$"
 
 # Regular Expressions for Ranges of same units
 
 check.range <<- "^[0-9]{1,2}-[0-9]{1,2}(mm|cm|m)$"

 check.dimension <<- paste(check.dim,"|",check.dimx,"|",check.range,sep = "")
 
 # Regular Expressions for Pack Qty
 
 check.pack.qty <<- "^[0-9]{1,3}$"
                          
 # Minor web title formatting errors

 toi.web.desc.err <<- "2in1|EDT|\\&|Bodywash|Eyedrops|Bodyspray|Dryskin|Footcare|Lipgloss|Lipliner|Facewash|Coolmint|Nailpolish|Supergel|Supershine|Footcare|Cremepuff"
 
 # Regular Expressions for Material
 
 check.material.cat <<- "^Wood$|^Metal$|^Paper$|^Ceramic$|^Fabric$|^Plastic$|^Other$"
 check.material.value <<- "^Slate$|^Chrome Plated$|^Teslin$|^Pyrex$|^Stoneware$|^Earthenware$|^Reed$|^Bamboo$|^Ash$|^Chipboard$|^Cork$|^Eucalyptus$|^MDF$|^Oak$|^Particleboard$|^Pine$|^Recycled Board$|^Softwood$|^Wicker$|^Willow$|^Aluminium$|^Brass$|^Chrome$|^Copper$|^Gold$|^Iron$|^Silver$|^Stainless Steel$|^Cardboard$|^Greyboard$|^Paper$|^China$|^Glass$|^Porcelain$|^Cotton$|^Elastane$|^Nylon$|^Polycotton$|^Polyester$|^Viscose$|^Acrylic$|^Fiberglass$|^Graphite$|^Latex$|^Melamine$|^PES$|^Polyamide$|^Polycarbonate$|^Polyethylene$|^Polypropylene$|^PVA$|^PVC$|^Vinyl$|^Bassine$|^Canvas$|^Coir$|^Faux Leather$|^Foam$|^Jute$|^Microfibre$|^Rubber$|^Polythene$|^Coco Fibre$|^Fleece$|^Galvanised Steel$|^Steel$|^EVA$"      
 check.material.effect <<- "^(Copper|Silver|Gold|Bronze) Effect$"
 check.material.pct <<- "^[0-9]{1,3}% (Cotton|Polyester)$"
 check.material.ratio <<- "^[0-9]{1,2}% (Cotton|Polyester) - [0-9]{1,2}% (Cotton|Polyester)$"
 
 check.material <<- paste(check.material.cat,check.material.value,check.material.effect,check.material.pct,check.material.ratio,sep = "|") 
 
# Regular Expressions for Washable
 
 check.washable <<- "^Washable$|^Do Not Wash$|^Wash at 30$|^Wash at 40$|^Wash at 40 Synth$|^Wash at 40 Wool$|^Wash at 60$|^Hand Wash Only$|^Dry Clean Only$|^Spot Clean Only$|^Wipe Clean Only$|^Dishwasher proof$"
 
 # Regular Expressions for Age
 
 check.age <<- "^From Birth$|^6 Months \\+$|^12 Months \\+$|^18 Months \\+$|^2 Years \\+$|^3 Years \\+$|^4 Years \\+$|^5 Years \\+$|^6 Years \\+$|^7 Years \\+$|^8 Years \\+$|^12 Years \\+$|^Adult Only$"

 # Regular Expressions for Assembly
 
 check.assembly <<- "^Yes$|^No$"
 
 # Regular Expressions for Capacity
 
 check.capacity <<- "^[0-9]{1,3}(\\.[0-9]{1,2})?(ml|L| Bottles| Pints)$"
 
 # Regular Expressions for Coverage
 
 #check.coverage <<- "^[0-9]{1,3}(\\.[0-9]{1,2})?(m²)$"
 
 check.coverage <<- "[0-9]{1,3}(m²)"
 
 # Regular Expressions for Power
 
 check.power.category <<- "^Mains$|^Gas$|^Petrol$|^Battery$|^Solar$|^Electric$|^Manual$"
 check.power.wattage <<- "^[0-9]{1,4}W$"
 
 #check.size <<- paste(check.fit,"|",check.hygiene,"|",check.nappy,"|",check.bandage,"|",check.shoe.size,"|",check.engine.size,sep = "")
 check.power <<- paste(check.power.category,"|",check.power.wattage,sep = "")

 #--------------------------------------------------------------------------------------------------------------
 
 get.size <- function(psa1) {
      
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
       
       return(check.size)
       
 }
