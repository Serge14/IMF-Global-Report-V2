library(data.table)
#library(stringr)

setwd("/home/sergiy/Documents/Work/Nutricia/Global/Ver2")

# Read all necessary files

df = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201802/BFpivot3.csv", 
           header = TRUE, stringsAsFactors = FALSE, data.table = TRUE)

dictCompanies = fread("dictCompanies.csv")
dictBrands = fread("dictBrands.csv")
dictSG = fread("dictSG.csv")
dictSpecials = fread("dictSpecials.csv")
selection = fread("selection.csv")

df = df[, c("Period", "Subbrand", "Age", "Scent", "PIECES", "VALUE", "VOLUME", 
            "Channel", "Region", "Coef", "Correction") := NULL] # or "Correction" in a full name

# Transform to upper case in order to subset
df[, Form:=toupper(Form)]
df = df[(Form == "SOLID" | Form == "PURE" | Form == "POWDER") & PS0 == "IMF"]

# Transform other columns to upper case since file consists of different letters' cases
df[, SKU:= toupper(SKU)]
df[, PS0:=toupper(PS0)]
df[, PS2:=toupper(PS2)]
df[, PS3:=toupper(PS3)]
df[, PS:=toupper(PS)]
df[, Size:=toupper(Size)]
df[, Brand:=toupper(Brand)]
df[, Company:=toupper(Company)]
df[, PriceSegment:=toupper(PriceSegment)]
df[, Additives:=toupper(Additives)]

df[, Size := gsub("\\d\\*", "", Size)]
df[, Size := gsub("\\.", "P", Size)]



# Some rename either to avoid long names or mess with the same brand names 
# belonging to different companies

df[Company == "KHOROLSKII MK" & Brand == "MALYSH", Brand := "MALYSH KH"]
df[Company == "KHOROLSKII MK" & Brand == "MALYUTKA", Brand := "MALYUTKA KH"]
df[Company == "NUTRICIA" & Brand == "MALYSH ISTRINSKII", Brand := "MALYSH"] # delete ISTR
df[Company == "ASSOCIACIYA DETSKOGO PITANIYA", Company := "ASSOCIACIYA DP"]
df[Company == "ABBOTT LABORATORIES", Company := "ABBOTT LAB"]
df[Company == "KOMPANIYA EKSTREYD", Company := "EKSTREYD"]

# SKU resegmentation

df[SKU == "NAN_PREMIUM GIPOALLERGENNYI 3_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NESTLE",
   PS := "HYPOALLERGENIC"]

df[SKU == "NAN_PREMIUM KISLOMOLOCHNYI 3_400GR_12+_KISLOMOLOCHNYI_IMF_GUM_SPECIALS_GUM SPECIALS_NESTLE",
   PS := "DIGESTIVE COMFORT"]

df[SKU == "KABRITA_GOLD 3_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_HYPROCA NUTRITION" |
     SKU == "KABRITA_GOLD 3_800GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_HYPROCA NUTRITION" |
     SKU == "NENNI_NENNI 3_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_BIBICALL" |
     SKU == "NENNI_ZOLOTAYA KOZOCHKA_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_BIBICALL",
   PS := "SOY / GOAT"]

df[SKU == "NUTRILON_PEPTI JUNIOR_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NUTRICIA" |
     SKU == "NUTRILON_PEPTI JUNIOR_450GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NUTRICIA",
   PS := "ALLERGY TREATMENT"]

df[SKU == "NUTRILON_PEPTI JUNIOR_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NUTRICIA" |
     SKU == "NUTRILON_PEPTI JUNIOR_450GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NUTRICIA",
   PS2 := "IF"]

# Add Kabrita to Fruit Plus

# Rename Soy & Goat segments
setkey(dictSG, SKU)
setkey(df, SKU)
df[dictSG, PS := Segment]


df = df[, .(Items = sum(PIECESC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
        by = .(Ynb, Mnb, Brand, Size, PS0, PS2, PS3, PS, Company, PriceSegment, 
               Form, Additives)]

# IF PLUS must be moved to FO
df = df[PS3 == "PLUS" & PS2 == "IF", PS2 := "FO"]

# Add new segments
df[, PS1 := PS2]
df[PS2 == "IF" | PS2 == "FO", PS1 := "IFFO"]

# Rename segment names
df[PS3 == "BASE" | PS3 == "PLUS", PS3 := "CORE"]
df[PS3 == "SPECIALS", PS3 := "TAILORED"]

df[PS2 == "GUM", PS2 := "GUM1TO3"]

# Rename Additives names
df[Additives == "FLAVOURED", Additives := "FLAVOURD"]
df[Additives == "NON-FLAVOURED", Additives := "UNFLAVOU"]

# Rename Price Segments' names
df[PriceSegment == "MAINSTREAM", PriceSegment := "MAINSTRE"]

# Rename Specials segment names in order to align names with global report names
setkey(dictSpecials, localName)
setkey(df, PS)
df[dictSpecials, PS := globalName]

# Rename company in order to align names with global report names
setkey(dictCompanies, localName)
setkey(df, Company)
df[dictCompanies, Company := globalName]

# Rename brands in order to align names with global report names
setkey(dictBrands, localName)
setkey(df, Brand)
df[dictBrands, Brand := globalName]

df[, Organic := "NONORGAN"]
df[Brand == "HIPP", Organic := "ORGANIC"]

# Select brands and companies to report
sel = unique(selection$Company)
df = df[!(Company %in% sel), Company := "AO"] # ALL OTHERS


#sel = unique(selection[c("Company", "Brand")])
sel = unique(selection[, .(Company, Brand)])
#df1 = df1[!(Brand %in% sel), Brand := "ALL OTHER BRANDS"]
df = df[!(paste0(df$Company, df$Brand) %in% paste0(sel$Company, sel$Brand)), 
        Brand := "AO"] # ALL OTHERS

df[Brand == "ALL OTHERS", PriceSegment := "NOT PRICED"]

# Function to delete NA

#NAdelete = function(DT) {
#  n = dim(DT)[2]
#  for (i in names(DT)[(n-6):n]) {DT[is.na(get(i)), (i):=""]}
#}

# H1 L1
collect = dcast(df[, .(PS0, Ynb, Mnb, Volume, Value)], 
                PS0 ~ Ynb + Mnb, 
                value.var = c("Volume", "Value"), 
                fun.aggregate = sum)

# H1 L2
dftemp = dcast(df[, .(PS0, PS1, Ynb, Mnb, Volume, Value)], 
               PS0 + PS1 ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect = rbind(collect, dftemp, fill = TRUE)

# H1 L3
dftemp = dcast(df[, .(PS0, PS1, PS2, Ynb, Mnb, Volume, Value)], 
               PS0 + PS1 + PS2 ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect = rbind(collect, dftemp, fill = TRUE)

# H1 L4
dftemp = dcast(df[, .(PS0, PS1, PS2, PS3, Ynb, Mnb, Volume, Value)], 
               PS0 + PS1 + PS2 + PS3 ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect = rbind(collect, dftemp, fill = TRUE)

# H1 L5
dftemp = dcast(df[, .(PS0, PS1, PS2, PS3, PS, Ynb, Mnb, Volume, Value)], 
               PS0 + PS1 + PS2 + PS3 + PS ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect = rbind(collect, dftemp, fill = TRUE)

# H1 L6
dftemp = dcast(df[, .(PS0, PS1, PS2, PS3, PS, Company, Ynb, Mnb, Volume, Value)], 
               PS0 + PS1 + PS2 + PS3 + PS + Company ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect = rbind(collect, dftemp, fill = TRUE)

# H1 L7
dftemp = dcast(df[, .(PS0, PS1, PS2, PS3, PS, Company, Brand, 
                      Ynb, Mnb, Volume, Value)], 
               PS0 + PS1 + PS2 + PS3 + PS + Company + Brand ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect = rbind(collect, dftemp, fill = TRUE)

# H1 L8
dftemp = df[, .(Volume = sum(Volume), Value = sum(Value)), 
            by = .(PS0, PS1, PS2, PS3, PS, Company, Brand, Size, Ynb, Mnb)]

dftemp = dcast(dftemp, 
               PS0 + PS1 + PS2 + PS3 + PS + Company + Brand + Size ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect = rbind(collect, dftemp, fill = TRUE)

n = dim(collect)[2]
for (i in names(collect)[(n-6):n]) {
  collect[is.na(get(i)), (i):=""]
}
#collect = NAdelete(collect)

collect[, ITEMLIST := paste(1, PS0, PS1, PS2, PS3, PS, Company, Brand, Size)]

# delete all connected with NA at the names of ITEMLIST


#write.csv(collect, "collect.csv", row.names = FALSE, na = "")

## NEXT LEVELS are on

# H2 L1
collect2 = dcast(df[, .(PS0, Ynb, Mnb, Volume, Value)], 
                 PS0 ~ Ynb + Mnb, 
                 value.var = c("Volume", "Value"), 
                 fun.aggregate = sum)

# H2 L2
dftemp = dcast(df[,.(PS0, Company, Ynb, Mnb, Volume, Value)], 
               PS0 + Company ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect2 = rbind(collect2, dftemp, fill = TRUE)

# H2 L3
dftemp = dcast(df[,.(PS0, Company, Brand, Ynb, Mnb, Volume, Value)], 
               PS0 + Company + Brand ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect2 = rbind(collect2, dftemp, fill = TRUE)

# H2 L4
dftemp = dcast(df[,.(PS0, Company, Brand, PriceSegment, Ynb, Mnb, Volume, Value)], 
               PS0 + Company + Brand + PriceSegment ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect2 = rbind(collect2, dftemp, fill = TRUE)

# H2 L5
dftemp = dcast(df[,.(PS0, Company, Brand, PriceSegment, PS1, 
                     Ynb, Mnb, Volume, Value)], 
               PS0 + Company + Brand + PriceSegment + PS1 ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect2 = rbind(collect2, dftemp, fill = TRUE)

# H2 L6
dftemp = dcast(df[,.(PS0, Company, Brand, PriceSegment, PS1, PS2,
                     Ynb, Mnb, Volume, Value)], 
               PS0 + Company + Brand + PriceSegment + PS1 + PS2 ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect2 = rbind(collect2, dftemp, fill = TRUE)

# H2 L7
dftemp = dcast(df[,.(PS0, Company, Brand, PriceSegment, PS1, PS2, PS3,
                     Ynb, Mnb, Volume, Value)], 
               PS0 + Company + Brand + PriceSegment + PS1 + PS2 + PS3 ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect2 = rbind(collect2, dftemp, fill = TRUE)

# H2 L8
dftemp = dcast(df[,.(PS0, Company, Brand, PriceSegment, PS1, PS2, PS3, PS,
                     Ynb, Mnb, Volume, Value)], 
               PS0 + Company + Brand + PriceSegment + PS1 + PS2 + PS3 + PS ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect2 = rbind(collect2, dftemp, fill = TRUE)

n = dim(collect2)[2]
for (i in names(collect2)[(n-6):n]) {
  collect2[is.na(get(i)), (i):=""]
}

collect2[, ITEMLIST := paste(2, PS0, Company, Brand, PriceSegment, PS1, PS2, PS3, PS)]

# delete all connected with NA at the names of ITEMLIST


# H3 L1
collect3 = dcast(df[, .(PS0, Form, Ynb, Mnb, Volume, Value)], 
                 PS0 + Form ~ Ynb + Mnb, 
                 value.var = c("Volume", "Value"), 
                 fun.aggregate = sum)

# H3 L2
dftemp = dcast(df[, .(PS0, Form, Ynb, Mnb, Volume, Value)], 
               PS0 + Form ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect3 = rbind(collect3, dftemp, fill = TRUE)

# H3 L3
dftemp = dcast(df[, .(PS0, Form, Organic, Ynb, Mnb, Volume, Value)], 
               PS0 + Form + Organic ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect3 = rbind(collect3, dftemp, fill = TRUE)

# H3 L4
dftemp = dcast(df[, .(PS0, Form, Organic, Company, Ynb, Mnb, Volume, Value)], 
               PS0 + Form + Organic + Company ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect3 = rbind(collect3, dftemp, fill = TRUE)

# H3 L5
dftemp = dcast(df[, .(PS0, Form, Organic, Company, Brand, 
                      Ynb, Mnb, Volume, Value)], 
               PS0 + Form + Organic + Company + Brand ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect3 = rbind(collect3, dftemp, fill = TRUE)

# H3 L6
dftemp = dcast(df[, .(PS0, Form, Organic, Company, Brand, PS2,
                      Ynb, Mnb, Volume, Value)], 
               PS0 + Form + Organic + Company + Brand + PS2 ~ Ynb + Mnb, 
               value.var = c("Volume", "Value"), 
               fun.aggregate = sum)

collect3 = rbind(collect3, dftemp, fill = TRUE)

n = dim(collect3)[2]
for (i in names(collect3)[(n-3):n]) {
  collect3[is.na(get(i)), (i):=""]
}

collect3[, ITEMLIST := paste(3, PS0, Form, Organic, Company, Brand, PS2)]

collect = rbind(collect, collect2, collect3, fill = TRUE)

#collect[, ITEMLIST := gsub(" NA", "", ITEMLIST)]

n = dim(collect)[2]

setcolorder(collect, c(n-3, 1, (n-10):(n-4), (n-2):n, 2:(n-11)))
setorder(collect, ITEMLIST)

write.csv(collect, "collect.csv", row.names = FALSE, na = "")
