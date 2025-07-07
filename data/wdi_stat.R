#load data from World Development Indicators
library(WDI)
library(data.table)
library(fst)

# Check units with
#WDIsearch("GDP, PPP")
#WDIsearch("Population, total")

wdi_varlist <- fread('wdi_identifier,description,varname
SP.POP.TOTL,Population,pop
NY.GDP.MKTP.PP.KD,"GDP, PPP (constant ???? international $)",gdp_ppp
NY.GDP.MKTP.KD,"GDP (constant 2010 US$)",gdp
') # changing year!! as now 2017
#NY.GDP.MKTP.IN,"GDP Deflator",gdp_deflator# cannot be downloaded

allwdi <- data.table(WDI(country = "all", 
                         start = 1960, 
                         end = 2018,
                         indicator = wdi_varlist$wdi_identifier, 
                         extra = TRUE))
setnames(allwdi, "iso3c", "iso3")
setnames(allwdi, wdi_varlist$wdi_identifier, wdi_varlist$varname)

wdi_stat <- allwdi[iso3 != "<NA>" & !is.na(pop) & !is.na(gdp), .(year, iso3, gdp, gdp_ppp, pop)]

# Compute GDP in USD2019
gdp_deflators <- setDT(read_excel("data/API_NY.GDP.DEFL.ZS_DS2_en_excel_v2_6226265.xls", skip = 3))
gdp_deflators <- gdp_deflators[`Country Code`=='USA',-(1:4)]

wdi_stat[, gdp := gdp / gdp_deflators$`2010` * gdp_deflators$`2018`]
wdi_stat[, gdp_ppp := gdp_ppp / gdp_deflators$`2017` * gdp_deflators$`2018`]

write_fst(wdi_stat,'data/wdi_stat.fst')
