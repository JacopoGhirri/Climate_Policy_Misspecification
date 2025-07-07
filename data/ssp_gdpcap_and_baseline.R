wdi_stat = setDT(read_fst("data/wdi_stat.fst"))

# ORIGINAL SSP ASSUMPTIONS
# Source: https://tntcat.iiasa.ac.at/SspDb/dsd
ssp_csv = "data/SspDb_country_data_2013-06-12.csv"

## OECD Population [millions]
ssp_pop = ssp_query(ssp_csv, "OECD Env-Growth", "Population")
## OECD GDP PPP [billion US$2005/yr]
ssp_gdp = ssp_query(ssp_csv, "OECD Env-Growth", "GDP|PPP")
## Compute gdp per capita for SSP scenario [USD2018/yr]
ssp_gdpcap = compute_gdp_cap(ssp_gdp, ssp_pop, wdi_stat)
removers <- c("ABW", "BHR", "BRB", "HKG", "LCA", "MAC", "MDV", "MLT", "PSE", "PYF", "SGP", "TON")

ssp_gdpcap <- ssp_gdpcap[-which(ssp_gdpcap$iso3 %in% removers),]#countries with no temperature data, ~1% of GDP

ssp_gwp <- ssp_gdpcap[,.(gdp_ref = sum(gdpcap_nocc * pop) * 1e-3), by = c("ssp","year")]

ssp_gdpcap <- ssp_gdpcap[which(ssp_gdpcap$ssp=='ssp2'),-1]
write.csv(ssp_gdpcap, 'data/ssp_gdpcap.csv', row.names = F)


ssp_gdpcap <- read.csv('data/ssp_gdpcap.csv')
baseline_temp <- read_feather("data/baseline_temp.feather")#get_baseline_temp()
downscaling.coeff <- read_parquet('data/cmip6_downscaling_coef.parquet')

project_bhm <- function(tas, gdpr, gdpc_2010, tas_base, param){
  stopifnot(length(tas) == 19) # 2010-2100 [5-year]
  stopifnot(length(gdpr) == 91) # 2010-2100
  
  .gdpcap <- rep(gdpc_2010,91)
  idx_tas <- ceiling((1:91) / 5)
  idx_tas1 <- pmin(19,ceiling((1:91) / 5) + 1)
  for (i in 2:91) {
    tas_i <- tas[idx_tas[i]] + ((i - 1) %% 5) * (tas[idx_tas1[i]] - tas[idx_tas[i]]) / 5
    .delta <- warming_effect(tas_i - tas[1] + tas_base, tas_base, param)
    .gdpcap[i] <- .gdpcap[i - 1] * (1 + gdpr[i] + .delta)
  }
  
  names(.gdpcap) <- 2010:2100
  y.int <- 2010 + 5*0:18
  
  return(gdpcap_cc = .gdpcap)#[which(names(.gdpcap) %in% y.int)])
}

t <- 0*seq(2010,2100,5)
names(t)=seq(2010,2100,5)

dt <- downscale_temp(t, downscaling.coeff)

full_gdp <- numeric(length(seq(2010,2100)))

for(c in unique(ssp_gdpcap$iso3)){
  tas <- dt[which(dt$iso3 == c),-1]
  tas <- tas$value[order(tas$year)]
  gdp <- project_bhm(tas, 
                     ssp_gdpcap$gdpr[which(ssp_gdpcap$iso3 == c)], 
                     ssp_gdpcap$gdpcap_nocc[which(ssp_gdpcap$iso3 == c & ssp_gdpcap$year == 2010)], 
                     baseline_temp$tas[which(baseline_temp$iso3 == c)],
                     par)
  gdp <- gdp*ssp_gdpcap$pop[which(ssp_gdpcap$iso3==c)]# & ssp_gdpcap$year %in% y.int)]
  full_gdp <- full_gdp + gdp
}
full_gdp <- full_gdp*1e-3
baseline <- cbind(seq(2010,2100), full_gdp)
colnames(baseline)<-c("year","gdp")
#baseline_1 <- ssp_gwp[which(ssp_gwp$ssp == 'ssp2'), c(2,3)]
write.csv(baseline, 'data/baseline_world.csv', row.names = F)
