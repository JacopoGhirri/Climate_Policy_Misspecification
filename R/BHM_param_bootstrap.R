read_param_bhm <- function() { #from https://github.com/lolow/iampact-ipcc-sr15c/blob/main/R/impact_bhm.R
  
  mb_list <- list()
  
  # Original BHM2015 dataset
  dta <- setDT(read.dta13('data/BDD2018/data/input/GrowthClimateDataset_Stata13.dta'))
  dta <- dta[, .(iso3 = iso, year, pop = Pop, gdp = TotGDP * 1.12538, #(USD2010->USD2018) 
                 growth = growthWDI,temp = UDel_temp_popweight)]
  dta <- dta[!iso3 %in% c('COD', 'ROU')]
  dta <- dta[iso3 == 'ZAR', iso3 := 'COD']
  dta <- dta[iso3 == 'ROM', iso3 := 'ROU']
  dta <- na.omit(dta)
  mb_list = c(mb_list, list(`BHM DATASET` = dta))
  
  # Non-significant coefficients are set to zero for main estimates
  # (ref. replication code)
  # Pooled bootstrap_noLag 
  pb <- read_fst('data/BDD2018/data/output/bhm_sr_param.fst', as.data.table = T)
  mb <- as.matrix(pb[,.(b1,b2,optimal,normalize)])
  mb_list = c(mb_list, list(`BHM SR` = mb))
  # Rich/Poor noLag specification
  pb <- fread("data/BurkeHsiangMiguel2015_Replication/data/output/bootstrap/bootstrap_richpoor.csv")
  mb <- as.matrix(pb[,.(temp,temppoor,temp2,temp2poor)])
  mb_list = c(mb_list, list(`BHM SR RP` = mb))
  # Pooled bootstrap_5Lag
  pb <- read_fst('data/BDD2018/data/output/bhm_lr_param.fst', as.data.table = T)
  mb <- as.matrix(pb[,.(b1,b2,optimal,normalize)])
  mb[1,2] <- 0 # Quadratic term not significant
  mb_list = c(mb_list, list(`BHM LR` = mb))
  pb <- read_fst('data/BDD2018/data/output/bhm_lr_param.fst', as.data.table = T)
  mb <- as.matrix(pb[,.(b1,b2,optimal,normalize)])
  mb_list = c(mb_list, list(`BHM LR ORIG` = mb))
  # Rich/Poor 5-lag specification 
  pb <- fread("data/BurkeHsiangMiguel2015_Replication/data/output/bootstrap/bootstrap_richpoor_5lag.csv")
  mb <- as.matrix(pb[,.(tlin,tlinpoor,tsq,tsqpoor)])
  mb[1,4] <- 0 # Quadratic term not significant
  mb[1,c(1,3)] <- 0 # Rich coefficients not significant
  mb_list = c(mb_list, list(`BHM LR RP` = mb))
  pb <- fread("data/BurkeHsiangMiguel2015_Replication/data/output/bootstrap/bootstrap_richpoor_5lag.csv")
  mb <- as.matrix(pb[,.(tlin,tlinpoor,tsq,tsqpoor)])
  mb_list = c(mb_list, list(`BHM LR RP ORIG` = mb))
  # KW2020 (panel regression Tab4 (2) - reproducing BHM SR, but at higher resolution)
  mb_list = c(mb_list, list(`KW2020 BHM` = matrix(c(0.00947,-7.09e-04),nrow = 1)))
  # BT2019 (Tab S3 panel A Column (1-2), Baseline specification)
  # BT2019 STATE - linear not significant
  mb_list = c(mb_list, list(`BT2019 STATE` = matrix(c(0.0033392,-3.013e-04),nrow = 1)))
  mb_list = c(mb_list, list(`BT2019 CNTRY` = matrix(c(0.0108371,-5.752e-04),nrow = 1)))
  # Pretis2018 (Tab 1 Specification M2)
  # Baseline is 2006-2014 in paper
  mb_list = c(mb_list, list(`PRETIS2018` = matrix(c(0.0115,-4e-04),nrow = 1)))
  # Newell2018 (Tab 4 Specification DJO*+T2)
  mb_list = c(mb_list, list(`NEWELL2018 DJO T2` = matrix(c(0.013219,-3.85e-04),nrow = 1)))
  # HS2019 (Tab 4 Column 1)
  mb_list = c(mb_list, list(`HS2019` = matrix(c(0.00928,-4.535e-04),nrow = 1)))
  # ACEVODO2020 (Tab 1 Column 5)
  mb_list = c(mb_list, list(`ACEVODO2020` = matrix(c(1.347 / 100,-0.051 / 100),nrow = 1)))
  
  return(mb_list)
}

get.BHM.par <- function(){
  par <- read_param_bhm()
  par <- par$`BHM SR`[,1:2]
  return(par)
}

