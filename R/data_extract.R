#interpolate linearly missing values
int.lin <- function(Dt, y.int, years){
  setDF(Dt)
  for(y in y.int[-which(y.int %in% years)]){
    yb <- which(colnames(Dt) == y)
    ym5 <- which(colnames(Dt) == y-5)
    yp5 <- which(colnames(Dt) == y+5)
    
    Dt[which(is.na(Dt[,yb])),yb] <- (Dt[which(is.na(Dt[,yb])),ym5] + Dt[which(is.na(Dt[,yb])),yp5])/2
  }
  
  setDT(Dt)
  return(Dt)
}

get_forallyears <- function(DT){
  setDF(DT)
  nd <- DT
  for(i in 2:ncol(DT)){#emissions in all but the first timestep account for the emissions in the entire 5y long period
    nd[,i] <- 3*DT[, i] + 2*DT[, (i-1)]
  }
  setDT(nd)
  return(nd)
}

get.data <- function(){
  years <- seq(2010,2100,10)
  y.int <- seq(2010,2100,5)
  
  baselines <- c('EN_NPi2100', 'EN_NPi2100_COV') #COVID scenarios discarded in final analysis
  
  tol = 5*1e-3 #tolerance on negative Mitigation costs of 0.5%
  
  #read data
  DT <- fread("data/engage_snapshot_1706260074.csv", header = T)
  DT$ID <- as.factor(DT$Model):as.factor(DT$Scenario)
  DT <- DT[(str_detect(DT$Scenario, 'NDC') | str_detect(DT$Scenario, 'NP')),]#keep only relevan scenarios
  
  merid <- DT$ID[which(DT$Variable=='GDP|MER')]#keep only scenarios with GDP|MER
  DT <- DT[which(DT$ID %in% merid),]
  
  #define structured models
  DT$SM <- gsub("-.*","",DT$Model)
  DT$SM <- str_replace_all(DT$SM, pattern = c('0' = '', '1' = '', '2' = '', '3' = '', '4' = '', '5' = '', '6' = '', '7' = '', '8' = '', '9' = '', ' .' = ''))
  DT$SM <- str_replace_all(DT$SM, pattern = c(' ' = '', "ix" = '', '/CGE' = ''))
  DT$SM[which(DT$SM=='AIM.')] <- 'AIM'
  DT$COV <- str_detect(DT$Scenario, 'COV')
  DT$SM <- ifelse(DT$COV, paste(DT$SM,"_COV", sep = ''), DT$SM)
  DT <- DT[which(!(DT$SM == 'IMAGE')),]#constant GDP across scenarios
  DT <- DT[which(!(DT$SM %in% c('GEM_COV', 'REMIND_COV'))),]#Less than 20 scenarios
  DT$SM <- as.factor(as.character(DT$SM))
  DT <- DT[,-c("2021", "2022", "2023", "2024")]
  
  #missing 2010 values, inferred from similar SMs when possible
  DT$`2010`[which(is.na(DT$`2010`) & DT$Variable=='GDP|MER')] <- mean(DT$`2010`[which(DT$Variable=='GDP|MER' & DT$Scenario %in% baselines & DT$SM %in% c('IMAGE', 'REMIND', 'REMIND_COV'))])
  DT$`2010`[which(is.na(DT$`2010`) & DT$Variable=='Emissions|CO2' & DT$Model == 'GEM-E3 V2021')] <- DT$`2010`[which(DT$Variable=='Emissions|CO2' & DT$Model == 'GEM-E3_092019' & DT$Scenario %in% baselines)]
  
  #remove scenarios with non decade values reported
  setDF(DT)
  for(y in years){
    DT <- DT[which(!is.na(DT[,which(colnames(DT) == y)])),]
  }
  setDT(DT)
  
  #extract variables and meta informations
  meta <- DT[, c("Model", "Scenario", "Region", "ID", "SM", "COV")]
  meta <- meta[duplicated(meta$ID),]
  data.emi <- DT[which(DT$Variable=='Emissions|CO2'), -c("Model", "Scenario", "Region", "Variable", "Unit", "SM", "COV")]
  data.gdp <- DT[which(DT$Variable=='GDP|MER'), -c("Model", "Scenario", "Region", "Variable", "Unit", "SM", "COV")]
  
  meta <- meta[order(meta$ID),]
  data.emi <- data.emi[order(data.emi$ID),1:19]/1e3 #in GtCO2
  
  data.gdp <- data.gdp[order(data.gdp$ID),1:19]# in billion US$2010/yr
  
  #compute GDP loss
  data.gdpl <- data.gdp
  for(i in 1:nrow(meta)){
    row <- replicate(ncol(data.gdpl),NaN)
    baseline <- which(meta$Model==meta$Model[i] & meta$Scenario %in% baselines & meta$COV == meta$COV[i])
    if(length(baseline)== 1){
      row <- 1 - data.gdp[i,]/data.gdp[baseline,]
    } else if(length(baseline)>1){print(i)}
    data.gdpl[i,]<- row
  }
  
  #coherent reporting of 2010 emission -> important for Alignment of cumulative emission FoSR
  for(s in unique(meta$SM)){
    data.emi$`2010`[which(meta$SM == s)] <- mean(data.emi$`2010`[which(meta$SM == s & meta$Scenario %in% baselines)]) #different versions of GEM and REMIND, but same value
  }
  
  #linear interpolation for missing data
  data.emi <- int.lin(data.emi, y.int, years)
  data.gdpl <- int.lin(data.gdpl, y.int, years)
  #emissions are every 5 years, interpolate and sum
  data.emi <- get_forallyears(data.emi)
  
  #remove CB > 2000
  cb <- get.cb(data.emi)
  meta <- meta[which(cb <= 2000),]
  data.emi <- data.emi[which(cb <= 2000),]
  data.gdpl <- data.gdpl[which(cb <= 2000),]
  
  #remove biased scenarios
  meta <- meta[which(data.gdpl[,1] < tol),]
  data.emi <- data.emi[which(data.gdpl[,1] < tol),]
  data.gdpl <- data.gdpl[which(data.gdpl[,1] < tol),]

  #sanity checks
  if(-min(data.gdpl)>tol){stop("Negative mitigation costs")}
  if(nrow(meta) != nrow(data.emi) | nrow(meta) != nrow(data.gdpl)){stop("Missing scenarios")}
  
  #format
  data_list <- list(meta = meta, data = list(DT.e.co2 = data.emi, DT.miticost = data.gdpl))
  return(list(data = data_list, y.int = y.int, tol = tol))
}
