# ========================================================================================================== #

# Install and upload libraries and source the custom script----

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(plotly)
library(reshape2)
library(lmtest)
library(future.apply)
library(abind)
library(parallel)
library(tidyverse)
library(geosphere)
library(patchwork)
library(openxlsx)


# ========================================================================================================== #
# 
# This code includes five parts:
# 
# (1) Figure 1. iNEXT.BetaDiv standardization for two plots in Gentry??s data. 
# (2) Figure 2. Latitudinal beta diversity gradient based on the iNEXT.BetaDiv standardization under six coverage values for Gentry??s 197 plots, each with 10 subplots.
# (3) Figure 3. The iNEXT.BetaDiv standardization for two time periods for beetle data.
# (4) Figure 4. Trajectories of temporal beta diversity over time for tree species incidence data among 100 subplots in six second-growth rainforests.
# (5) Figure 5. Temporal and spatial gamma, alpha and beta diversity for fish species based on SWC-IBTS data from 1985 to 2010.
# 
# See "Brief guide" for details. 
# 
# ========================================================================================================== #


library(devtools)
install_github("AnneChao/iNEXT.3D")       # Press 'enter' key to skip update options
library(iNEXT.3D)
install_github("AnneChao/iNEXT.BetaDiv")  # Press 'enter' key to skip update options
library(iNEXT.BetaDiv)

source("Source R code.txt")


# ========================================================================================================== #
# Figure 1. iNEXT.BetaDiv standardization for two plots in Gentry??s data. 

load (url ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/gentry197.r'))

gentry = gentry197[c('madden','jejuimi')]
gentry = lapply(gentry, function(k) t(k))
names(gentry) <- c("Madden","Jejuimi")


## Figure 1 (a)
output_fig_1a = iNEXTBetaDiv(gentry, datatype = 'abundance', base = "size", nboot = 200)
fig_1a_or_3a(output_fig_1a)


## Figure 1 (b)
output_fig_1b = iNEXTBetaDiv(gentry, datatype = 'abundance', base = 'coverage', nboot = 200, level=seq(0.5,1,0.05))
fig_1b_or_3b(output_fig_1b)



# ========================================================================================================== #
# Figure 2. Latitudinal beta diversity gradient based on the iNEXT.BetaDiv standardization under six coverage values for Gentry??s 197 plots, each with 10 subplots.

load (url ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/gentry197.r'))
gentry.coord <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/gentry.coord.txt', row.names = 1)


gentry_data <- data.frame()

for(i in 1:length(gentry197)) {
  
  x <- t(gentry197[[i]])
  out = iNEXTBetaDiv(x, level=seq(0.5,1,0.1), datatype='abundance', nboot = 0,conf = 0.95)
  df <- out$Region_1$beta[,c(1:4,6)]
  
  m <- df[df$Method=="Observed_alpha" & df$Order==0 ,]$Size %>% round
  
  
  df_inter <- subset(df, Method=="Interpolated")
  df_obs <- subset(df, Method=="Observed")
  df_extra <- subset(df, Method=="Extrapolated")
  
  for(j in 1:nrow(df_extra)){
    df_extra[j,]$Method <- "Extrapolated(short-range)"
    if(round(df_extra[j,]$Size) > 2*m ){ df_extra[j,]$Method <- "Extrapolated(long-range)" }
  }
  
  Div <- rbind(df_inter,df_obs,df_extra)
  
  
  physic <- gentry.coord[which(rownames(gentry.coord)==names(gentry197)[i]),]
  df <- cbind(Div, Latitude=physic$Lat,
              Longitude=physic$Long,Elevation=physic$Elev,
              Precip=physic$Precip,
              Plot=names(gentry197)[i])
  df = df %>% filter( Method=="Observed"| level %in% seq(0.5,1,0.1) ) 
  gentry_data <- rbind(gentry_data,df)
}


fig_2(gentry_data)



# ========================================================================================================== #
# Figure 3. The iNEXT.BetaDiv standardization for two time periods for beetle data.

## Figure 3 (a)
beetle = list('Logged'   = read.xlsx('Data Beetles.xlsx', rowNames = T, sheet = 1),
              'Unlogged' = read.xlsx('Data Beetles.xlsx', rowNames = T, sheet = 2))

output_fig_3a = iNEXTBetaDiv(beetle, datatype = 'abundance', base = "size", nboot = 200)
fig_1a_or_3a(output_fig_3a)


## Figure 3 (b)
output_fig_3b = iNEXTBetaDiv(beetle, datatype = 'abundance', base = 'coverage', nboot = 200,level= seq(0.8,1,0.025))
fig_1b_or_3b(output_fig_3b)



# ========================================================================================================== #
# Figure 4. Trajectories of temporal beta diversity over time for tree species incidence data among 100 subplots in six second-growth rainforests.

Cuat.raw   = read.xlsx("Data Second-growth forests.xlsx", sheet = 1)
LindEl.raw = read.xlsx("Data Second-growth forests.xlsx", sheet = 2)
Tiri.raw   = read.xlsx("Data Second-growth forests.xlsx", sheet = 3)
LindSu.raw = read.xlsx("Data Second-growth forests.xlsx", sheet = 4)
FEB.raw    = read.xlsx("Data Second-growth forests.xlsx", sheet = 5)
JE.raw     = read.xlsx("Data Second-growth forests.xlsx", sheet = 6)


inci.raw = list(SGF.data.transf(Cuat.raw   %>% filter(Year >= 2005)), 
                SGF.data.transf(LindEl.raw %>% filter(Year >= 2005)), 
                SGF.data.transf(Tiri.raw   %>% filter(Year >= 2005)), 
                SGF.data.transf(LindSu.raw %>% filter(Year >= 2005)), 
                SGF.data.transf(FEB.raw), 
                SGF.data.transf(JE.raw))

age = data.frame(Assem = c("Cuatro Rios", "Lindero el Peje", "Tirimbina", "Lindero Sur", "Finca el Bejuco", "Juan Enriquez"),
                 Age = c(25,20,15,12,2,2))   ## fix on year 1997


cpu.cores <- detectCores() - 1
cl <- makeCluster(cpu.cores)
clusterExport(cl, varlist = c("inci.raw", "for_fig_4"), envir = environment())
clusterEvalQ(cl, c(library(tidyverse), library(iNEXT.BetaDiv), library(reshape2)))

forests.output = parLapply(cl, inci.raw, function(x) for_fig_4(x, nboot = 200))

stopCluster(cl)

fig_4(forests.output)



# ========================================================================================================== #
# Figure 5. Temporal and spatial gamma, alpha and beta diversity for fish species based on SWC-IBTS data from 1985 to 2010.

fish <- read.csv("Data Fish_Lat55-60.csv")
fish = fish %>% mutate(region = ifelse(LatBand %in% c(55.5, 56, 56.5, 57), 'South', 
                                       ifelse(!(LatBand %in% c(57.5, 60)), 'North', 'Other'))) %>% filter(region != 'Other')
groupyear = matrix(1985:2010, nrow = 2)
colnames(groupyear) = paste(groupyear[1,], groupyear[2,], sep = '~')


cpu.cores <- detectCores()-1
cl <- makeCluster(cpu.cores)
clusterExport(cl, varlist = c("rarefysamples", "fish", "groupyear"), envir = environment())
clusterEvalQ(cl, c(library(tidyverse), library(iNEXT.BetaDiv), library(reshape2)))

simu_output = parLapply(cl, 1:200, function(k) {
  region <- unique(fish$region)
  
  TSrf <- list()
  
  for(i in 1:length(region)){
    data2 <- fish[fish$region==region[i],]
    TSrf[[i]]<-rarefysamples(data2) 
  }
  names(TSrf) <- region
  
  rf <- do.call(rbind, TSrf)
  rf <- data.frame(rf, LatBand=rep(names(TSrf), times=unlist(lapply(TSrf, nrow))))
  rf <- rf[!is.na(rf$Year),-1]
  rownames(rf)<-NULL
  data = rf
  
  cov = c(0.99, 0.999, 1)
  
  ## ================== Temporal ================== ##
  beta.temp = lapply(region, function(i) {
    tmp = data %>% filter(LatBand %in% i)
    
    tmp2 = lapply(2:length(unique(tmp$Year)), function(j) {
      g1 = dcast(tmp %>% filter(Year == sort(unique(tmp$Year))[1]), Species ~ LatBand, value.var = 'Abundance')
      
      g2 = dcast(tmp %>% filter(Year == sort(unique(tmp$Year))[j]), Species ~ LatBand, value.var = 'Abundance')
      
      out = full_join(g1, g2, by = 'Species')[,-1]
      
      out[is.na(out)] = 0
      out
    })
    names(tmp2) = sort(unique(tmp$Year))[-1]
    
    return(tmp2)
  })
  
  names(beta.temp) = region
  
  output.temp = lapply(1:length(beta.temp),  function(i) {
    result = iNEXTBetaDiv(beta.temp[[i]], q = c(0, 1, 2), datatype = 'abundance', level = cov, nboot = 0)
    
    cbind(lapply(result, function(y) lapply(1:3, function(i) cbind(y[[i]], div_type = names(y)[i])) %>% do.call(rbind,.)) %>% 
            do.call(rbind,.) %>% filter(level %in% cov | Method == 'Observed'),
          Latitude = names(beta.temp)[i])
  }) %>% do.call(rbind,.)
  
  output.temp$Region = as.numeric(output.temp$Region)
  output.temp = rbind(output.temp, output.temp %>% filter(Method == 'Observed', level %in% cov) %>% mutate('Method' = paste('Observed_', div_type, sep = '')))
  output.temp[output.temp$Method == 'Observed', 'level'] = 'Observed'
  
  ## ================== Spatial ================== ##
  beta.spat = lapply( list( c('South', 'North') ), function(i) {
    tmp1 = data %>% filter(LatBand %in% i[[1]])
    tmp2 = data %>% filter(LatBand %in% i[[2]])
    
    year = unique(data$Year) %>% sort
    
    tmp = lapply(year, function(j) {
      g1 = dcast(tmp1 %>% filter(Year == j), Species ~ LatBand, value.var = 'Abundance')
      g2 = dcast(tmp2 %>% filter(Year == j), Species ~ LatBand, value.var = 'Abundance')
      
      out = full_join(g1, g2, by = 'Species')[,-1]
      
      out[is.na(out)] = 0
      out
    })
    names(tmp) = year
    
    return(tmp)
  })
  
  names(beta.spat) = 'South vs. North'
  
  output.spat = lapply(1:length(beta.spat),  function(i) {
    result = iNEXTBetaDiv(beta.spat[[i]], q = c(0, 1, 2), datatype = 'abundance', level = cov, nboot = 0)
    
    cbind(lapply(result, function(y) lapply(1:3, function(i) cbind(y[[i]], div_type = names(y)[i])) %>% do.call(rbind,.)) %>% 
            do.call(rbind,.) %>% filter(level %in% cov | Method == 'Observed'),
          Latitude = names(beta.spat)[i])
  }) %>% do.call(rbind,.)
  
  output.spat$Region = as.numeric(output.spat$Region)
  output.spat = rbind(output.spat, output.spat %>% filter(Method == 'Observed', level %in% cov) %>% mutate('Method' = paste('Observed_', div_type, sep = '')))
  output.spat[output.spat$Method == 'Observed', 'level'] = 'Observed'
  
  list("temporal" = output.temp[,c('Order','level','Region','div_type','Latitude','Estimate')], 
       "spatial" = output.spat[,c('Order','level','Region','div_type','Latitude','Estimate')])
})

stopCluster(cl)


output_fig_5 = list('temporal' = simu_output[[1]]$temporal,
                    'spatial'  = simu_output[[1]]$spatial)
for (i in 2:length(simu_output)) {
  if (sum(simu_output[[i]]$temporal == 'Inf') == 0)
    output_fig_5$temporal = full_join(output_fig_5$temporal, 
                                      simu_output[[i]]$temporal, 
                                      by = c('Order', 'level', 'Region', 'div_type', 'Latitude'))
  
  if (sum(simu_output[[i]]$spatial == 'Inf') == 0)
    output_fig_5$spatial = full_join(output_fig_5$spatial, 
                                     simu_output[[i]]$spatial, 
                                     by = c('Order', 'level', 'Region', 'div_type', 'Latitude'))
}

output_fig_5$temporal = cbind(output_fig_5$temporal[,1:5], 'Estimate' = apply(output_fig_5$temporal[,-(1:5)], 1, mean))
output_fig_5$spatial  = cbind(output_fig_5$spatial[,1:5],  'Estimate' = apply(output_fig_5$spatial[,-(1:5)],  1, mean))

fig_5a(output_fig_5)
fig_5b(output_fig_5, goalC = 0.999)


