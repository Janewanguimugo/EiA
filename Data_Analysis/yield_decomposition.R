# package names
packages <- c("frontier", "dplyr", "tidyr", "knitr", "car", "RColorBrewer", "DT")

# install packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])}

# load packages
invisible(lapply(packages, library, character.only = TRUE))
################
# read .csv file with data
file <- 'https://raw.githubusercontent.com/jvasco323/EiA_YGD_workflow/main/data-wheat-ethiopia.csv'
data <- read.csv(url(file))

# list variables of interest
str(data)

####################
unique(data$oxplough_freq_cat)
# create final data
data <- subset(data, yield_tha > 0)
data <- subset(data, residues_yn == "No" | residues_yn == "Yes")
data <- subset(data, soil_slope == "Flat" | soil_slope == "Medium" | soil_slope == "Steep")
data <- subset(data, zone_new != "")
data <- subset(data, oxplough_freq_cat == "<Two" | 
                 oxplough_freq_cat == "Three" | 
                 oxplough_freq_cat == "Four" | 
                 oxplough_freq_cat == ">Five")
data <- subset(data, weeding_freq_cat == "None" | 
                 weeding_freq_cat == "One" | 
                 weeding_freq_cat == "Two" | 
                 weeding_freq_cat == "Three+")

glimpse(data)
skimr::skim(data)
# fill NA values
data$seed_kgha[is.na(data$seed_kgha)] <- mean(data$seed_kgha, na.rm=T)
data$nfert_kgha[is.na(data$nfert_kgha)] <- 0
data$herb_lha[is.na(data$herb_lha)] <- 0
data$handweeding_persdayha[is.na(data$handweeding_persdayha)] <- 0

unique(data$farming_system)
# reclassify categorical variables
data$variety = ifelse(data$variety != 'Landrace' &  data$variety != 'unknown', 'Improved', data$variety)
data$variety = ifelse(data$variety == 'Landrace', 'unknown', data$variety)
data$nfert_yn = ifelse(data$nfert_kgha == 0, 'N0', 'N+')
data$weeding_yn = ifelse(data$herb_lha == 0 & data$handweeding_persdayha == 0, 'No', 'Yes')

# copy df with transformed data
data_new <- data

# replace 0 with small value for log-transformation
data_new[data_new == 0] = 0.0001

# log-transform continuous variables
vars1 <- c('gyga_gdd', 'gyga_tseas', 'seed_kgha', 'gyga_ai', 'gyga_av_water', 'nfert_kgha', 'pfert_kgha',
           'herb_lha', 'handweeding_persdayha', 'yield_tha')
log_f <- function(x){log(x)}
data_new[,vars1] <- lapply(data_new[,vars1], log_f)

# set categorical variables to factor
vars2 <- c('farming_system', 'aez', 'zone_new', 'season_year', 'variety', 'soil_depth', 'soil_fertility',
           'waterlogging_yn', 'drought_yn', 'soilwatercons_yn', 'manure_yn', 'residues_yn', 'previous_crop',
           'oxplough_freq_cat', 'weeding_yn', 'pesticide_yn', 'disease_incidence_yn', 'pest_incidence_yn')
data_new[,vars2] <- lapply(data_new[,vars2], factor)


#########


# fit ols regression model
ols <- 
  lm(yield_tha ~ 
       season_year + gyga_gdd + gyga_tseas + seed_kgha + variety +
       gyga_ai + gyga_av_water + soil_depth + soil_fertility + waterlogging_yn + drought_yn + soilwatercons_yn + 
       nfert_kgha + manure_yn + residues_yn + previous_crop + oxplough_freq_cat +                                
       herb_lha + handweeding_persdayha + weeding_yn + pesticide_yn + disease_incidence_yn + pest_incidence_yn,  
     data=data_new)

# check vif values
 vif(ols)

# see parameter estimates
 summary(ols)


##########################################
# fit cobb-douglas stochastic frontier
sfa_cd <- 
  sfa(yield_tha ~ 
        season_year + gyga_gdd + gyga_tseas + seed_kgha + variety +
        gyga_ai + gyga_av_water + soil_depth + soil_fertility + waterlogging_yn + drought_yn + soilwatercons_yn +
        nfert_kgha + manure_yn + residues_yn + previous_crop + oxplough_freq_cat +
        herb_lha + handweeding_persdayha + weeding_yn + pesticide_yn + disease_incidence_yn + pest_incidence_yn,
      data=data_new)

# add technical efficiency score to data frame
data_new$te_score_cd = efficiencies(sfa_cd, asInData=T)

# see parameter estimates
summary(sfa_cd)

##############################################
# fit translog stochastic frontier    Q1 how did we choose what goes in the 2nd order terms
sfa_tl <- 
  sfa(yield_tha ~ 
        
        # 1st order terms (linear)
        season_year + gyga_gdd + gyga_tseas + seed_kgha + variety +
        gyga_ai + gyga_av_water + soil_depth + soil_fertility + waterlogging_yn + drought_yn + soilwatercons_yn +
        nfert_kgha + manure_yn + residues_yn + previous_crop + oxplough_freq_cat +
        herb_lha + handweeding_persdayha + weeding_yn + pesticide_yn + disease_incidence_yn + pest_incidence_yn +
        
        # 2nd order terms (squared)
        I(0.5*gyga_gdd^2) + I(0.5*gyga_tseas^2) + I(0.5*seed_kgha^2) +
        I(0.5*gyga_ai^2) + I(0.5*gyga_av_water^2) +
        I(0.5*nfert_kgha^2) +
        I(0.5*herb_lha^2) + I(0.5*handweeding_persdayha^2) +
        
        # 2nd order terms (interactions) 
        I(gyga_gdd*gyga_tseas) + I(gyga_gdd*seed_kgha) + I(gyga_gdd*gyga_ai) + I(gyga_gdd*gyga_av_water) +
        I(gyga_gdd*nfert_kgha) + I(gyga_gdd*herb_lha) + I(gyga_gdd*handweeding_persdayha) +
        I(gyga_tseas*seed_kgha) + I(gyga_tseas*gyga_ai) + I(gyga_tseas*gyga_av_water) + I(gyga_tseas*nfert_kgha) +
        I(gyga_tseas*herb_lha) + I(gyga_tseas*handweeding_persdayha) +
        I(seed_kgha*gyga_ai) + I(seed_kgha*gyga_av_water) + I(seed_kgha*nfert_kgha) + I(seed_kgha*herb_lha) +
        I(seed_kgha*handweeding_persdayha) +
        I(gyga_ai*gyga_av_water) + I(gyga_ai*nfert_kgha) + I(gyga_ai*herb_lha) + I(gyga_ai*handweeding_persdayha) +
        I(gyga_av_water*nfert_kgha) + I(gyga_av_water*herb_lha) + I(gyga_av_water*handweeding_persdayha) +
        I(nfert_kgha*herb_lha) + I(nfert_kgha*handweeding_persdayha) +
        I(herb_lha*handweeding_persdayha),
      data=data_new)

# add technical efficiency score to data frame
data_new$te_score_tl = efficiencies(sfa_tl, asInData=T)

# see parameter estimates
# summary(sfa_tl)

################################
# estimate efficiency yield gap (%)
data_new['efficiency_yg'] = 100 - (data_new['te_score_cd'] * 100)

# select relevant columns
data_new <- data_new[c('zone_new', 'season_year', 'hhid', 'plotid', 'subplotid', 'te_score_cd', 'te_score_tl', 
                       'efficiency_yg')]

# merge the new columns to original data frame
data <- merge(data, data_new, by=c('zone_new', 'season_year', 'hhid', 'subplotid'), all.x=T)

# estimate technical efficiency yield (t/ha)
data['ytex_tha'] = data['yield_tha'] / data['te_score_cd']

##############################
# create an empty data frame
data_final <- data.frame()

unique(data$year)
# create loop per year
for(yr in unique(data$year)){
  subset_year <- subset(data, year == yr)
  
  # create loop per climate zone
  for(cz in unique(subset_year$gyga_cz)){
    subset_cz <- subset(subset_year, gyga_cz == cz)
    
    # create loop per soil type
    for(soil in unique(subset_cz$soil_fertility)){
      subset_soil <- subset(subset_cz, soil_fertility == soil)
      
      # create column with field class based on yield distribution
      subset_soil$field_class <- ifelse(subset_soil$yield_tha >= quantile(subset_soil$yield_tha, 0.90), 
                                        'YHF', '')
      subset_soil$field_class <- ifelse(subset_soil$yield_tha <= quantile(subset_soil$yield_tha, 0.10), 
                                        'YLF', subset_soil$field_class)
      subset_soil$field_class <- ifelse(subset_soil$yield_tha > quantile(subset_soil$yield_tha, 0.10) & 
                                          subset_soil$yield_tha < quantile(subset_soil$yield_tha, 0.90), 
                                        'YAF', subset_soil$field_class)
      
      # subset highest yielding fields only
      yhf <- subset(subset_soil, field_class == 'YHF')
      
      # add column with yhf in t/ha to data frame
      subset_soil['yhf_tha'] <- mean(yhf$yield_tha, na.rm=T)
      
      # bind all individual fields into single data frame
      data_final <- rbind(data_final, subset_soil)
    }}}

##########################################
# load dataframe with yw data
file <- 'https://raw.githubusercontent.com/jvasco323/EiA_YGD_workflow/main/data-gps-coordinates-final.csv'
yw_data <- read.csv(url(file))
yw_data <- yw_data[c('hhid', 'GYGA_CZ', 'Yw_average', 'Yw_2009', 'Yw_2013')]
yw_data <- unique(yw_data)

glimpse(data_final)
# merge yw data with the rest of the data
data_final <- merge(data_final, yw_data, by='hhid', all.x=T)

# get yw per field
data_final$yw_tha <- ifelse(data_final$year == 2009, data_final$Yw_2009, data_final$Yw_2013)

# summarize of yw data
summary_yw <- unique(data_final[c('GYGA_CZ', 'Yw_average', 'Yw_2009', 'Yw_2013')])
summary_yw <- aggregate(summary_yw[c(2:4)], by=list('GYGA_CZ'=summary_yw$GYGA_CZ), FUN=mean)
summary_yw[c(2:4)] <- round(summary_yw[c(2:3)], 1)
colnames(summary_yw)[1] <- 'Climate zone'
colnames(summary_yw)[2] <- 'Yw long-term (t/ha)'
colnames(summary_yw)[3] <- 'Yw 2009 (t/ha)'
colnames(summary_yw)[4] <- 'Yw 2013 (t/ha)'
DT::datatable(summary_yw,
              options=list(columnDefs=list(list(className='dt-center')), 
                           pageLength=5, 
                           lengthMenu=c(5, 10, 15)))
###########################################################

# total yield gap in t/ha
data_final['yg_total_2009'] <- data_final['Yw_2009'] - data_final['yield_tha']
data_final['yg_total_2013'] <- data_final['Yw_2013'] - data_final['yield_tha']
data_final['yg_total'] <- ifelse(data_final$year == 2009, data_final$yg_total_2009, 
                                 data_final$yg_total_2013)

# efficiency yield gap in t/ha
data_final['eff_yg_tha_2009'] <- data_final['ytex_tha'] - data_final['yield_tha']
data_final['eff_yg_tha_2013'] <- data_final['ytex_tha'] - data_final['yield_tha']
data_final['eff_yg_tha'] <- ifelse(data_final$year == 2009, data_final$eff_yg_tha_2009, 
                                   data_final$eff_yg_tha_2013)

# resource yield gap in t/ha
data_final['res_yg_tha_2009'] <- data_final['yhf_tha'] - data_final['ytex_tha']
data_final['res_yg_tha_2013'] <- data_final['yhf_tha'] - data_final['ytex_tha']
data_final['res_yg_tha'] <- ifelse(data_final$year == 2009, data_final$res_yg_tha_2009, 
                                   data_final$res_yg_tha_2013)

# technology yield gap in t/ha
data_final['tech_yg_tha_2009'] <- data_final['Yw_2009'] - data_final['yhf_tha']
data_final['tech_yg_tha_2013'] <- data_final['Yw_2013'] - data_final['yhf_tha']
data_final['tech_yg_tha'] <- ifelse(data_final$year == 2009, data_final$tech_yg_tha_2009, 
                                    data_final$tech_yg_tha_2013)

# aggregate absolute yield gaps by zone
absolute <- aggregate(data_final[c('yield_tha', 'eff_yg_tha', 'res_yg_tha', 'tech_yg_tha')], by=list('zone_new'=data_final$zone_new), FUN=mean, na.rm=T)
absolute_t <- t(absolute)
colnames(absolute_t) <- absolute$zone_new
absolute_t <- absolute_t[-1, ]

# make barplot
pal <- rev(palette(brewer.pal(n=4, name="Blues")))
pal <- rev(palette(brewer.pal(n=4, name="Blues")))
par(mfrow=c(1,1), mar=c(7,5,4,9), yaxs='i')
{barplot(absolute_t, 
         las=2, 
         cex.lab=1.1,
         ylim=c(0, 10),
         ylab='Yield and yield gaps (t/ha)',
         main='Yield gap decomposition for wheat in Ethiopia (t/ha)',
         col=pal) 
  abline(h=9, col="black", lty=2)
  legend("topright", 
         inset=c(-0.325, 0),
         legend=c("Technology Yg", "Resource Yg", "Efficiency Yg", "Actual yield"), 
         fill=c(palette(brewer.pal(n=4, name="Blues"))), 
         xpd=TRUE)
  box()}

####################################################################################
# yield gap closure relative to yw
data_final['yg_closure_2009'] <- (data_final['yield_tha'] / data_final['Yw_2009']) * 100
data_final['yg_closure_2013'] <- (data_final['yield_tha'] / data_final['Yw_2013']) * 100
data_final['yg_closure'] <- ifelse(data_final$year == 2009, data_final$yg_closure_2009, 
                                   data_final$yg_closure_2013)

# ytex relative to yw
data_final['eff_yg_2009'] <- (data_final['ytex_tha'] / data_final['Yw_2009']) * 100
data_final['eff_yg_2013'] <- (data_final['ytex_tha'] / data_final['Yw_2013']) * 100
data_final['ytex_closure'] <- ifelse(data_final$year == 2009, data_final$eff_yg_2009, 
                                     data_final$eff_yg_2013)

# yhf relative to yw
data_final['res_yg_2009'] <- (data_final['yhf_tha'] / data_final['Yw_2009']) * 100
data_final['res_yg_2013'] <- (data_final['yhf_tha'] / data_final['Yw_2013']) * 100
data_final['yhf_closure'] <- ifelse(data_final$year == 2009, data_final$res_yg_2009, 
                                    data_final$res_yg_2013)

# intermediate yield gaps
data_final$eff_yg <- data_final$ytex_closure - data_final$yg_closure
data_final$res_yg <- data_final$yhf_closure - data_final$ytex_closure
data_final$tech_yg <- 100 - data_final$yhf_closure

# aggregate absolute yield gaps by zone
relative <- aggregate(data_final[c('yg_closure', 'eff_yg', 'res_yg', 'tech_yg')], by=list('zone_new'=data_final$zone_new), FUN=mean, na.rm=T)
relative_t <- t(relative)
colnames(relative_t) <- relative$zone_new
relative_t <- relative_t[-1, ]

# make barplot
par(mfrow=c(1,1), mar=c(7,5,4,9), yaxs='i')
{barplot(relative_t, 
         las=2, 
         cex.lab=1.1,
         ylim=c(0, 100),
         ylab='Yield gap closure (% of Yw)',
         main='Yield gap decomposition for wheat in Ethiopia (%)',
         col=pal) 
  abline(h=50, col="black", lty=2)
  abline(h=80, col="black", lty=1)
  legend("topright", 
         inset=c(-0.325, 0),
         legend=c("Technology Yg", "Resource Yg", "Efficiency Yg", "Actual yield"), 
         fill=c(palette(brewer.pal(n=4, name="Blues"))),
         xpd=T)
  box()}


