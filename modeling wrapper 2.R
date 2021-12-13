<<<<<<< HEAD

# This is the modeling wrapper 

# Steps in the process


  #1) simulate the fishery for the basleline year (2019) under actual regulations/fishing conditions
      # Requires:
        # a) catch-at-length dist'n. I use MRIP data from 2018 and 2019 to create this dist'n (gamma) for each 
        #    species included in the simualtion (SF, BSB, scup, WF, RD) in the basleline year.
        # b) catch-per-trip distribution. These are based on trips that caught or targeted summer flounder. 
        #    I use 2019 data to create fitted dist'ns for scup, WF, RD. Catch-per-trip distributions for 
        #     SF and BSB are based on copula modeling.
        # c) Sets of utility parameters from each of the four surveys
        # d) Distributions of trip costs derived from the 2017 expenditure survey
        # e) a file containing regulations for each state and species. There are several intra-seasonal periods for 
        #    for each state that differ in  regualtions across species. 
      
        # After running the calibration, retain keep- and release-at-length for summer flounder (#'s), total keep and release 
        # for other species, and number of choice occasions for each state/period. Note that for these outputs, 
        # there will be X estimates based on X draws of utility parameters. 

  #2) Simulate the fishery under alternative regulations and a new catch-at-length distribution for summer flounder
        # a) Create new catch-at-length distributions for summer flounder based on population numbers at length. 
        # a) Calcualte angler welfare/fishing effort changes and changes in catch



# Modeling wrapper test
install.packages("readxl")
install.packages("tidyr")
install.packages("reshape2")
install.packages("splitstackshape")
install.packages("doBy")
install.packages("WriteXLS")
install.packages("Writexl")
install.packages('Rcpp')
install.packages("ggplot2")
install.packages("ggplot")
install.packages("dplyr")

library(Rcpp)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(splitstackshape)
library(doBy)
library(WriteXLS)
library(fitdistrplus)
library(ggplot2)


# Set the wd to wherever all the code/data is 

#setwd("C:/Users/andrew.carr-harris/Dropbox/NMFS/fluke_mse/simulation_R_code/")


# Start the clock!
  ptm <- proc.time()


########## 
# 1) Run the calibration files
# I've included only the Massachusetts file for now
 
source("calibration2 MA.R")
source("calibration2 RI.R")
source("calibration2 CT.R")
source("calibration2 NY.R")
source("calibration2 NJ.R")
source("calibration2 DE.R")
source("calibration2 MD.R")
source("calibration2 VA.R")
source("calibration2 NC.R")
  

# Combine the results  
calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                       pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                       pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))

#calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA))
calibration_output_by_period[is.na(calibration_output_by_period)] = 0
write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")

aggregate_calibration_output= subset(calibration_output_by_period, select=-c(state, alt_regs))
aggregate_calibration_output = aggregate(aggregate_calibration_output, by=list(calibration_output_by_period$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_calibration_output,"aggregate_calibration_output.xlsx")

saveRDS(calibration_output_by_period, file = "calibration_output_by_period.rds")
saveRDS(aggregate_calibration_output, file = "aggregate_calibration_output.rds")

##########  
costs_all <- NULL
costs_all[[1]] <- costs_new_all_MA
costs_all[[2]] <- costs_new_all_RI
costs_all[[3]] <- costs_new_all_CT
costs_all[[4]] <- costs_new_all_NY
costs_all[[5]] <- costs_new_all_NJ
costs_all[[6]] <- costs_new_all_DE
costs_all[[7]] <- costs_new_all_MD
costs_all[[8]] <- costs_new_all_VA
costs_all[[9]] <- costs_new_all_NC
saveRDS(costs_all, file = "costs_all.rds")

ud1 <- data.frame(read_excel("utility_param_draws_MA_NY.xlsx"))
saveRDS(ud1, file="utility_param_draws_MA_NY.rds")
ud2 <- data.frame(read_excel("utility_params_draws_NJ.xlsx"))
saveRDS(ud2, file="utility_param_draws_NJ.rds")
ud3 <- data.frame(read_excel("utility_params_draws_DE_MD.xlsx"))
saveRDS(ud3, file="utility_param_draws_DE_MD.rds")
ud4 <- data.frame(read_excel("utility_params_draws_VA_NC.xlsx"))
saveRDS(ud4, file="utility_param_draws_VA_NC.rds")

##########  
# Input new population numbers-at-length distribution, run the following script to create catch-at-length for summer flounder
# For now, this I am using the same catch-at-length distirbutions for the baseline and prediction years 

# source("catch at length given stock structure - prediction.R")
##########  







##########  
# run the simulation code under the new set of regulations (directed_trips_region - alternative regs test.xlsx)
# I've included only the Massachusetts file for now

# Input the data set containing alternative regulations and directed trips (directed_trips_region - alternative regs test.xlsx)
directed_trips_table <- data.frame(read_excel("directed_trips_region - alternative regs test.xlsx"))

# Read-in the current population length composition
size_data_read <- data.frame(read_excel("sf_fitted_sizes_y2plus.xlsx"))

# Input the calibration output which contains the number of choice occasions needed to simulate
#calibration_data = data.frame(read_excel("calibration_output_by_period.xlsx"))
calibration_data_table <- readRDS("calibration_output_by_period.rds")
#utility parameter draws
ud1 <- readRDS("utility_param_draws_MA_NY.rds")
ud2 <- readRDS("utility_param_draws_NJ.rds")
ud3 <- readRDS("utility_param_draws_DE_MD.rds")
ud4 <- readRDS("utility_param_draws_VA_NC.rds")

#costs
costs_new <- readRDS( "costs_all.rds")

params <- list(state1 = c("MA","RI","CT","NY","NJ","DE","MD","VA","NC"),
               region1 = c(rep("NO",4),"NJ",rep("SO",4)),
               calibration_data_table = rep(list(calibration_data_table),9),
               directed_trips_table = rep(list(directed_trips_table),9),
               size_data_read = rep(list(size_data_read),9),
               param_draws_MANY = c(rep(list(ud1),4),list(ud2),rep(list(ud3),2),rep(list(ud4),2)),
               costs_new_all = costs_new)

params <- list(state1 = "MA",
               region1 = "NO",
               calibration_data_table = list(calibration_data_table),
               directed_trips_table = list(directed_trips_table),
               size_data_read = list(size_data_read),
               param_draws_MANY = list(ud1),
               costs_new_all = list(costs_new[[1]]))



# loop over states
source("prediction2 MA.R")

xx <- predict_rec_catch(state1 = "MA",
                        region1 = "NO",
                        calibration_data_table = calibration_data_table,
                        directed_trips_table = directed_trips_table,
                        size_data_read = size_data_read,
                        param_draws_MANY = ud1,
                        costs_new_all = costs_new[[1]])

safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
xx <- purrr::pmap(params, safe_predict_rec_catch)
#xx <- purrr::pmap_dfr(params, safe_predict_rec_catch)

source("prediction2 RI.R")
source("prediction2 CT.R")
source("prediction2 NY.R")
source("prediction2 NJ.R")
source("prediction2 DE.R")
source("prediction2 MD.R")
source("prediction2 VA.R")
source("prediction2 NC.R")


prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                      pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                      pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))
# 
#prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA))
prediction_output_by_period[is.na(prediction_output_by_period)] = 0
write_xlsx(prediction_output_by_period,"prediction_output_by_period.xlsx")

aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs))
aggregate_prediction_output = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")

##########  


# Stop the clock
proc.time() - ptm







=======

# This is the modeling wrapper 

# Steps in the process


  #1) simulate the fishery for the basleline year (2019) under actual regulations/fishing conditions
      # Requires:
        # a) catch-per-trip distribution. These are based on trips that caught or targeted summer flounder. 
        #    I use 2019 data to create fitted dist'ns for scup, WF, RD. Catch-per-trip distributions for 
        #     SF and BSB are based on copula modeling.
        # b) Sets of utility parameters from each of the four surveys
        # c) Distributions of trip costs derived from the 2017 expenditure survey
        # d) a file containing regulations for each state and species. There are 24 bi-monthly periods for 
        #    for each state. Some periods differ in  regualtions across species. 
      
        # After running the calibration, retain keep- and release-at-length for summer flounder (#'s), total keep and release 
        # for other species, and number of choice occasions for each state/period. Note that for these outputs, 
        # there will be X estimates based on X draws of utility parameters. 

  #2) Simulate the fishery under alternative regulations and a new catch-at-length distribution for summer flounder
        # a) Create new catch-at-length/catch-per-trip distributions for summer flounder based on population numbers at length. 
        # a) Calcualte angler welfare/fishing effort changes and changes in catch



# Modeling wrapper test
install.packages("readxl")
install.packages("tidyr")
install.packages("reshape2")
install.packages("splitstackshape")
install.packages("doBy")
install.packages("WriteXLS")
install.packages("Writexl")
install.packages('Rcpp')
install.packages("ggplot2")
install.packages("ggplot")
install.packages("dplyr")
install.packages("rlist")

install.packages("fitdistrplus")
install.packages("MASS")
install.packages("psych")
install.packages("rgl")
install.packages("copula")
install.packages("VineCopula")
install.packages("readxl")

install.packages("scales")
install.packages("univariateML")
install.packages("xlsx")
install.packages("writexl")
install.packages("logspline")

library(psych)
library(rgl)
library(copula)
library(VineCopula)
library(readxl)
library(scales)
library(univariateML)
library(xlsx)
library(fitdistrplus)
library(logspline)

library(Rcpp)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(splitstackshape)
library(doBy)
library(WriteXLS)
library(rlist)
library(xtable)
library(MASS)
library(stats)
# Set the wd to wherever all the code/data is 

#setwd("C:/Users/andrew.carr-harris/Dropbox/NMFS/fluke_mse/simulation_R_code/")
setwd("C:/Users/Lou/Dropbox/NMFS/fluke_mse/simulation_R_code/")


# Start the clock!
ptm <- proc.time()


########## 
# 1) Run the calibration files

source("calibration4 MA.R")
source("calibration4 RI.R")
source("calibration4 CT.R")
source("calibration4 NY.R")
source("calibration4 NJ.R")
source("calibration4 DE.R")
source("calibration4 MD.R")
source("calibration4 VA.R")
source("calibration4 NC.R")
  

# Combine the results  
calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                       pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                       pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))

#calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA))
calibration_output_by_period[is.na(calibration_output_by_period)] = 0
write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")



aggregate_calibration_output= subset(calibration_output_by_period, select=-c(state, alt_regs, period))
aggregate_calibration_output = aggregate(aggregate_calibration_output, by=list(calibration_output_by_period$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_calibration_output,"aggregate_calibration_output.xlsx")


#Apply the calibration estimates of total catch to the catch-at-length distribution used in the assessment 
tot_sf_keep = aggregate_calibration_output$tot_keep
tot_sf_rel = aggregate_calibration_output$tot_rel
tot_sf_catch = tot_sf_keep+tot_sf_rel

assment_CAL = data.frame(read_excel("assessment_catch_at_length.xlsx"))                                                                            
assment_CAL$calibration_keep_at_length=assment_CAL$ab1_prop*tot_sf_keep
assment_CAL$calibration_release_at_length=assment_CAL$b2_prop*tot_sf_rel

calibration_catch_at_length= subset(assment_CAL, select=c(l_in_bin, calibration_keep_at_length, calibration_release_at_length))

##########  




##########  
# Input new population numbers-at-age distribution (numbers_at_age_YYYY) in the following script to create population adjusted 
# catch-at-length and catch-per-trip for summer flounder
source("catch at length given stock structure - prediction.R")
##########  







##########  
# run the simulation code under the new set of regulations (regulatiopn file is directed_trips_region - alternative regs test.xlsx)

source("prediction3 MA.R")
source("prediction3 RI.R")
source("prediction3 CT.R")
source("prediction3 NY.R")
source("prediction3 NJ.R")
source("prediction3 DE.R")
source("prediction3 MD.R")
source("prediction3 VA.R")
source("prediction3 NC.R")


prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                      pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                      pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))

prediction_output_by_period[is.na(prediction_output_by_period)] = 0
write_xlsx(prediction_output_by_period,"prediction_output_by_period.xlsx")

aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs, period))
aggregate_prediction_output = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")

##########  


# Stop the clock
proc.time() - ptm



###
# Calculate ouput statisitics for calibration and prediction year
source("simulation output stats.R")


>>>>>>> b165bd6027ba6aa8117f6bec01b259f18059e338
