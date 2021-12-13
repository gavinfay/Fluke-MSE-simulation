
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



# # Modeling wrapper test
# install.packages("readxl")
# install.packages("tidyr")
# install.packages("reshape2")
# install.packages("splitstackshape")
# install.packages("doBy")
# install.packages("WriteXLS")
# install.packages("Writexl")
# install.packages('Rcpp')
# install.packages("ggplot2")
# install.packages("ggplot")
# install.packages("dplyr")

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

# 
# # Set the wd to wherever all the code/data is 
# 
# #setwd("C:/Users/andrew.carr-harris/Dropbox/NMFS/fluke_mse/simulation_R_code/")
# 
# 
# # Start the clock!
#   ptm <- proc.time()
# 
# 
# ########## 
# # 1) Run the calibration files
# # I've included only the Massachusetts file for now
#  
# source("calibration2 MA.R")
# source("calibration2 RI.R")
# source("calibration2 CT.R")
# source("calibration2 NY.R")
# source("calibration2 NJ.R")
# source("calibration2 DE.R")
# source("calibration2 MD.R")
# source("calibration2 VA.R")
# source("calibration2 NC.R")
#   
# 
# # Combine the results  
# calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
#                                                        pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
#                                                        pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))
# 
# #calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA))
# calibration_output_by_period[is.na(calibration_output_by_period)] = 0
# write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")
# 
# aggregate_calibration_output= subset(calibration_output_by_period, select=-c(state, alt_regs))
# aggregate_calibration_output = aggregate(aggregate_calibration_output, by=list(calibration_output_by_period$sim),FUN=sum, na.rm=TRUE)
# write_xlsx(aggregate_calibration_output,"aggregate_calibration_output.xlsx")
# 
# saveRDS(calibration_output_by_period, file = "calibration_output_by_period.rds")
# saveRDS(aggregate_calibration_output, file = "aggregate_calibration_output.rds")
# 
# ##########  
# costs_all <- NULL
# costs_all[[1]] <- costs_new_all_MA
# costs_all[[2]] <- costs_new_all_RI
# costs_all[[3]] <- costs_new_all_CT
# costs_all[[4]] <- costs_new_all_NY
# costs_all[[5]] <- costs_new_all_NJ
# costs_all[[6]] <- costs_new_all_DE
# costs_all[[7]] <- costs_new_all_MD
# costs_all[[8]] <- costs_new_all_VA
# costs_all[[9]] <- costs_new_all_NC
# saveRDS(costs_all, file = "costs_all.rds")
# 
# ud1 <- data.frame(read_excel("utility_param_draws_MA_NY.xlsx"))
# saveRDS(ud1, file="utility_param_draws_MA_NY.rds")
# ud2 <- data.frame(read_excel("utility_params_draws_NJ.xlsx"))
# saveRDS(ud2, file="utility_param_draws_NJ.rds")
# ud3 <- data.frame(read_excel("utility_params_draws_DE_MD.xlsx"))
# saveRDS(ud3, file="utility_param_draws_DE_MD.rds")
# ud4 <- data.frame(read_excel("utility_params_draws_VA_NC.xlsx"))
# saveRDS(ud4, file="utility_param_draws_VA_NC.rds")
# 
# ##########  
# # Input new population numbers-at-length distribution, run the following script to create catch-at-length for summer flounder
# # For now, this I am using the same catch-at-length distirbutions for the baseline and prediction years 
# 
# # source("catch at length given stock structure - prediction.R")
# ##########  
# 
# 
# 
# 



##########  
# run the simulation code under the new set of regulations (directed_trips_region - alternative regs test.xlsx)
# I've included only the Massachusetts file for now

# Input the data set containing alternative regulations and directed trips (directed_trips_region - alternative regs test.xlsx)
directed_trips_table <- data.frame(read_excel("directed_trips_region - alternative regs test.xlsx"))

# Read-in the current population length composition
size_data_read <- data.frame(read_excel("sf_fitted_sizes_y2plus.xlsx"))

# Read-in current population lenght composition (from sinatra output)
Nlen <- 42
om_length_cm <- scan("om-length.dat",n=Nlen+1)
cm2in <- readr::read_csv("cm2in.csv", col_names = FALSE)
lenbinuse <- as.integer(unlist(cm2in[,1]))
Nlen_in <- length(lenbinuse)
cm2in <- cm2in %>% 
  dplyr::select(-1) %>% 
  as.matrix() %>% 
  I()
om_length_in <- om_length_cm[-1] %*% t(cm2in)
size_data <- data.frame(fitted_prob = rep(om_length_in,3),
                  fitted_length = rep(lenbinuse,3),
                  region = rep(c("SO","NJ","NO"), each = Nlen_in),
                  year = rep("y2",3*Nlen_in))


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
               #size_data_read = rep(list(size_data_read),9),
               size_data_read = rep(list(size_data),9),
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

#prediction_output_by_period <- purrr::map_dfr(xx,1)
prediction_output_by_period <- xx[[1]][[1]]

# 
#prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA))
prediction_output_by_period[is.na(prediction_output_by_period)] = 0
write_xlsx(prediction_output_by_period,"prediction_output_by_period.xlsx")

#aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs))
 aggregate_prediction_output <- dplyr::select(prediction_output_by_period, -state, -alt_regs) %>% 
#   colSums() %>% 
   I()
aggregate_prediction_output = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")

pred_len <- tibble(aggregate_prediction_output) %>% 
  dplyr::select(contains("length")) %>% 
  pivot_longer(cols = 1:ncol(.), names_to = "bin",values_to = "num") %>% 
  separate(bin, into =c("type","len"),sep = "_length_") %>% 
  mutate(len = as.numeric(len)) %>% 
  I()
pred_len
out_lens <- tibble(type = rep(c("release","keep"),each=Nlen_in),
                   len = rep(lenbinuse,2)) %>% 
  left_join(pred_len) %>% 
  replace_na(list(num=0)) %>% 
  I()
out_lens
in2cm <- readr::read_csv("in2cm.csv", col_names = FALSE)[,-1]
keep <- out_lens %>% 
  filter(type == "keep") %>% 
  dplyr::select(num) %>% 
  unlist() %>%
  I()
keep <- keep %*% t(in2cm)
release <- out_lens %>% 
  filter(type == "release") %>% 
  dplyr::select(num) %>% 
  unlist() %>%
  I()
release <- release %*% t(in2cm)
write.table(round(rbind(keep,release),3),file = "rec-catch.out", row.names = FALSE, col.names = FALSE)

  
##########  


# Stop the clock
proc.time() - ptm







