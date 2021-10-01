
rm(list = ls(all=TRUE))



#heading <- "
#------------------------------------------------------------------------------------
#
# PROJECT:      COVID-19 modelling: Children and vaccination (12-15 years)
# CONTACT:      Lisa Brouwers
# EMAIL:        analysenheten@folkhalsomyndigheten.se
# AFFILIATION:  FD-AN
#
# Created:      2021-09-01
# R version:   	3.6.2
#
# What the script does: Simulates the two scenarios 1. no vaccination and 2. vaccination
#                       of children aged 12-15 years in Sweden. A detailed description can 
#                       be found in the technical report mentioned in the 'read_me' file. 
#                       In the report four different set-ups are used: 
#                         1. Contacts increase by 50% in october and vaccination takes 4 months 
#                         2. Contacts increase by 50% in october and vaccination takes 2.5 months
#                         3. Contacts stay the same as september value and vaccination takes 4 months
#                         4. Contacts stay the same as september value and vaccination takes 2.5 months
#                       To produce all results found in report, parameter values need to be 
#                       changed in section "5.  Set scenario parameters".
#               
#               
#------------------------------------------------------------------------------------
#\n"






#-------------#
# Disposition #
#-------------#

# 1.  Some important notes on notations
# 2.  Paths (You need to create your own, see section)
# 3.  Libraries
# 4.  Read incidence and population data
# 5.  Set scenario parameters
# 6.  Fixed values and parameters (fraction reported, antibody levels, and vaccination levels)
# 7.  Extract data used as starting point used in simulation
# 8.  Contact Matrix, transmission model parameters, and Infectivity
# 9.  Find infectivity at beginning of pandemic and at week 35
# 10. Seeding of main model
# 11. Find rate of vaccination (of youths aged 12-15 years)
# 12. Main model and simulation
#     - Scenario with no vaccination
#     - Scenario with vaccination
# 13. Create and save result tables
# 14. Save figures



#-----------------------------------------------------------------------------------
# 1. Some notes on notations 
#-----------------------------------------------------------------------------------

## Use a SEIR compartment model with five age groups.


#### Compartments ###
##
## S        := Susceptible 
## E        := Exposed, infected but not infectious. 
## I_symp   := Infected case that becomes reported
## I_asymp  := Infected case that not becomes reported, i.e. unreported case.
## V        := Vaccinated 12-15 year olds.
## R        := recovered/immune compartment. Those older than 15 and vaccinated assumed to be in this compartment
## 
####


#### Rates and parameters specified ###
##
## eta := rate of leaving incubation/exposed period. Denoted rho in report.
## gamma_D := rate of leaving the infectious compartment, i.e. 1/gamma_D = mean time infectious
##
####

#### Parameters determined by immunity level and reproduction number  ###
##
## infectivity := probability of infecting a susceptible contact per meeting 
## (average numbers of contacts per day is 10)
##
####


#-----------------------------------------------------------------------------------
# 2. Paths
#-----------------------------------------------------------------------------------

# Input your own path instead of "YOUR_PATH" where folder is located in project.path (end with "/").
# e.g. project.path 	     <- "C:/Users/Your_Name/Modelling/"

project.path  <- "YOUR_PATH"
data.path     <- paste(project.path, "Data", sep="")
output.path   <- paste(project.path, "Output",sep="")
script.path   <- paste(project.path, "Script",sep="")





#---------------------------------------------------------------------------------------------------
# 3. Libraries
#---------------------------------------------------------------------------------------------------



library(openxlsx)
require(deSolve)
require(rootSolve) 


#---------------------------------------------------------------------------------------------------
# 4. Read incidence and population data
#---------------------------------------------------------------------------------------------------


## Incidence data. 

# Actual reported cases for the age groups of the model: 0-11, 12-15, 16-17, 18-69, and 70+
incidence_age <- read.xlsx(paste(data.path,"/antal_fall_riket_v1.xlsx",sep="")) 
incidence_age$Statistikdatum <- as.Date(incidence_age$Statistikdatum,  origin = "1899-12-30")



# Estimated number of reported cases from 03 sept 2021 to 30 sept 2021 from "Scenario 0" in "Löpande modellering - delrapport 5".
# https://www.folkhalsomyndigheten.se/publicerat-material/publikationsarkiv/s/scenarier-for-fortsatt-spridning--delrapport-5/
# Scenario 0 is the scenario that best has followed the number of reported cases.
# The age groups of these estimated number of reported cases are: 0-19, 20-69, and 70+.


incidence_est <- read.xlsx(paste(data.path,"/Riket_Scenario_0.xlsx",sep=""))
Dates_est <- as.Date(as.numeric(incidence_est$Time), origin ="2020-01-01")



# Actual reported cases for other age groups: 0-11, 12-15, 16-17, 18-19, 20-69, and 70+.
# According to the mean proportion as seen from these cases (from 24 may to 3 sept), 
# we distribute the cases estimated from incidence_est that comes in 0-19, 20-69, and 70+ into our age groups in this modelling.

incidence_age_for_virsim <- read.xlsx(paste(data.path,"/antal_fall_riket_v2.xlsx",sep="")) 
incidence_age_for_virsim$Statistikdatum <- as.Date(incidence_age_for_virsim$Statistikdatum,  origin = "1899-12-30")



## Population data
# Population data from size 2020

pop_size <- read.xlsx(paste(data.path,"/Population_0-11_12-15_16-17_18-69_70+.xlsx",sep="")) 

group_names <- c("0-11", "12-15", "16-17", "18-69", "70+")
group_names <- pop_size$Agegroup


N_0_11  <- pop_size$Pop[1]
N_12_15 <- pop_size$Pop[2]
N_16_17 <- pop_size$Pop[3]
N_18_69 <- pop_size$Pop[4]
N_70    <- pop_size$Pop[5]

N_all <- c(N_0_11, N_12_15, N_16_17, N_18_69, N_70)
names(N_all) <- group_names
N <- sum(N_all)

age_prop <- N_all/N


### Age groups for incidence_age_for_virsim
N_0_19  <- 2397774
N_20_69 <- 6413764
###


#-----------------------------------------------------------------------------------
# 5. Set scenario parameters
#-----------------------------------------------------------------------------------


#
#--- Choose set-up for simulation
#

#VE two doses
Vacc_eff    <- 0.7 # 0.950 # 0.7
Vacc_eff_70 <- 0.7 # 0.894 # 0.7

Vacc_eff_12_15 <- Vacc_eff

# VE one dose (same but then reduced by 'reduction_one_dose')
Vacc_eff_one_dose    <- 0.7 # 0.893 # 0.7
Vacc_eff_one_dose_70 <- 0.7 # 0.846 # 0.7
reduction_one_dose   <- (1-0.5) # (1-0.34) # (1-0.5) 



# assumed (effective) coverage 1st October
Vacc_16_17 <- 0.750 * Vacc_eff
Vacc_18_69 <- 0.750 * Vacc_eff  
Vacc_70    <- 0.922 * Vacc_eff_70 


# Vaccine coverage 12-15
coverage <- 0.75

# Assumed time to to reach assumed vaccine coverage of youths 
time_to_vacc_youth <- 120 # 120 # 78

# Time (days) from vaccination to effect
delayed_response <- 3*7 

# Increase in infectioussness (e.g. increased contacts) from 1 October based on end of aug beginning of sept
increse_from_aug <- 1.5 # 1.0 # 1.5

# Number of days to simulate spread from 1 October
number_of_days <- 200


#-----------------------------------------------------------------------------------
# 6. Fixed values and parameters (fraction reported, antibody levels, and vaccination levels)
#-----------------------------------------------------------------------------------



# Fraction reported
p_symp_0_11  <- 0.09520353  
p_symp_12_15 <- 0.264967
p_symp_16_17 <- 0.3694089
p_symp_18_69 <- 0.4051672
p_symp_70    <- 0.7

# Vacc_eff_w20 symbolise what fraction of vaccinated that developed antibodies after two doses. 
# Vacc_eff_one_dose_w20 symbolise what fraction of vaccinated that developed antibodies after one dose. 

Vacc_eff_w20    <- 0.95
Vacc_eff_w20_70 <- 0.894

Vacc_eff_w20_one_dose     <- 0.893 
Vacc_eff_w20_one_dose_70  <- 0.846


#
# immunity at week 21-22 (2021) minus 2 weeks, i.e. week 19-20


# https://www.folkhalsomyndigheten.se/publicerat-material/publikationsarkiv/p/pavisning-av-antikroppar-efter-genomgangen-covid-19-i-blodprov-fran-oppenvarden-delrapport-1/
Imm_w20_0_11  <- 0.218 # 0-10  
Imm_w20_12_15 <- 0.344 # 11-15  
Imm_w20_16_17 <- 0.368 # 16-19
Imm_w20_18_69 <- 0.520 # 20-64
Imm_w20_70    <- 0.850 # 65+


# How many vaccinated at week 20
# At least one dose
N_forsta_dos_w20_0_11  <- 0
N_forsta_dos_w20_12_15 <- 0
N_forsta_dos_w20_16_17 <- 0
N_forsta_dos_w20_18_69 <- 0.320 * N_18_69
N_forsta_dos_w20_70    <- 0.927 * N_70

# Two doses
N_andra_dos_w20_0_11  <- 0
N_andra_dos_w20_12_15 <- 0
N_andra_dos_w20_16_17 <- 0
N_andra_dos_w20_18_69 <- 0.0595 * N_18_69
N_andra_dos_w20_70    <- 0.5320 * N_70


## Only one dose
N_endast_en_dos_w20_0_11  <- N_forsta_dos_w20_0_11 - N_andra_dos_w20_0_11 
N_endast_en_dos_w20_12_15 <- N_forsta_dos_w20_12_15 - N_andra_dos_w20_12_15 
N_endast_en_dos_w20_16_17 <- N_forsta_dos_w20_16_17 - N_andra_dos_w20_16_17 
N_endast_en_dos_w20_18_69 <- N_forsta_dos_w20_18_69 - N_andra_dos_w20_18_69 
N_endast_en_dos_w20_70    <- N_forsta_dos_w20_70 - N_andra_dos_w20_70 


# How many vaccinated by week 33
# At least one dose
N_forsta_dos_w33_0_11   <- 0
N_forsta_dos_w33_12_15  <- 0
N_forsta_dos_w33_16_17  <- 0.43 * N_16_17  
N_forsta_dos_w33_18_69  <- 0.771 * N_18_69
N_forsta_dos_w33_70     <- 0.945 * N_70 


# Two doses
N_andra_dos_w33_0_11  <- 0
N_andra_dos_w33_12_15 <- 0
N_andra_dos_w33_16_17 <- 0.0043 * N_16_17 
N_andra_dos_w33_18_69 <- 0.576 * N_18_69 
N_andra_dos_w33_70    <- 0.922 * N_70 


# Only one dose
N_enbart_en_dos_w33_0_11  <- 0
N_enbart_en_dos_w33_12_15 <- 0
N_enbart_en_dos_w33_16_17 <- N_forsta_dos_w33_16_17 - N_andra_dos_w33_16_17
N_enbart_en_dos_w33_18_69 <- N_forsta_dos_w33_18_69 - N_andra_dos_w33_18_69
N_enbart_en_dos_w33_70    <- N_forsta_dos_w33_70 - N_andra_dos_w33_70



#-----------------------------------------------------------------------------------
# 7. Extract data used as starting point used in simulation
#-----------------------------------------------------------------------------------



#
#--- Case data from 24 May to september 03 and estimated number of reported cases from 4 september to 30 september
#


## MAY 34 - SEPT 03
start_date <- which(incidence_age$Statistikdatum == "2021-05-24")
last_date  <- which(incidence_age$Statistikdatum == "2021-09-03")


# Sum of reported cases from 24 may (week 21) to 3 sept (end of week 35) 
casesw21tow35_0_11   <- sum(incidence_age$X0.11[start_date:last_date]) 
casesw21tow35_12_15  <- sum(incidence_age$X12.15[start_date:last_date]) 
casesw21tow35_16_17  <- sum(incidence_age$X16.17[start_date:last_date]) 
casesw21tow35_18_69  <- sum(incidence_age$X18.69[start_date:last_date]) 
casesw21tow35_70     <- sum(incidence_age$X70[start_date:last_date]) 

# Proportion of each age group that became a case between 24 may to 3 sept
prop_new_cases_0_11   <- casesw21tow35_0_11 / N_0_11
prop_new_cases_12_15  <- casesw21tow35_12_15 / N_12_15
prop_new_cases_16_17  <- casesw21tow35_16_17 / N_16_17
prop_new_cases_18_69  <- casesw21tow35_18_69 / N_18_69
prop_new_cases_70     <- casesw21tow35_70 / N_70




## SEPT 04 - SEPT 30
## Scale to all cases, not only reported
Sept04toSept30 <- c(which(Dates_est == "2021-09-04"):which(Dates_est == "2021-09-30"))

incidence_est_0_19_rep  <- incidence_est$Incidence_reported_cases_original_alpha_0_19 + incidence_est$Incidence_reported_cases_delta_0_19
incidence_est_0_19      <- incidence_est_0_19_rep[Sept04toSept30] / ((p_symp_0_11*N_0_11+p_symp_12_15*N_12_15+p_symp_16_17*N_16_17)/(N_0_11+N_12_15+N_16_17))


incidence_est_20_69_rep  <- incidence_est$Incidence_reported_cases_original_alpha_20_69 + incidence_est$Incidence_reported_cases_delta_20_69
incidence_est_20_69      <- incidence_est_20_69_rep[Sept04toSept30] / p_symp_18_69


incidence_est_70_rep  <- incidence_est$Incidence_reported_cases_original_alpha_70_plus + incidence_est$Incidence_reported_cases_delta_70_plus
incidence_est_70      <- incidence_est_70_rep[Sept04toSept30] / p_symp_70


# Distribute estimated cases of 0-69 among the subgroups 0-14, 15-19, 
# and 20-69 according to the mean proportion as seen from 
# cases from 24 may to 3 sept.

cases_for_virsim_w21tow33_0_11   <- sum(incidence_age_for_virsim$X0.11[start_date:last_date]) 
cases_for_virsim_w21tow33_12_15  <- sum(incidence_age_for_virsim$X12.15[start_date:last_date]) 
cases_for_virsim_w21tow33_16_17  <- sum(incidence_age_for_virsim$X16.17[start_date:last_date]) 
cases_for_virsim_w21tow33_18_19  <- sum(incidence_age_for_virsim$X18.19[start_date:last_date]) 
cases_for_virsim_21tow33_20_69   <- sum(incidence_age_for_virsim$X20.69[start_date:last_date]) 



incidence_est_0_11  <- incidence_est_0_19* cases_for_virsim_w21tow33_0_11/
  (cases_for_virsim_w21tow33_0_11 + cases_for_virsim_w21tow33_12_15 + cases_for_virsim_w21tow33_16_17 + cases_for_virsim_w21tow33_18_19)

incidence_est_12_15 <- incidence_est_0_19 * cases_for_virsim_w21tow33_12_15/
  (cases_for_virsim_w21tow33_0_11 + cases_for_virsim_w21tow33_12_15 + cases_for_virsim_w21tow33_16_17 + cases_for_virsim_w21tow33_18_19)

incidence_est_16_17 <- incidence_est_0_19 * cases_for_virsim_w21tow33_16_17/
  (cases_for_virsim_w21tow33_0_11 + cases_for_virsim_w21tow33_12_15 + cases_for_virsim_w21tow33_16_17 + cases_for_virsim_w21tow33_18_19)

incidence_est_18_69 <- incidence_est_20_69 + incidence_est_0_19 * cases_for_virsim_w21tow33_18_19 / 
  (cases_for_virsim_w21tow33_0_11 + cases_for_virsim_w21tow33_12_15 + cases_for_virsim_w21tow33_16_17 + cases_for_virsim_w21tow33_18_19)

 
# How large proportion of each age group infected between week 
# 36 (or end of week 35, but hey, it's just a variable name) to week 39
prop_new_cases_w36_to_w39_0_11   <- sum(incidence_est_0_11) /N_0_11
prop_new_cases_w36_to_w39_12_15  <- sum(incidence_est_12_15)/N_12_15
prop_new_cases_w36_to_w39_16_17  <- sum(incidence_est_16_17)/N_16_17
prop_new_cases_w36_to_w39_20_69  <- sum(incidence_est_18_69)/N_18_69
prop_new_cases_w36_to_w39_70     <- sum(incidence_est_70)/N_70



#
#--- Immunity level at the end of week 35 (3 september); 
#--- i.e. those infected up to week 35 and those (effectively) vaccinated up to week 33.
#


# Fraction of each agegroup that are immune from infection by 3 september. 
# Use antibody surveys symbolising fraction infected until week 20 as basis.
# Need to remove those vaccinated by then. 
# Vacc_eff_w20 symbolise what fraction of vaccinated that developed antibodies after two doses. 
# Vacc_eff_one_dose_w20 symbolise what fraction of vaccinated that developed antibodies after one dose. 


# immune from infection
Imm_w35_0_11  <- Imm_w20_0_11  - (N_andra_dos_w20_0_11*Vacc_eff_w20  + N_endast_en_dos_w20_0_11*Vacc_eff_w20_one_dose)/N_0_11   + prop_new_cases_0_11/p_symp_0_11   
Imm_w35_12_15 <- Imm_w20_12_15 - (N_andra_dos_w20_12_15*Vacc_eff_w20 + N_endast_en_dos_w20_12_15*Vacc_eff_w20_one_dose)/N_12_15 + prop_new_cases_12_15/p_symp_12_15 
Imm_w35_16_17 <- Imm_w20_16_17 - (N_andra_dos_w20_16_17*Vacc_eff_w20 + N_endast_en_dos_w20_16_17*Vacc_eff_w20_one_dose)/N_16_17 + prop_new_cases_16_17/p_symp_16_17      
Imm_w35_18_69 <- Imm_w20_18_69 - (N_andra_dos_w20_18_69*Vacc_eff_w20 + N_endast_en_dos_w20_18_69*Vacc_eff_w20_one_dose)/N_18_69 + prop_new_cases_18_69/p_symp_18_69       
Imm_w35_70    <- Imm_w20_70    - (N_andra_dos_w20_70*Vacc_eff_w20_70 + N_endast_en_dos_w20_70*Vacc_eff_w20_one_dose_70)/N_70    + prop_new_cases_70/p_symp_70          


# Fraction immune from vaccination by week 33
# Correct for efficacy against infection of delta
Vacc_16_17_w33  <- (N_andra_dos_w33_16_17*Vacc_eff + N_enbart_en_dos_w33_16_17*Vacc_eff_one_dose*reduction_one_dose)/N_16_17
Vacc_18_69_w33  <- (N_andra_dos_w33_18_69*Vacc_eff + N_enbart_en_dos_w33_18_69*Vacc_eff_one_dose*reduction_one_dose)/N_18_69
Vacc_70_w33     <- (N_andra_dos_w33_70*Vacc_eff + N_enbart_en_dos_w33_70*Vacc_eff_one_dose_70*reduction_one_dose)/N_70


# Proportion in each age group immune either from infection or vaccination
# Used in calculations of infection rate
Immune_prop_w35  <- c(Imm_w35_0_11, 
                      Imm_w35_12_15, 
                      Vacc_16_17_w33 + (1 - Vacc_16_17_w33) * Imm_w35_16_17, 
                      Vacc_18_69_w33 + (1 - Vacc_18_69_w33) * Imm_w35_18_69, 
                      Vacc_70_w33    + (1 - Vacc_70_w33) * Imm_w35_70)



#
#--- Assumed immunity level at week 39 (30 september); 
#--- i.e. those infected and those (effectively) vaccinated at week 39.
#

# immune from infection
Imm_w39_0_11  <- Imm_w20_0_11  - (N_andra_dos_w20_0_11*Vacc_eff_w20  + N_endast_en_dos_w20_0_11*Vacc_eff_w20_one_dose)/N_0_11   + prop_new_cases_0_11/p_symp_0_11   + prop_new_cases_w36_to_w39_0_11
Imm_w39_12_15 <- Imm_w20_12_15 - (N_andra_dos_w20_12_15*Vacc_eff_w20 + N_endast_en_dos_w20_12_15*Vacc_eff_w20_one_dose)/N_12_15 + prop_new_cases_12_15/p_symp_12_15 + prop_new_cases_w36_to_w39_12_15
Imm_w39_16_17 <- Imm_w20_16_17 - (N_andra_dos_w20_16_17*Vacc_eff_w20 + N_endast_en_dos_w20_16_17*Vacc_eff_w20_one_dose)/N_16_17 + prop_new_cases_16_17/p_symp_16_17 + prop_new_cases_w36_to_w39_16_17
Imm_w39_18_69 <- Imm_w20_18_69 - (N_andra_dos_w20_18_69*Vacc_eff_w20 + N_endast_en_dos_w20_18_69*Vacc_eff_w20_one_dose)/N_18_69 + prop_new_cases_18_69/p_symp_18_69 + prop_new_cases_w36_to_w39_20_69
Imm_w39_70    <- Imm_w20_70    - (N_andra_dos_w20_70*Vacc_eff_w20_70 + N_endast_en_dos_w20_70*Vacc_eff_w20_one_dose_70)/N_70    + prop_new_cases_70/p_symp_70       + prop_new_cases_w36_to_w39_70


# Proportion in each age group immune either from infection or vaccination at 30 september
# Used in calculations of infection rate
Immune_prop  <- c(Imm_w39_0_11, 
                  Imm_w39_12_15, 
                  Vacc_16_17 + (1 - Vacc_16_17) * Imm_w39_16_17, 
                  Vacc_18_69 + (1 - Vacc_18_69) * Imm_w39_18_69, 
                  Vacc_70    + (1 - Vacc_70) * Imm_w39_70)





# df_immune_susc <- data.frame(Ålder = c(group_names), Proportion_immuna = Immune_prop, Antal_immuna = round(Immune_prop * unname(N_all)), Antal_mottagliga = round((1-Immune_prop) * unname(N_all) ))
# XL_file_name <- paste(output.path,"/immune_susceptible","_VE_",Vacc_eff*100, ".xlsx", sep ="")
# 
#
#write.xlsx(df_immune_susc, XL_file_name )



names(Immune_prop) <- group_names
Immune  <- Immune_prop * N_all



#-----------------------------------------------------------------------------------
# 8. Contact Matrix and transmission model parameters
#-----------------------------------------------------------------------------------


# Contact matrix

M_original <- as.matrix(read.xlsx(paste(data.path,"/Contact_Matrix_row_to_column_0-11_12-15_16-17_18-69_70+.xlsx",sep="")) ) 

dimnames(M_original) <- list(group_names,
                             group_names)

M_original <- t(M_original) # from row to column into column to row

# mean number of contacts 10.13324
sum(apply(M_original, 2, sum)*N_all)/ sum(N_all)

# Incubation/exposed time (here defined as time from infection to becoming infectious)
eta     <- 1/5.1 
# Time being infectious
gammaD  <- 1/5


# Infectivities
original_infectivity <- 1.0
alfa_variant <- 1.5 
delta_variant0 <- 1.6*alfa_variant 

virus_variant <- delta_variant0


#-----------------------------------------------------------------------------------
# 9. Find infectivity at beginning of pandemic and at week 35
#-----------------------------------------------------------------------------------


# Use R0 to find infectivity at beginning of pandemic
R0_start <- 1.8


find_infection_rate_start <- function(x){
  R0 = function(x){
    e = eigen(x * M_original / gammaD)
    res <- e$values[1]
    return(res)
  }
  res <- R0_start - R0(x)
}

# at start of pandemic infection rate was (no interventions):
infection_rate_original <- uniroot(find_infection_rate_start, interval = c(0,1000))$root
infection_rate_original

# assumed infection rate with delta with no interventions:
infection_rate_start <- virus_variant * infection_rate_original



# in case you want to make it time dependent. 
# if not, doesnt matter.
infectivity <- function(time){
  return(infection_rate_start)
}



## Calculate the infection rate based on R_t and immunity level week 35

Observed_Rt <- 1.02

M_new <- matrix(rep(NA,5*5), ncol = 5)
M_new[,1] <- M_original[,1] * (1 - Immune_prop_w35)
M_new[,2] <- M_original[,2] * (1 - Immune_prop_w35)
M_new[,3] <- M_original[,3] * (1 - Immune_prop_w35)
M_new[,4] <- M_original[,4] * (1 - Immune_prop_w35)
M_new[,5] <- M_original[,5] * (1 - Immune_prop_w35)

Rt = function(infection){
  
  e = eigen(infection * M_new / gammaD)
  res <- e$values[1]
  return(res)
}



find_infection_rate <- function(x){
  res <- Observed_Rt - Rt(x)
  return(res)
}

# at week 35 infection rate was
infection_rate <- uniroot(find_infection_rate, interval = c(0,1000))$root
infection_rate
#----

infection_rate/infection_rate_start
# in 'infectivity' used in ODE, we use the higher delta inf given by 'infection_rate_start'. 
# Need to scale down to the observed infectivity week 35,
# the infectivity then becomes "infection_rate"
# NB! infection_rate/infection_rate_start can be seen as the contact reduction. 


M <- increse_from_aug*(infection_rate/infection_rate_start)*M_original




#-----------------------------------------------------------------------------------
# 10. Seeding of main model
#-----------------------------------------------------------------------------------


# We use scenario 0 for cases to 30 september, then the cases are on its way down
# That is, R_t < 1.
# We use that individuals are infectious for 5 days; that is, use the sum of the five last days 
# from scenario 0 as number of infectious. 
# For the exposed state, assume that these infectious individuals created R_t new cases. 
# Since the estimated number of daily cases in scenario 0 start to decrease 22 september we use a value close to 1.

incidence_sc0 <- incidence_est_0_19_rep + incidence_est_20_69_rep + incidence_est_70_rep
incidence_sc0 <- incidence_sc0[Sept04toSept30]
Dates_sc0 <- Dates_est[Sept04toSept30]

max_sc0 <- which(incidence_sc0==max(incidence_sc0))
Dates_sc0[max_sc0]



## Assumed effective reproduction number 30 sept. 
# When calculating the instantaneous R_t for simulated data of scenario 0
# results vary between 0.97 and 0.99, therefore chose 0.98
R_now <- 0.98 # 
# Assume 5 days infectioussness
Days_before <- 4

end_virsim <- length(incidence_est_0_11)


E_init <- c(E_0_11 = sum(incidence_est_0_11[(end_virsim-Days_before):end_virsim]) * R_now, 
            E_12_15 = sum(incidence_est_12_15[(end_virsim-Days_before):end_virsim]) * R_now, 
            E_16_17 = sum(incidence_est_16_17[(end_virsim-Days_before):end_virsim]) * R_now,
            E_18_69 = sum(incidence_est_18_69[(end_virsim-Days_before):end_virsim]) * R_now, 
            E_70 = sum(incidence_est_70[(end_virsim-Days_before):end_virsim]) * R_now)


I_symp_init   <- c(I_symp_0_11 = sum(incidence_est_0_11[(end_virsim-Days_before):end_virsim]) * p_symp_0_11, 
                   I_symp_12_15 = sum(incidence_est_12_15[(end_virsim-Days_before):end_virsim])* p_symp_12_15, 
                   I_symp_16_17 = sum(incidence_est_16_17[(end_virsim-Days_before):end_virsim]) * p_symp_16_17, 
                   I_symp_18_69 = sum(incidence_est_18_69[(end_virsim-Days_before):end_virsim]) * p_symp_18_69, 
                   I_symp_70 = sum(incidence_est_70[(end_virsim-Days_before):end_virsim])* p_symp_70)


I_asymp_init  <- c(I_asymp_0_11 = sum(incidence_est_0_11[(end_virsim-Days_before):end_virsim]) * (1 - p_symp_0_11), 
                   I_asymp_12_15 = sum(incidence_est_12_15[(end_virsim-Days_before):end_virsim]) * (1 - p_symp_12_15), 
                   I_asymp_16_17 = sum(incidence_est_16_17[(end_virsim-Days_before):end_virsim]) * (1 - p_symp_16_17), 
                   I_asymp_18_69 = sum(incidence_est_18_69[(end_virsim-Days_before):end_virsim]) * (1 - p_symp_18_69), 
                   I_asymp_70 = sum(incidence_est_70[(end_virsim-Days_before):end_virsim]) * (1 - p_symp_70))




S_init <- unname(N_all - E_init - I_symp_init - I_asymp_init - unname(Immune))
names(S_init) = c("S_0_11","S_12_15", "S_16_17", "S_18_69", "S_70")

R_init        <- c(R_0_11  = unname(Immune["0-11"]) , 
                   R_12_15  = unname(Immune["12-15"]) , 
                   R_16_17 = unname(Immune["16-17"]) ,  
                   R_18_69 = unname(Immune["18-69"]), 
                   R_70 =  unname(Immune["70+"]))


R_init/N_all
sum(R_init)/N

init <- c(S_init, 
          E_init,
          I_symp_init,
          I_asymp_init, 
          V_12_15 = 0) 




#-----------------------------------------------------------------------------------
# 11. Find rate of vaccination (of youths aged 12-15 years)
#-----------------------------------------------------------------------------------

# Here we find the rate of vaccination for youths of age 12-15 years that 
# corresponds to our sought coverage (75% as default) in 'time_to_vacc_youth' days.
# All youths have equal chance of becoming vaccinated. 
# Hence, we only need to use two compartments to find the rate of vaccination.
# 
# S_12_15 := unvaccinated 
# V_12_15 := vaccinated

# at the beginning no 12-15 vaccinated. 
init_vacc <- c(S_12_15 = N_12_15, V_12_15 = 0)


Vaccination <- TRUE


## Find vaccination rate x (1/mean time to vaccinate one 12-15) that corresponds to % coverage of 12-15 in time_to_vacc_youth days

# Function vacc_coverage
# input: inverse of vaccination rate (mean time to vaccination)
# output: the vaccination coverage reached when vaccination is performed for time_to_vacc_youth days

vacc_coverage <- function(rate_inv){
  
  vacc_rate <- function(time){
    res <- 0
    if(Vaccination){
      res <- (1/rate_inv) * (time < time_to_vacc_youth) # if time is less than time_to_vacc_youth => continue vaccinate, otherwise stop. 
    }
    
    return(res)
  }
  
  vacc.model <- function(time, state, parameters) {
    # S       <- state[1] # not vaccinated
    # V       <- state[2] # vaccinated
    par <- as.list(c(state, parameters))
    with(par, {
      
      
      dS_12_15  <- - vacc_rate(time)*S_12_15
      
      dV_12_15 <- vacc_rate(time)*S_12_15
      
      
      dx <- c(dS_12_15, dV_12_15)
      list(dx)
    }
    )
  }
  
  
  # states
  Opt_par <- c()# för infectivity
  t <- 1:365 # time in days
  fit_vacc <- data.frame(ode(y = init_vacc, times = t, func = vacc.model , parms = Opt_par))
  
  return(fit_vacc$V_12_15[length(t)]/N_12_15)
  
}


# Function root_function 
# input: inverse of rate of vaccination x (mean time to vaccination)
# output: difference between the vaccination coverage reached with rate x and the sought 'coverage'.
root_function <- function(x){
  return((vacc_coverage(x) - coverage))
  
}

# Now find rate that minimises the output of root_function
inv_vacc_rate_use <- uniroot(root_function, interval = c(1,100))$root

# Create vacc_rate function. 
# Depends on time, since if time_to_vacc_youth reached, no more individuals should be vaccinated
vacc_rate <- function(time){
  res <- 0
  if(Vaccination){
    res <- (1/inv_vacc_rate_use) * (time < (time_to_vacc_youth)) # if time is less than time_to_vacc_youth => continue vaccinate, otherwise stop.
  }
  
  return(res)
}


# check the vaccination and save figure on the vaccination progress obtained if wanted. 

vacc.model <- function(time, state, parameters) {
  # S       <- state[1] # not vaccinated
  # V       <- state[2] # vaccinated
  par <- as.list(c(state, parameters))
  with(par, {
    
    dS_12_15  <- - vacc_rate(time)*S_12_15
    
    dV_12_15 <- vacc_rate(time)*S_12_15
    
    
    dx <- c(dS_12_15, dV_12_15)
    list(dx)
  }
  )
}


# states
Opt_par <- c() # for infectivity, but we assumed it constant.
t <- 1:(time_to_vacc_youth+30) # time in days
fit_vacc <- data.frame(ode(y = init_vacc, times = t, func = vacc.model , parms = Opt_par))

t_vacc_days <- as.Date(t,origin="2021-09-30")


### if you want to Inspect rate of vaccination, uncomment following:

# pdf_file_name <- paste(output.path,"/Täckning_tid_",time_to_vacc_youth,".pdf", sep ="")
# 
# 
# pdf(pdf_file_name,width=7, height=6 )
# 
plot(t_vacc_days, fit_vacc$V_12_15/N_12_15*100, xlab = "", type = "l", lwd = 2,
     ylab = "Vaccinationstäckning (%)", main ="Vaccinationstäckning 12-15-åringar",
     ylim = c(0,80), xaxt = 'n')

axis(side = 1, at = as.Date(c("2021-10-01", "2021-11-01", "2021-12-01", "2022-01-01", "2022-02-01","2022-03-01")),
     labels = c("2021-okt", "2021-nov", "2021-dec", "2022-jan", "2022-feb", "2022-mar"), tcl = -0.2)

# dev.off()



#-----------------------------------------------------------------------------------
# 12. Main model and simulation
#-----------------------------------------------------------------------------------


# Assuming it takes 'delayed_response' (3 weeks) to vaccine response
# just shift the obtained vaccination curve to obtain effective vaccination.
vacc_rate <- function(time){
  res <- 0
  if(Vaccination){
    res <- 1/inv_vacc_rate_use * (time < (time_to_vacc_youth + delayed_response))
    res <- res * (time >= (delayed_response)) 
  }
  
  return(res)
}




#
#--- Scenario with no vaccination
#



Vaccination <- FALSE
#Vaccination <- TRUE


seir.model <- function(time, state, parameters) {
  # S       <- state[1] # susceptibles
  # E       <- state[2] # latent/exposed but not infectious
  # I_symp  <- state[3] # infected who get reported
  # I_asymp <- state[4] # infected who remain non-reported
  # V       <- state[5] # vaccinated 12-15-year olds
  # R       <- state[6] # recovered/immune. Not really a state since we implicitly know how many they are = N - rest
  par <- as.list(c(state, parameters))
  with(par, {
    
    I_0_11  <- I_symp_0_11  + I_asymp_0_11
    I_12_15 <- I_symp_12_15 + I_asymp_12_15
    I_16_17 <- I_symp_16_17 + I_asymp_16_17
    I_18_69 <- I_symp_18_69 + I_asymp_18_69
    I_70    <- I_symp_70 + I_asymp_70
    I_prop  <- c(I_0_11/N_0_11, I_12_15/N_12_15, I_16_17/N_16_17, I_18_69/N_18_69, I_70/N_70)
    
    
    dS_0_11   <- -infectivity(time) * S_0_11  * sum(M[,"0-11"] * I_prop)
    dS_12_15  <- -infectivity(time) * S_12_15 * sum(M[,"12-15"] * I_prop) - vacc_rate(time) * Vacc_eff_12_15 * S_12_15
    dS_16_17  <- -infectivity(time) * S_16_17 * sum(M[,"16-17"] * I_prop) 
    dS_18_69  <- -infectivity(time) * S_18_69 * sum(M[,"18-69"] * I_prop)
    dS_70     <- -infectivity(time) * S_70    * sum(M[,"70+"] * I_prop)
    
    dV_12_15 <- vacc_rate(time) * Vacc_eff_12_15 * S_12_15
    
    dE_0_11   <-  infectivity(time) * S_0_11  * sum(M[,"0-11"] * I_prop)  - eta*E_0_11
    dE_12_15  <-  infectivity(time) * S_12_15 * sum(M[,"12-15"] * I_prop) - eta*E_12_15
    dE_16_17  <-  infectivity(time) * S_16_17 * sum(M[,"16-17"] * I_prop) - eta*E_16_17
    dE_18_69  <-  infectivity(time) * S_18_69 * sum(M[,"18-69"] * I_prop) - eta*E_18_69
    dE_70     <-  infectivity(time) * S_70    * sum(M[,"70+"] * I_prop)   - eta*E_70

    
    dI_symp_0_11  <- p_symp_0_11  * eta * E_0_11  - gammaD * I_symp_0_11
    dI_symp_12_15 <- p_symp_12_15 * eta * E_12_15 - gammaD * I_symp_12_15
    dI_symp_16_17 <- p_symp_16_17 * eta * E_16_17 - gammaD * I_symp_16_17
    dI_symp_18_69 <- p_symp_18_69 * eta * E_18_69 - gammaD * I_symp_18_69
    dI_symp_70    <- p_symp_70    * eta * E_70    - gammaD * I_symp_70
    
    dI_asymp_0_11   <- (1 - p_symp_0_11)  * eta * E_0_11   - gammaD * I_asymp_0_11
    dI_asymp_12_15  <- (1 - p_symp_12_15) * eta * E_12_15  - gammaD * I_asymp_12_15
    dI_asymp_16_17  <- (1 - p_symp_16_17) * eta * E_16_17  - gammaD * I_asymp_16_17
    dI_asymp_18_69  <- (1 - p_symp_18_69) * eta * E_18_69  - gammaD * I_asymp_18_69
    dI_asymp_70     <- (1 - p_symp_70)    * eta * E_70     - gammaD * I_asymp_70
    
    dR_0_11   <- gammaD * I_0_11
    dR_12_15  <- gammaD * I_12_15
    dR_16_17  <- gammaD * I_16_17
    dR_18_69  <- gammaD * I_18_69
    dR_70     <- gammaD * I_70 
    
    dx <- c(dS_0_11, dS_12_15, dS_16_17, dS_18_69, dS_70,
            dE_0_11, dE_12_15, dE_16_17, dE_18_69, dE_70,
            dI_symp_0_11, dI_symp_12_15, dI_symp_16_17, dI_symp_18_69, dI_symp_70,
            dI_asymp_0_11, dI_asymp_12_15, dI_asymp_16_17, dI_asymp_18_69, dI_asymp_70,
            dV_12_15)
    
    list(dx)
  }
  )
}



Opt_par <- c() # for infectivity

t <- 1:number_of_days # time in days of simulation
fit_novacc <- data.frame(ode(y = init, times = t, func = seir.model , parms = Opt_par))

# Total number of daily new cases
Incidence_all <- eta*(fit_novacc$E_0_11 + fit_novacc$E_12_15 + fit_novacc$E_16_17 + fit_novacc$E_18_69 + fit_novacc$E_70) 


Incidence_all_no_vacc_0_11  <- eta * (fit_novacc$E_0_11)
Incidence_all_no_vacc_12_15 <- eta * (fit_novacc$E_12_15)
Incidence_all_no_vacc_16_17 <- eta * (fit_novacc$E_16_17)
Incidence_all_no_vacc_18_69 <- eta * (fit_novacc$E_18_69)
Incidence_all_no_vacc_70    <- eta * (fit_novacc$E_70)



Incidence_rep_no_vacc       <- eta * (fit_novacc$E_0_11*p_symp_0_11 + fit_novacc$E_12_15*p_symp_12_15 + fit_novacc$E_16_17*p_symp_16_17 + fit_novacc$E_18_69*p_symp_18_69 + fit_novacc$E_70*p_symp_70)
Incidence_rep_no_vacc_0_11  <- eta * fit_novacc$E_0_11 * p_symp_0_11
Incidence_rep_no_vacc_12_15 <- eta * fit_novacc$E_12_15 * p_symp_12_15
Incidence_rep_no_vacc_16_17 <- eta * fit_novacc$E_16_17 * p_symp_16_17
Incidence_rep_no_vacc_18_69 <- eta * fit_novacc$E_18_69 * p_symp_18_69
Incidence_rep_no_vacc_70    <- eta * fit_novacc$E_70 * p_symp_70




#
#--- Scenario with vaccination
#


#Vaccination <- FALSE
Vaccination <- TRUE




seir.model <- function(time, state, parameters) {
  # S       <- state[1] # susceptibles
  # E       <- state[2] # latent/exposed but not infectious
  # I_symp  <- state[3] # infected who get reported
  # I_asymp <- state[4] # infected who remain non-reported
  # V       <- state[5] # vaccinated 12-15-year olds
  # R       <- state[6] # recovered/immune, not really a state since we implicitly know how many they are N-rest
  par <- as.list(c(state, parameters))
  with(par, {
    
    I_0_11  <- I_symp_0_11  + I_asymp_0_11
    I_12_15 <- I_symp_12_15 + I_asymp_12_15
    I_16_17 <- I_symp_16_17 + I_asymp_16_17
    I_18_69 <- I_symp_18_69 + I_asymp_18_69
    I_70    <- I_symp_70 + I_asymp_70
    I_prop  <- c(I_0_11/N_0_11, I_12_15/N_12_15, I_16_17/N_16_17, I_18_69/N_18_69, I_70/N_70)
    
    
    dS_0_11   <- -infectivity(time) * S_0_11  * sum(M[,"0-11"] * I_prop)
    dS_12_15  <- -infectivity(time) * S_12_15 * sum(M[,"12-15"] * I_prop) - vacc_rate(time) * Vacc_eff_12_15 * S_12_15
    dS_16_17  <- -infectivity(time) * S_16_17 * sum(M[,"16-17"] * I_prop) 
    dS_18_69  <- -infectivity(time) * S_18_69 * sum(M[,"18-69"] * I_prop)
    dS_70     <- -infectivity(time) * S_70    * sum(M[,"70+"] * I_prop)
    
    dV_12_15 <- vacc_rate(time) * Vacc_eff_12_15 * S_12_15
    
    dE_0_11   <-  infectivity(time) * S_0_11  * sum(M[,"0-11"] * I_prop)  - eta*E_0_11
    dE_12_15  <-  infectivity(time) * S_12_15 * sum(M[,"12-15"] * I_prop) - eta*E_12_15
    dE_16_17  <-  infectivity(time) * S_16_17 * sum(M[,"16-17"] * I_prop) - eta*E_16_17
    dE_18_69  <-  infectivity(time) * S_18_69 * sum(M[,"18-69"] * I_prop) - eta*E_18_69
    dE_70     <-  infectivity(time) * S_70    * sum(M[,"70+"] * I_prop)   - eta*E_70
    

    
    dI_symp_0_11  <- p_symp_0_11  * eta * E_0_11  - gammaD * I_symp_0_11
    dI_symp_12_15 <- p_symp_12_15 * eta * E_12_15 - gammaD * I_symp_12_15
    dI_symp_16_17 <- p_symp_16_17 * eta * E_16_17 - gammaD * I_symp_16_17
    dI_symp_18_69 <- p_symp_18_69 * eta * E_18_69 - gammaD * I_symp_18_69
    dI_symp_70    <- p_symp_70    * eta * E_70    - gammaD * I_symp_70
    
    dI_asymp_0_11   <- (1 - p_symp_0_11)  * eta * E_0_11   - gammaD * I_asymp_0_11
    dI_asymp_12_15  <- (1 - p_symp_12_15) * eta * E_12_15  - gammaD * I_asymp_12_15
    dI_asymp_16_17  <- (1 - p_symp_16_17) * eta * E_16_17  - gammaD * I_asymp_16_17
    dI_asymp_18_69  <- (1 - p_symp_18_69) * eta * E_18_69  - gammaD * I_asymp_18_69
    dI_asymp_70     <- (1 - p_symp_70)    * eta * E_70     - gammaD * I_asymp_70
    
    dR_0_11   <- gammaD * I_0_11
    dR_12_15  <- gammaD * I_12_15
    dR_16_17  <- gammaD * I_16_17
    dR_18_69  <- gammaD * I_18_69
    dR_70     <- gammaD * I_70 
    
    dx <- c(dS_0_11, dS_12_15, dS_16_17, dS_18_69, dS_70,
            dE_0_11, dE_12_15, dE_16_17, dE_18_69, dE_70,
            dI_symp_0_11, dI_symp_12_15, dI_symp_16_17, dI_symp_18_69, dI_symp_70,
            dI_asymp_0_11, dI_asymp_12_15, dI_asymp_16_17, dI_asymp_18_69, dI_asymp_70,
            dV_12_15)
    
    list(dx)
  }
  )
}



Opt_par <- c() # for infectivity


fit_vacc <- data.frame(ode(y = init, times = t, func = seir.model , parms = Opt_par))



Incidence_all <- eta*(fit_vacc$E_0_11 + fit_vacc$E_12_15 + fit_vacc$E_16_17 + fit_vacc$E_18_69 + fit_vacc$E_70)

Incidence_all_vacc_0_11  <- eta * fit_vacc$E_0_11
Incidence_all_vacc_12_15 <- eta * fit_vacc$E_12_15
Incidence_all_vacc_16_17 <- eta * fit_vacc$E_16_17
Incidence_all_vacc_18_69 <- eta * fit_vacc$E_18_69
Incidence_all_vacc_70    <- eta * fit_vacc$E_70




Incidence_rep_vacc <- eta * (fit_vacc$E_0_11*p_symp_0_11 + fit_vacc$E_12_15*p_symp_12_15 + fit_vacc$E_16_17*p_symp_16_17 + fit_vacc$E_18_69*p_symp_18_69 + fit_vacc$E_70*p_symp_70)
Incidence_rep_vacc_0_11  <- eta * fit_vacc$E_0_11 * p_symp_0_11
Incidence_rep_vacc_12_15 <- eta * fit_vacc$E_12_15 * p_symp_12_15
Incidence_rep_vacc_16_17 <- eta * fit_vacc$E_16_17 * p_symp_16_17
Incidence_rep_vacc_18_69 <- eta * fit_vacc$E_18_69 * p_symp_18_69
Incidence_rep_vacc_70    <- eta * fit_vacc$E_70 * p_symp_70




#-----------------------------------------------------------------------------------
# 13. Create and save result tables
#-----------------------------------------------------------------------------------



#
#--- Table of total number of cases
#
 

res_0_11 <- c(round(sum(Incidence_all_no_vacc_0_11)), 
              round(sum(Incidence_all_vacc_0_11)) , 
              round(sum(Incidence_all_no_vacc_0_11) - sum(Incidence_all_vacc_0_11)),
              round(100*(sum(Incidence_all_no_vacc_0_11)-sum(Incidence_all_vacc_0_11))/ sum(Incidence_all_no_vacc_0_11), digits=2))

res_12_15 <-c(round(sum(Incidence_all_no_vacc_12_15)), 
              round(sum(Incidence_all_vacc_12_15)), 
              round(sum(Incidence_all_no_vacc_12_15) - sum(Incidence_all_vacc_12_15)) ,
              round(100*(sum(Incidence_all_no_vacc_12_15) - sum(Incidence_all_vacc_12_15) )/sum(Incidence_all_no_vacc_12_15), digits=2))

res_16_17 <-c(round(sum(Incidence_all_no_vacc_16_17)), 
              round(sum(Incidence_all_vacc_16_17)), 
              round(sum(Incidence_all_no_vacc_16_17) - sum(Incidence_all_vacc_16_17)) ,
              round(100*(sum(Incidence_all_no_vacc_16_17) - sum(Incidence_all_vacc_16_17) )/sum(Incidence_all_no_vacc_16_17), digits=2))


res_18_69 <- c(round(sum(Incidence_all_no_vacc_18_69)), 
               round(sum(Incidence_all_vacc_18_69)), 
               round(sum(Incidence_all_no_vacc_18_69) - sum(Incidence_all_vacc_18_69)) ,
               round(100*(sum(Incidence_all_no_vacc_18_69) - sum(Incidence_all_vacc_18_69) )/sum(Incidence_all_no_vacc_18_69), digits=2))

res_70 <- c(round(sum(Incidence_all_no_vacc_70)), 
            round(sum(Incidence_all_vacc_70)), 
            round(sum(Incidence_all_no_vacc_70) - sum(Incidence_all_vacc_70)) ,
            round(100*(sum(Incidence_all_no_vacc_70) -  sum(Incidence_all_vacc_70))/ sum(Incidence_all_no_vacc_70), digits=2)) 

M_res <- matrix(c(res_0_11, res_12_15, res_16_17, res_18_69, res_70),
                nrow = 5, byrow = TRUE)

df_res <- as.data.frame(M_res)

colnames(df_res) <- c("Utan vaccin", "Med vaccin", "Minskning",  "Minskning procent")
df_age <- data.frame(Ålder = c(group_names))

df_res <- cbind(df_age,df_res)
df_res






#
#--- Table of reported number of cases
#


res_0_11_rep <- c(round(sum(Incidence_rep_no_vacc_0_11)), 
                  round(sum(Incidence_rep_vacc_0_11)), 
                  round(sum(Incidence_rep_no_vacc_0_11) - sum(Incidence_rep_vacc_0_11)), 
                  round(100*(sum(Incidence_rep_no_vacc_0_11)-sum(Incidence_rep_vacc_0_11))/ sum(Incidence_rep_no_vacc_0_11), digits=2))

res_12_15_rep <-c(round(sum(Incidence_rep_no_vacc_12_15)), 
                  round(sum(Incidence_rep_vacc_12_15)), 
                  round(sum(Incidence_rep_no_vacc_12_15) - sum(Incidence_rep_vacc_12_15)), 
                  round(100*(sum(Incidence_rep_no_vacc_12_15) - sum(Incidence_rep_vacc_12_15) )/sum(Incidence_rep_no_vacc_12_15), digits=2))

res_16_17_rep <-c(round(sum(Incidence_rep_no_vacc_16_17)), 
                  round(sum(Incidence_rep_vacc_16_17)), 
                  round(sum(Incidence_rep_no_vacc_16_17) - sum(Incidence_rep_vacc_16_17)), 
                  round(100*(sum(Incidence_rep_no_vacc_16_17) - sum(Incidence_rep_vacc_16_17) )/sum(Incidence_rep_no_vacc_16_17), digits=2))


res_18_69_rep <- c(round(sum(Incidence_rep_no_vacc_18_69)), 
                   round(sum(Incidence_rep_vacc_18_69)), 
                   round(sum(Incidence_rep_no_vacc_18_69) - sum(Incidence_rep_vacc_18_69)),
                   round(100*(sum(Incidence_rep_no_vacc_18_69) - sum(Incidence_rep_vacc_18_69) )/sum(Incidence_rep_no_vacc_18_69), digits=2))

res_70_rep <- c(round(sum(Incidence_rep_no_vacc_70)), 
                round(sum(Incidence_rep_vacc_70)), 
                round(sum(Incidence_rep_no_vacc_70) - sum(Incidence_rep_vacc_70)),
                round(100*(sum(Incidence_rep_no_vacc_70) -  sum(Incidence_rep_vacc_70))/ sum(Incidence_rep_no_vacc_70), digits=2)) 

M_res_rep <- matrix(c(res_0_11_rep, res_12_15_rep, res_16_17_rep, res_18_69_rep, res_70_rep),
                nrow = 5, byrow = TRUE)

df_res_rep <- as.data.frame(M_res_rep)

colnames(df_res_rep) <- c("Utan vaccin", "Med vaccin", "Minskning",  "Minskning procent")
df_age <- data.frame(Ålder = c(group_names))

df_res_rep <- cbind(df_age,df_res_rep)
df_res_rep

XL_file_name      <- paste(output.path,"/Table_tid_tills_täckning_", time_to_vacc_youth,"_VE_",Vacc_eff*100,"_ökn_kont_",increse_from_aug, ".xlsx", sep ="")
XL_file_name_rep  <- paste(output.path,"/Reported_Table_tid_tills_täckning_", time_to_vacc_youth,"_VE_",Vacc_eff*100,"_ökn_kont_",increse_from_aug, ".xlsx", sep ="")


#write.xlsx(df_res, XL_file_name )

write.xlsx(df_res_rep, XL_file_name_rep )



#-----------------------------------------------------------------------------------
# 14. Save figures
#-----------------------------------------------------------------------------------


col_0_11  <- "springgreen3"
col_12_15 <- "black"
col_16_17 <- "steelblue1"
col_18_69 <- "slateblue4"
col_70    <- "tomato1"





#
#--- Figures of total number of daily cases for each age group
#


pdf_file_name <- paste(output.path,"/Incidence_tid_tills_täckning_", time_to_vacc_youth,"_VE_",Vacc_eff*100,"_ökn_kont_",increse_from_aug,".pdf", sep ="")
png_file_name <- paste(output.path,"/Incidence_tid_tills_täckning_", time_to_vacc_youth,"_VE_",Vacc_eff*100,"_ökn_kont_",increse_from_aug,".png", sep ="")



#pdf(pdf_file_name,width=12, height=8 )

par(mfrow=c(2,3))
par(mar = c(5, 4, 4, 2.1))


# 0-11

Title <- paste("Dagligt antal nya fall 0-11", sep="")
t_date <- as.Date(t, origin="2021-09-30")
plot(t_date, Incidence_all_no_vacc_0_11, type = "l", main = Title, ylab ="Incidens",  xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_0_11*1.3)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_0_11,lwd = 2,  col = col_0_11)
lines(t_date, Incidence_all_vacc_0_11,lwd = 2,  col = col_0_11, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_0_11), lty =c(1,2), lwd = 2)



# 12-15
Title <- paste("Dagligt antal nya fall 12-15", sep="")

##
plot(t_date, Incidence_all_no_vacc_12_15, type = "l", main = Title, ylab ="Incidens", xlab = "", 
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_12_15*1.3)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_12_15,lwd = 2,  col = col_12_15)
lines(t_date, Incidence_all_vacc_12_15,lwd = 2,  col = col_12_15, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_12_15), lty =c(1,2), lwd = 2)




# 16-17

Title <- paste("Dagligt antal nya fall 16-17", sep="")

##
plot(t_date, Incidence_all_no_vacc_16_17, type = "l", main = Title, ylab ="Incidens",  xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_16_17*1.3)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_16_17,lwd = 2,  col = col_16_17)
lines(t_date, Incidence_all_vacc_16_17,lwd = 2,  col = col_16_17, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_16_17), lty =c(1,2), lwd = 2)



# 18-69

Title <- paste("Dagligt antal nya fall 18-69", sep="")

##
plot(t_date, Incidence_all_no_vacc_18_69, type = "l", main = Title, ylab ="Incidens", xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_18_69*1.3)))
)


grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_18_69,lwd = 2,  col = col_18_69)
lines(t_date, Incidence_all_vacc_18_69,lwd = 2,  col = col_18_69, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_18_69), lty =c(1,2), lwd = 2)



# 70

Title <- paste("Dagligt antal nya fall 70+", sep="")

##
plot(t_date, Incidence_all_no_vacc_70, type = "l", main = Title, ylab ="Incidens",  xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_70*1.3)))
)


grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_70,lwd = 2,  col = col_70)
lines(t_date, Incidence_all_vacc_70,lwd = 2,  col = col_70, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_70), lty =c(1,2), lwd = 2)

#dev.off()








#png(png_file_name, width=12*150, height=8*150, res=200 )


par(mfrow=c(2,3))
par(mar = c(5, 4, 4, 2.1))

Title <- paste("Dagligt antal nya fall 0-11", sep="")

plot(t_date, Incidence_all_no_vacc_0_11, type = "l", main = Title, ylab ="Incidens", xlab = "", 
          ylim=c(0, ceiling(max(Incidence_all_no_vacc_0_11*1.3)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_0_11,lwd = 2,  col = col_0_11)
lines(t_date, Incidence_all_vacc_0_11,lwd = 2,  col = col_0_11, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_0_11), lty =c(1,2), lwd = 2)

# 12-15
Title <- paste("Dagligt antal nya fall 12-15", sep="")

##
plot(t_date, Incidence_all_no_vacc_12_15, type = "l", main = Title, ylab ="Incidens", xlab = "", 
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_12_15*1.3)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_12_15,lwd = 2,  col = col_12_15)
lines(t_date, Incidence_all_vacc_12_15,lwd = 2,  col = col_12_15, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_12_15), lty =c(1,2), lwd = 2)




# 16-17
Title <- paste("Dagligt antal nya fall 16-17", sep="")

##
plot(t_date, Incidence_all_no_vacc_16_17, type = "l", main = Title, ylab ="Incidens", xlab = "", 
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_16_17*1.3)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_16_17,lwd = 2,  col = col_16_17)
lines(t_date, Incidence_all_vacc_16_17,lwd = 2,  col = col_16_17, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_16_17), lty =c(1,2), lwd = 2)


# 18-69

Title <- paste("Dagligt antal nya fall 18-69", sep="")

##
plot(t, Incidence_all_no_vacc_18_69, type = "l", main = Title, ylab ="Incidens", xlab = "", 
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_18_69*1.3)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_18_69,lwd = 2,  col = col_18_69)
lines(t_date, Incidence_all_vacc_18_69,lwd = 2,  col = col_18_69, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_18_69), lty =c(1,2), lwd = 2)



# 70

Title <- paste("Dagligt antal nya fall 70+", sep="")

##
plot(t_date, Incidence_all_no_vacc_70, type = "l", main = Title, ylab ="Incidens", xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_70*1.3)))
)


grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_70,lwd = 2,  col = col_70)
lines(t_date, Incidence_all_vacc_70,lwd = 2,  col = col_70, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_70), lty =c(1,2), lwd = 2)


#dev.off()




#
#--- Figures of reported number of daily cases for each age group
#

pdf_file_name <- paste(output.path,"/Reported_tid_tills_täckning_", time_to_vacc_youth,"_VE_",Vacc_eff*100,"_ökn_kont_",increse_from_aug,".pdf", sep ="")
png_file_name <- paste(output.path,"/Reported_tid_tills_täckning_", time_to_vacc_youth,"_VE_",Vacc_eff*100,"_ökn_kont_",increse_from_aug,".png", sep ="")



#pdf(pdf_file_name,width=12, height=8 )

par(mfrow=c(2,3))
par(mar = c(5, 4, 4, 2.1))


# 0-11
Title <- paste("Dagligt antal nya rapporterade fall 0-11", sep="")
t_date <- as.Date(t, origin="2021-09-30")
plot(t_date, Incidence_all_no_vacc_0_11*p_symp_0_11, type = "l", main = Title, ylab ="Incidens",  xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_0_11*1.3*p_symp_0_11)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_0_11*p_symp_0_11,lwd = 2,  col = col_0_11)
lines(t_date, Incidence_all_vacc_0_11*p_symp_0_11,lwd = 2,  col = col_0_11, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_0_11), lty =c(1,2), lwd = 2)


# 12-15
Title <- paste("Dagligt antal nya rapporterade fall 12-15", sep="")

##
plot(t_date, Incidence_all_no_vacc_12_15*p_symp_12_15, type = "l", main = Title, ylab ="Incidens", xlab = "", 
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_12_15*1.3*p_symp_12_15)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_12_15*p_symp_12_15,lwd = 2,  col = col_12_15)
lines(t_date, Incidence_all_vacc_12_15*p_symp_12_15,lwd = 2,  col = col_12_15, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_12_15), lty =c(1,2), lwd = 2)



# 16-17

Title <- paste("Dagligt antal nya rapporterade fall 16-17", sep="")

##
plot(t_date, Incidence_all_no_vacc_16_17*p_symp_16_17, type = "l", main = Title, ylab ="Incidens",  xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_16_17*1.3*p_symp_16_17)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_16_17*p_symp_16_17,lwd = 2,  col = col_16_17)
lines(t_date, Incidence_all_vacc_16_17*p_symp_16_17,lwd = 2,  col = col_16_17, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_16_17), lty =c(1,2), lwd = 2)


# 18-69

Title <- paste("Dagligt antal nya rapporterade fall 18-69", sep="")

##
plot(t_date, Incidence_all_no_vacc_18_69*p_symp_18_69, type = "l", main = Title, ylab ="Incidens", xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_18_69*1.3*p_symp_18_69)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_18_69*p_symp_18_69,lwd = 2,  col = col_18_69)
lines(t_date, Incidence_all_vacc_18_69*p_symp_18_69,lwd = 2,  col = col_18_69, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_18_69), lty =c(1,2), lwd = 2)



# 70

Title <- paste("Dagligt antal nya rapporterade fall 70+", sep="")

##
plot(t_date, Incidence_all_no_vacc_70*p_symp_70, type = "l", main = Title, ylab ="Incidens",  xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_70*1.3*p_symp_70)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_70*p_symp_70,lwd = 2,  col = col_70)
lines(t_date, Incidence_all_vacc_70*p_symp_70,lwd = 2,  col = col_70, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_70), lty =c(1,2), lwd = 2)


#dev.off()







png(png_file_name, width=12*150, height=8*150, res=200 )


par(mfrow=c(2,3))
par(mar = c(5, 4, 4, 2.1))


# 0-11

Title <- paste("Dagligt antal nya rapporterade fall 0-11", sep="")
t_date <- as.Date(t, origin="2021-09-30")
plot(t_date, Incidence_all_no_vacc_0_11*p_symp_0_11, type = "l", main = Title, ylab ="Incidens",  xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_0_11*1.3*p_symp_0_11)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_0_11*p_symp_0_11,lwd = 2,  col = col_0_11)
lines(t_date, Incidence_all_vacc_0_11*p_symp_0_11,lwd = 2,  col = col_0_11, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_0_11), lty =c(1,2), lwd = 2)




# 12-15
Title <- paste("Dagligt antal nya rapporterade fall 12-15", sep="")

##
plot(t_date, Incidence_all_no_vacc_12_15*p_symp_12_15, type = "l", main = Title, ylab ="Incidens", xlab = "", 
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_12_15*1.3*p_symp_12_15)))
)


grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_12_15*p_symp_12_15,lwd = 2,  col = col_12_15)
lines(t_date, Incidence_all_vacc_12_15*p_symp_12_15,lwd = 2,  col = col_12_15, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_12_15), lty =c(1,2), lwd = 2)



# 16-17

Title <- paste("Dagligt antal nya rapporterade fall 16-17", sep="")

##
plot(t_date, Incidence_all_no_vacc_16_17*p_symp_16_17, type = "l", main = Title, ylab ="Incidens",  xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_16_17*1.3*p_symp_16_17)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_16_17*p_symp_16_17,lwd = 2,  col = col_16_17)
lines(t_date, Incidence_all_vacc_16_17*p_symp_16_17,lwd = 2,  col = col_16_17, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_16_17), lty =c(1,2), lwd = 2)




# 18-69

Title <- paste("Dagligt antal nya rapporterade fall 18-69", sep="")

##
plot(t_date, Incidence_all_no_vacc_18_69*p_symp_18_69, type = "l", main = Title, ylab ="Incidens", xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_18_69*1.3*p_symp_18_69)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_18_69*p_symp_18_69,lwd = 2,  col = col_18_69)
lines(t_date, Incidence_all_vacc_18_69*p_symp_18_69,lwd = 2,  col = col_18_69, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_18_69), lty =c(1,2), lwd = 2)



# 70

Title <- paste("Dagligt antal nya rapporterade fall 70+", sep="")

##
plot(t_date, Incidence_all_no_vacc_70*p_symp_70, type = "l", main = Title, ylab ="Incidens",  xlab = "",
     ylim=c(0, ceiling(max(Incidence_all_no_vacc_70*1.3*p_symp_70)))
)

grid(nx=NA, ny=NULL)
lines(t_date, Incidence_all_no_vacc_70*p_symp_70,lwd = 2,  col = col_70)
lines(t_date, Incidence_all_vacc_70*p_symp_70,lwd = 2,  col = col_70, lty = 2)
legend("topright", legend = c('Ej vaccination', 'Vaccination'), col = c(col_70), lty =c(1,2), lwd = 2)


dev.off()




