####################################
# Florida FRS Normal Cost/Benefit Model #
####################################

rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(scales)
library(zoo)
#setwd(getwd())

FileName <- 'FRS_BM_Inputs.xlsx'


YearStart <- 2021
Age <- 20:120
YOS <- 0:100
RetirementAge <- 20:120
Years <- 2011:2121    #(why 2121? Because 120 - 20 + 2021 = 2121)
#Updated from 2010 to 2011

#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}

#Import key data tables
SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates') #Teachers' mortality assumptions
#View(SurvivalRates)
#View(MaleMP)
MaleMP <- read_excel(FileName, sheet = 'MP-2018_Male') 
FemaleMP <- read_excel(FileName, sheet = 'MP-2018_Female')
SalaryGrowth <- read_excel(FileName, sheet = "Salary Growth")

################
SalaryEntry <- read_excel(FileName, sheet = "Salary and Headcount") %>% 
select(entry_age, start_sal, count_start)

##############
TerminationRateMale <- read_excel(FileName, sheet = 'Termination Rates Male')
TerminationRateFemale <- read_excel(FileName, sheet = 'Termination Rates Female')
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated*
#View(RetirementRates)


################
# Main rule: Retirement Eligibility
################

IsRetirementEligible <- function(Age, YOS){
  Check = ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI) |
                   (YOS >= NormalYOSII) |
                   (YOS >= ReduceRetYOS), TRUE, FALSE)
  return(Check)
}

################
# Sub rule: 2 Retirement Types
################

RetirementType <- function(Age, YOS){
  
  Check = ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI) |
                   (YOS >= NormalYOSII), "Normal",
                 ifelse(YOS >= ReduceRetYOS, "Reduced","No"))
  
  return(Check)
}


#These rates dont change so they're outside the function
#Transform base mortality rates and mortality improvement rates
MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)


##Mortality calculations
#Expand grid for ages 20-120 and years 2010 to 2121 (why 2121? Because 120 - 20 + 2021 = 2121)
MortalityTable <- expand_grid(Age, Years) %>% 
#Join base mortality table with mortality improvement table and calculate the final mortality rates
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP, by = c("Age", "Years")) %>% 
  left_join(FemaleMP, by = c("Age", "Years")) %>% 
  left_join(MaleMP_ultimate, by = "Age") %>% 
  left_join(FemaleMP_ultimate, by = "Age") %>% 
  mutate(MaleMP_final = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male),
         FemaleMP_final = ifelse(Years > max(FemaleMP$Years),  MP_ultimate_female, MP_female),
         entry_age = Age - (Years - YearStart),
         YOS = Age - entry_age) %>% 
  group_by(Age) %>%
  
  #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
  mutate(MPcumprod_male = cumprod(1 - MaleMP_final),
         #Started mort. table from 2011 (instead of 2010) 
         #to cumprod over 2011+ & then multiply by PubT_2010 mortality rates
         #when retiree mort rates are unavailable (because the retirement age is too young), active mort rates are chosen instead
         MPcumprod_female = cumprod(1 - FemaleMP_final),
         mort_male = ifelse(IsRetirementEligible(Age, YOS)==F | is.na(PubT_2010_healthy_retiree_male), PubT_2010_employee_male,
                            PubT_2010_healthy_retiree_male) * MPcumprod_male,
         mort_female = ifelse(IsRetirementEligible(Age, YOS)==F | is.na(PubT_2010_healthy_retiree_female), PubT_2010_employee_female,
                              PubT_2010_healthy_retiree_female) * MPcumprod_female,
         mort = (mort_male + mort_female)/2) %>% 
         #Recalcualting average
  filter(Years >= 2021, entry_age >= 20) %>% 
  ungroup()

#############
#############

#filter out the necessary variables
MortalityTable <- MortalityTable %>% select(Age, Years, entry_age, mort) %>% 
  arrange(entry_age) 

#View(MortalityTable)
######################
######################

##Separation Rates calculations
age_group_labels <- colnames(TerminationRateFemale)[-1]     
age_breaks <- c(min(Age), 24, 29, 34, 44, 54, max(Age))     #prepare age group labels and breaks for later data merge 
# cut(Age, breaks = age_breaks, labels = age_break_labels, include.lowest = T)

#Convert termination tables into long format
TerminationRateMale_long <- TerminationRateMale %>% 
  pivot_longer(-YOS, names_to = "age_group", values_to = "TermMale")
TerminationRateFemale_long <- TerminationRateFemale %>% 
  pivot_longer(-YOS, names_to = "age_group", values_to = "TermFemale")

#Calculate total retirement rates by adding "Drop_entry" and "Drop_eligible" together
RetirementRates <- RetirementRates %>% 
  mutate(RetMale = DROP_eligible_male + DROP_entry_male,
         RetFemale = DROP_eligible_female + DROP_entry_female) %>% 
  select(Age, RetMale, RetFemale)

#Merge term rates and retirement rates together
SeparationRates <- expand_grid(Age, YOS) %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(entry_age, Age) %>% 
  mutate(age_group = cut(Age, breaks = age_breaks, labels = age_group_labels, include.lowest = T)) %>%   #cut Age into age groups
  left_join(TerminationRateMale_long, by = c("YOS", "age_group")) %>% 
  left_join(TerminationRateFemale_long, by = c("YOS", "age_group")) %>% 
  left_join(RetirementRates, by = c("Age"))

#View(SeparationRates)

######################

#View(SeparationRates %>% select(RetirementType(SeparationRates$Age,SeparationRates$YOS)[1]))

#If you're eligible for normal retirement (DROP eligible), use the retirement rates, or else use term rates
SeparationRates <- SeparationRates %>% 
  mutate(retirement_type = RetirementType(Age,YOS),
         
         SepRateMale = ifelse(retirement_type == "Normal", RetMale, TermMale),
         SepRateFemale = ifelse(retirement_type == "Normal", RetFemale, TermFemale),
         SepRate = ((SepRateMale+SepRateFemale)/2)) %>% 
  group_by(entry_age) %>% 
  mutate(RemainingProb = cumprod(1 - lag(SepRate, default = 0)),
         SepProb = lag(RemainingProb, default = 1) - RemainingProb) %>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% select(Age, YOS, RemainingProb, SepProb)

#Custom function to calculate cumulative future values
cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}

#colnames(SalaryGrowth)[2] <- "YOS"
#Create a long-form table of Age and YOS and merge with salary data
SalaryData <- expand_grid(Age, YOS) %>% 
  mutate(entry_age = Age - YOS) %>%    #Add entry age
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(entry_age) %>% 
  left_join(SalaryEntry, by = "entry_age") %>% 
  left_join(SalaryGrowth, by = c("YOS"))


#######################################
#################
#################
#################


#View(SalaryData))
#Calculate FAS and cumulative EE contributions
#colnames(SalaryData)[7] <- "salary_increase"
benefit_cal <- function(
  output = "NC",
  DB_ARR = ARR, 
  DCreturn = DC_return,
  DB_EE = DB_EE_cont,
  DC_EE = DC_EE_cont,
  DC_ER = DC_ER_cont,
  DB_COLA = COLA,
  ea = HiringAge) {
  
  SalaryData <- SalaryData %>% 
    
    group_by(entry_age) %>% 
    mutate(Salary = start_sal*cumprod(1+lag(salary_increase,default = 0)),
           #Salary = pmin(Salary_gross, salary_cap),
           # IRSSalaryCap = pmin(Salary,IRSCompLimit),
           FinalAvgSalary = rollmean(lag(Salary), k = FinAvgSalaryYears, fill = NA, align = "right"),
           EEContrib = DB_EE*Salary,
           DBEEBalance = cumFV(Interest, EEContrib),
           CumulativeWage = cumFV(DB_ARR, Salary)) %>% 
    ungroup()
  
  
  #Survival Probability and Annuity Factor
  AnnFactorData <- MortalityTable %>% 
    select(Age, entry_age, mort) %>%
    group_by(entry_age) %>% 
    mutate(surv = cumprod(1 - lag(mort, default = 0)),
           surv_DR = surv/(1+DB_ARR)^(Age - entry_age),
           surv_DR_COLA = surv_DR * (1+DB_COLA)^(Age - entry_age),
           AnnuityFactor = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
    ungroup()
  
  #View(data.frame(shift(AnnFactorData$surv_DR_COLA, n = 1:101, type = "lead")))
  
  
  #For early retirement, the benefit equals normal benefit reduced by 5/12 of one percent for each month before preceding the normal retirement age (65)
  ########
  ReducedFactor <- expand_grid(Age, YOS) %>% 
    mutate(YearsNormRet = NormalRetAgeI - Age,
           RetType = RetirementType(Age, YOS),
           RF = pmax(0,ifelse(RetType == "Reduced", 1 - (5/12*12/100)*YearsNormRet,
                              ifelse(RetType == "No", 0, 1)))) %>% 
    rename(RetirementAge = Age) 
  
  # ReducedFactor_test <- ReducedFactor %>% 
  #   select(RetirementAge, YOS, RF) %>% 
  #   pivot_wider(names_from = YOS, values_from = RF)
  
  #Benefits, Annuity Factor and Present Value 
  # 4 normal benefit multipliers, depending on retirement age and YOS
  # Minimum monthly benefit = $34.25 * YOS, if age >= 65 and YOS >= 10
  # Maximum benefit = 100% of final average salary
  BenefitsTable <- expand_grid(Age, YOS, RetirementAge) %>% 
    mutate(entry_age = Age - YOS) %>% 
    filter(entry_age %in% SalaryEntry$entry_age) %>% 
    arrange(entry_age, Age, RetirementAge) %>% 
    left_join(SalaryData, by = c("Age", "YOS", "entry_age")) %>% 
    left_join(ReducedFactor %>% select(RetirementAge, YOS, RF), by = c("RetirementAge", "YOS")) %>%
    left_join(AnnFactorData %>% select(Age, entry_age, surv_DR, AnnuityFactor), by = c("RetirementAge" = "Age", "entry_age")) %>%
    #Rename surv_DR and AF to make clear that these variables are at retirement
    rename(surv_DR_ret = surv_DR, AF_Ret = AnnuityFactor) %>% 
    #Rejoin the table to get the surv_DR for the termination age
    left_join(AnnFactorData %>% select(Age, entry_age, surv_DR), by = c("Age", "entry_age")) %>% 
    mutate(MinBenefit = ifelse(RetirementAge >= 65 & YOS >=10, MinBenefitFactor*YOS*12, 0),
           BenMult = case_when(
             (RetirementAge >= 68 & YOS >= 8) | (YOS >= 36) ~ BenMult68_8_36,
             (RetirementAge >= 67 & YOS >= 8) | (YOS >= 35) ~ BenMult67_8_35,
             (RetirementAge >= 66 & YOS >= 8) | (YOS >= 34) ~ BenMult66_8_34,
             TRUE ~ BenMult65_8_33,
           ),
           ReducedFactMult = RF*BenMult, 
           PensionBenefit = pmin(pmax(ReducedFactMult * FinalAvgSalary*YOS, MinBenefit), FinalAvgSalary),    #Apply the max and min benefit
           AnnFactorAdj = AF_Ret * surv_DR_ret / surv_DR,
           PresentValue = ifelse(Age > RetirementAge, 0, PensionBenefit*AnnFactorAdj))
  
  #)
  
  #For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
  OptimumBenefit <- BenefitsTable %>% 
    group_by(entry_age, Age) %>% 
    summarise(MaxBenefit = max(PresentValue)) %>%
    mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
    ungroup()
  
  ####### Benefit Accrual & Normal Cost #######
  #### Real Pension Wealth = Pension Wealth adjusted for inflation
  #### Actuarial PV of Pension Wealth = Pension Wealth 
  #Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
  #####################################
  SalaryData <- SalaryData %>% 
    left_join(OptimumBenefit, by = c("Age", "entry_age")) %>% 
    left_join(SeparationRates, by = c("Age", "YOS")) %>%
    mutate(PenWealth = pmax(DBEEBalance,MaxBenefit),        #Members are assumed to elect the option with the greatest PV between a refund with interest and a deferred benefit
           RealPenWealth = PenWealth/(1 + assum_infl)^YOS,
           PVPenWealth = PenWealth/(1 + DB_ARR)^YOS * SepProb,
           PVCumWage = CumulativeWage/(1 + DB_ARR)^YOS * SepProb)
  
  
  #Calculate normal cost rate for each entry age
  NormalCost <- SalaryData %>% 
    group_by(entry_age) %>% 
    summarise(normal_cost = sum(PVPenWealth)/sum(PVCumWage)) %>% 
    ungroup()
  
  #View(NormalCost)
  
  #Calculate the aggregate normal cost
  NC_aggregate <- sum(NormalCost$normal_cost * SalaryEntry$start_sal * SalaryEntry$count_start)/
    sum(SalaryEntry$start_sal * SalaryEntry$count_start)
  
  #Calculate the aggregate normal cost
  NC_aggregate
  ################################
  
  
  ####### DC Account Balance 
  SalaryData2 <- SalaryData %>% 
    filter(entry_age == ea) %>% 
    select(Age, YOS, start_sal, salary_increase, Salary, RemainingProb) %>% 
    mutate(DC_EEContrib = Salary * DC_EE,
           DC_ERContrib = Salary * DC_ER,
           DC_Contrib = DC_EEContrib + DC_ERContrib,
           DC_balance = cumFV(DCreturn, DC_Contrib),
           RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
    left_join(SalaryData %>% select(Age, YOS, RealPenWealth), by = c("Age", "YOS")) %>% 
    mutate(RealHybridWealth = RealDC_balance + RealPenWealth) %>% 
    filter(Age <= 80)
  
  if (output == "NC") {
    return(NC_aggregate)
  } else if (output == "attrition") {
    return(SalaryData2 %>% 
             select(Age, RemainingProb))
  } else if (output == "DB"){
    return(SalaryData2 %>% 
             select(Age, RealPenWealth))
  } else if (output == "DC") {
    return(SalaryData2 %>% 
             select(Age, RealDC_balance))
  } else {
    return(SalaryData2 %>% 
             select(Age, RealHybridWealth))
  }
  
}

## Test benefit function
#Current DR is 6.8%
# NC <- benefit_cal(DB_ARR = 0.07)
# NC2 <- benefit_cal(DB_ARR = 0.06)
# DB_current <- benefit_cal(output = "DB") %>% 
#   rename(DB_current = RealPenWealth)
# DB_4 <- benefit_cal(output = "DB", DB_ARR = 0.04) %>% 
#   rename(DB_4 = RealPenWealth)
# DC_current <- benefit_cal(output = "DC") %>% 
#   rename(DC_current = RealDC_balance)
# DC_addER <- benefit_cal(output = "DC", DC_ER = DC_ER_cont + 0.03) %>% 
#   rename(DC_addER = RealDC_balance)
# 
# 
# output <- DB_current %>% 
#   left_join(DC_current, by = "Age") %>% 
#   left_join(DC_addER, by = "Age") %>% 
#   pivot_longer(-Age, names_to = "type", values_to = "wealth")
# 
# ggplot(output, aes(x= Age, y = wealth, col = type)) +
#   geom_line(size = 1) +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(0, 80, by = 10), 
#                      name = "Age (entry age at 27)", 
#                      expand = c(0,0)) +
#   scale_y_continuous(breaks = pretty_breaks(10),
#                      labels = dollar_format(),
#                      expand = c(0,0)) 
  
  



# 
# ## Graphing PWealth accrual
# ggplot(SalaryData, aes(Age,RealPenWealth/1000, group = entry_age, col = as.factor(entry_age)))+
#   geom_line(size = 1)+
#   theme_bw()+
#   scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x), 
#                      name = "Age (Entry age at 27)", expand = c(0,0)) + 
#   scale_y_continuous(breaks = seq(0, 5000, by = 100),labels = function(x) paste0("$",x), 
#                      name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) 
#

