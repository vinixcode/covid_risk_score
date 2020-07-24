
#! /usr/bin/env Rscript
# This script contains a single function to calculate the probability of hospitalzation, ICU and deaths given user input.
# install.packages('jsonlite')
source("~/projects/bellage/covid_risk_score/src/calculate_female_rate.R")
library(jsonlite)
library(purrr)

######## JUANES: The following code was copied from global_var.R ########

# Global variables can go here
#Flu data from CDC https://www.cdc.gov/flu/about/burden/index.html
prob_flu<- 35520883/(329.45*10^6)/26 #assume 26 weeks of flu season
hosp_flu<-490561/35520883
icu_flu<-0.075*hosp_flu #Beumer, M. C., et al. "Influenza virus and factors that are associated with ICU admission, pulmonary co-infections and ICU mortality." Journal of critical care 50 (2019): 59-65.
death_flu<-34157/35520883
  

fips<-""
#odds ratio for hand washing
hand_or<-0.45 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2190272/
#odds ratio for wearing PPE
ppe_or<-0.32 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2190272/
#transmissibility of regular close contact
transmissibility<-0.0045 #https://www.cdc.gov/mmwr/volumes/69/wr/pdfs/mm6909e1-H.pdf
#transmissibility of household close contact
transmissibility_household<- 0.105 #https://www.cdc.gov/mmwr/volumes/69/wr/pdfs/mm6909e1-H.pdf

# # susceptibility data for US, https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm
# susceptibility_total_cases = 4226
# total_hospitalized = 508
# total_icu = 121
# hosp_list =  c(2.05,  17.55, 24.75, 25.3,  36.05, 44.6,  50.8) / 100
# icu_list =   c(0,     3.1,   7.9,   7.95,  13.45, 20.75, 17.65) / 100
# infection hospitalization rate and fatality rate, https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext
age_list   = c(0,       10,      20,      30,      40,      50,      60,     70,     80)
hosp_list  = c(0,       0.0408,  1.04,    3.43,    4.25,    8.16,    11.8,   16.6,   18.4) / 100
#case to infection conversion 1.38% cfr, 0.657% infection fatality rate, use CDC ICU list
icu_list   = c(0,       0,       2,       2,       3.7,     5.05,    6.4,    9.3,    8.4) *.657/1.38 /100
death_list = c(0.00161, 0.00695, 0.0309,  0.0844,  0.161,   0.595,   1.93,   4.28,   7.8)  / 100

# Risk adjustment to suspetibility for comorbidities

## March 2020
# odds ratios, https://www.cdc.gov/mmwr/volumes/69/wr/mm6913e2.htm
# CCDC weekly, 2020 Vol No.2
#The Epidemiological Characteristics of an Outbreak of 2019 Novel
#Coronavirus Diseases (COVID-19) — China, 2020
#The Novel Coronavirus Pneumonia Emergency Response Epidemiology Team
# first is hospitalization odds ratio, second is ICU odds ratio, third is death odds ratio

## UPDATEs to OR June 2020
# For more info on assumptions and methodology, see 'doc/June2020_new_comorbidity_OR.xlsx'
# hospitalization: OR from https://www.cdc.gov/mmwr/volumes/69/wr/mm6925e1.htm?s_cid=mm6925e1_e&deliveryName=USCDC_921-DM30747#F1_down
#     Notes: lung was below 1 and brought up to 1. Other and immuno remain the pre-June 2020 numbers and are only used if no other condition.
# death https://www.medrxiv.org/content/10.1101/2020.05.06.20092999v1.full.pdf
#     Notes: hypertension was below 1 and brought up to 1. Other remains the pre-June 2020 numbers and is only used if no other condition.
# Obesity OR for ICU risk is from https://www.medrxiv.org/content/10.1101/2020.05.06.20092999v1.full.pdf

renal_or    = c(2.6, 5.82, 1.72) # death OR not available in CCDC
cvd_or      = c(1.4, 4.88, 1.27)
diabetes_or = c(3.1, 4.57, 1.79)
hyper_or    = c(1.1, 4.57, 1) # for ICU same as diabetes
smoker_or   = c(2.3, 2.64, 1.12) # death OR not available in CCDC
immune_or   = c(2.58, 2.86, 1.69) # death OR not available in CCDC
lung_or     = c(1, 2.83, 1.78)
obese_or  = c(1.9, 3.41, 1.46)
other_or    = c(4.21, 3.33, 6.11) # death OR not available in CCDC


#pregnant_or = c(1.23, 0.42, 1)
#neuro_or    = c(6.18, 2.30, 6.11) # death OR not available in CCDC
#liver_or    = c(2.44, 3.05, 6.11) # death OR not available in CCDC
#all_conditions_death_or = 27.84
# OR source: https://www.medrxiv.org/content/10.1101/2020.02.24.20027268v1
male_or     = c(1.8518, 1.85, 1.69)

#NYC county fips code
NY_fips_ls<-c("36005", "36047", "36061", "36081", "36085")


######## End of global_var.R code ########

risk2odds<-function(prob) {
  return (prob / (1 - prob))
}

odds2risk<-function(odds) {
  return (odds / (1 + odds))
}

logodds2risk <- function(logodds){
  return(exp(logodds)/(1 + exp(logodds)))
}

# urls class to store our urls
urls  = list(
  # CCDC
  ccdc_vol2_2020 = "https://www.unboundmedicine.com/medline/citation/32064853/[The_epidemiological_characteristics_of_an_outbreak_of_2019_novel_coronavirus_diseases__COVID_19__in_China]_",
  # CDC
  cdc_chatbot = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/index.html#cdc-chat-bot-open",
  cdc_get_ready = "https://www.cdc.gov/coronavirus/2019-ncov/daily-life-coping/get-your-household-ready-for-COVID-19.html",
  cdc_hand_hygiene = "https://www.cdc.gov/handhygiene/providers/guideline.html",
  cdc_high_risk = "https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-at-higher-risk.html",
  cdc_if_sick = "https://www.cdc.gov/coronavirus/2019-ncov/if-you-are-sick/steps-when-sick.html",
  cdc_mm6909e1 = "https://www.cdc.gov/mmwr/volumes/69/wr/pdfs/mm6909e1-H.pdf",
  cdc_mm6912e2 = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6912e2.htm",
  cdc_mm6913e2 = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6913e2.htm",
  cdc_ppe = "https://www.cdc.gov/coronavirus/2019-ncov/hcp/respirator-use-faq.html",
  cdc_prevention = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html",
  cdc_symptoms = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html",
  cdc_flu = "https://www.cdc.gov/flu/about/burden/index.html",
  cdc_hosp_June2020 = "https://www.cdc.gov/mmwr/volumes/69/wr/mm6925e1.htm",
  cdc_pregnancy = "https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-with-medical-conditions.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fneed-extra-precautions%2Fgroups-at-higher-risk.html#pregnancy",
  # NYT
  nytimes_data_article = "https://www.nytimes.com/article/coronavirus-county-data-us.html",
  # papers
  caramelo_etal_2020 = "https://www.medrxiv.org/content/10.1101/2020.02.24.20027268v1",
  russel_etal_2020 = "https://cmmid.github.io/topics/covid19/global_cfr_estimates.html",
  verity_etal_2020 = "https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext",
  open_safely = "https://www.medrxiv.org/content/10.1101/2020.05.06.20092999v1",
  wolfer_etall_2020 = "https://www.nature.com/articles/s41586-020-2196-x",
  covid_symptom_study = "https://covid19.joinzoe.com/us/about", 
  menni_etall_2020 = "https://www.nature.com/articles/s41591-020-0916-2",
  simonnet_etall_2020 = "https://onlinelibrary.wiley.com/doi/full/10.1002/oby.22831?af=R", 
  # social
  twitter_button = "https://twitter.com/intent/tweet?text=Find%20your%20COVID-19%20risk%20score!&url=https://19andme.shinyapps.io/covid_risk_score/",
  twitter_widget = "http://platform.twitter.com/widgets.js",
  facebook_button = "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2F19andme.shinyapps.io%2Fcovid_risk_score%2F",
  facebook_widget = "https://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v3.0"
)

# Parse arguments from command line
# to execute run: 
# Rscript susceptibility_wrapper.R 60 male 'list("is_cvd","is_diabetes")'
# Rscript susceptibility_wrapper.R 75 female 'NULL'
args <- commandArgs(trailingOnly = TRUE)
age <- args[1]
sex <- args[2]
input <- list(age = args[1], sex = args[2], conditions = eval(parse(text=args[3])))

calculate_susceptibility <- function (input){
    #input has the following fields:
    # age: numeric 
    # sex: string, expecting either "male" or "female", it can have "other", see line 40 if you want to modify the code to allow "other"
    # conditions: list of strings, expecting the following values:
    # "is_renal", if user selects "Chronic renal disease"
    # "is_cvd", if user selects "Cardiovascular disease"
    # "is_diabetes", if user selects "Diabetes"
    # "is_hyper", if user selects "Hypertension"
    # "is_smoker", if user selects "Current or former smoker"
    # "is_immune", if user selects "Immunocompromised condition"
    # "is_lung", if user selects "Chronic lung disease or asthma",
    # "is_other", if user selects "Other chronic disease",
    # "is_obese", if user selects "Obesity",
    # If user has no comorbidities, conditions will be NULL
    
    #check input validity
    # assertthat::assert_that(is.numeric(input$age), msg = "Age must be numeric.")
    assertthat::assert_that(tolower(input$sex) %in% c("male", "female", "other", "sex_other"), msg = "Sex must be one of the four: male, female, other, sex_other")
    
    # susceptibility calculation
    age = as.numeric(input$age)
    age_index = max(which(age_list <= age))
    
    # if sex is "sex_other" then use given probs if else (sex is female or male), calc female probs
    if(input$sex == "sex_other"){
      hosp_prob = hosp_list[age_index] # start with female and multiply up if user inputs male
      icu_prob = icu_list[age_index]
      death_prob = death_list[age_index]
    } else {
      hosp_prob = hosp_list_female[age_index] # start with female and multiply up if user inputs male
      icu_prob = icu_list_female[age_index]
      death_prob = death_list_female[age_index]
    }
    
    hosp_odds = risk2odds(hosp_prob)
    icu_odds = risk2odds(icu_prob)
    death_odds = risk2odds(death_prob)
    
    ## compile set of comorbidity ORs by which to adjust
    if (!is.null(input$conditions)>0){
      conditions_df <- map_df(input$conditions, ~tibble(condition=substr(.x, 4, nchar(.x)))) 
      conditions_df$hosp <- sapply(conditions_df$condition, function(x){eval(parse(text=paste0(x, "_or[1]")))})
      conditions_df$icu <- sapply(conditions_df$condition, function(x){eval(parse(text=paste0(x, "_or[2]")))})
      conditions_df$death <- sapply(conditions_df$condition, function(x){eval(parse(text=paste0(x, "_or[3]")))})
      
      ### hosp OR are mutually adjusted except for immuno and other - for these 2 only adjust if they are only condition
      ### ICU OR are not mutually adjusted, so use first 2 only
      ### Death OR are mutually adjusted except for other - for this one only adjust if it is only condition
      conditions_df <- conditions_df %>%
        mutate(hosp = ifelse(condition == "other" | condition == "immune",
                            ifelse(any(!condition %in% c("other","immune")), NA, hosp), hosp),
              icu = ifelse(rank(-1*icu)>2, NA, icu),
              death = ifelse(condition == "other",
                            ifelse(any(!condition %in% c("other")), NA, death), death))
      
      hosp_odds = hosp_odds * prod(conditions_df$hosp, na.rm=T)
      icu_odds = icu_odds * prod(conditions_df$icu, na.rm=T)
      death_odds = death_odds * prod(conditions_df$death, na.rm=T)
    }
    if (input$sex == "male") {
      hosp_odds = hosp_odds * male_or[1] # base odds are the female odds, multiplied by male_or if male
      icu_odds = icu_odds * male_or[2]
      death_odds = death_odds * male_or[3]
    }
    
    #Convert odds back to probabilities
    hosp_risk = odds2risk(hosp_odds)
    icu_risk = odds2risk(icu_odds)
    death_risk = odds2risk(death_odds)
    
    #Output is returned in JSON
    returnJson <- jsonlite::toJSON(list(hosp_risk = hosp_risk,
                icu_risk = icu_risk,
                death_risk = death_risk))
    return(returnJson)
}

#Execute function
calculate_susceptibility(input)

# examples
# input1 <- list(age = 50, sex = "male", conditions = NULL)
# calculate_susceptibility(input1)

# input2 <- list(age = 65, sex = "male", conditions = list("is_cvd", "is_hyper"))
# calculate_susceptibility(input2)

# input3 <- list(age = 57, sex = "other", conditions = list("is_other"))
# calculate_susceptibility(input3)

# input4 <- list(age = 32, sex = "male", conditions = list("is_obese"))
# calculate_susceptibility(input4)
