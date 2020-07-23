
#! /usr/bin/env Rscript
# This script contains a single function to calculate the probability of hospitalzation, ICU and deaths given user input.
# install.packages('jsonlite')
source("~/projects/bellage/covid_risk_score/src/global_var_v2.R")
source("~/projects/bellage/covid_risk_score/src/calculate_female_rate.R")
library(jsonlite)
library(purrr)

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
    
    #Update odds based on comorbidities
    #If user input more than two comorbidities, only the first two are considered.
    #[IMPORTANT]:The order of comorbidities can not be changed, renal disease has to be the first because its OR is the highest.
    for (condition_id in input$conditions[1:min(length(input$conditions), 2)]) {
        # remove "is_" prefix
        condition_root = substr(condition_id, 4, nchar(condition_id))
        hosp_odds = hosp_odds * eval(parse(text=paste0(condition_root, "_or[1]")))
        icu_odds = icu_odds * eval(parse(text=paste0(condition_root, "_or[2]")))
        death_odds = death_odds * eval(parse(text=paste0(condition_root, "_or[3]")))
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
