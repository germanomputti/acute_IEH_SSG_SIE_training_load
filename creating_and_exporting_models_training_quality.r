##This script creates and exports each model and two spreadsheets containing estimated means and all comparisons with p<0.05 for all models

library(emmeans)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(jtools)

rm(list=ls()) 
###CHANGE HERE FOR ACTUAL ROOT DIRECTORY
root=""
source(paste0(root,"/chosen_models_training_quality.r")) 

rm(list = setdiff(ls(), c("models_list","root")))






results_single_model = function(model, export_directory){
  
formula_model=formula(model) 
variable=as.character(formula_model[2])


#estimated means

emmean_condition=(emmeans(model,~condition))



post_hoc_condition=as.data.frame(pairs(emmean_condition,adjust="bonferroni"))



emmean_condition=as.data.frame(emmean_condition)



emmean_condition=cbind(variable,emmean_condition)



##model data




if (class(model)[1] == "lmerModLmerTest") {
    distribution="gaussiana"
    conf_int_fixed_effects=as.data.frame(confint(model))
} else {
    distribution=model@resp$family[1]
    conf_int_fixed_effects=as.data.frame(confint(model,method = "Wald"))
    
}


#Fixed Effects
fixed_effects=as.data.frame(summary(model)$coefficients)
fixed_effects <- rownames_to_column(fixed_effects, var = "contrast")

#####When gamma distribution is used, there is no df column on fixed effects coeficients
#This column needs to be added here so we can later on use rbind() to have a summary from the results

if(length(fixed_effects) == 5){  
    fixed_effects$df=NA
    fixed_effects <- fixed_effects[, c(1:3, ncol(fixed_effects), 4:(ncol(fixed_effects) - 1))] 
} 



model_data=data.frame(
distribution=distribution,
formula_model=paste(as.character(formula_model)[2],
as.character(formula_model)[1],
as.character(formula_model)[3]),
aic=AIC(model),
total_observations=nobs(model))



# Observed and predicted data

fitted_data = fitted(model)
observed_data = model.frame(model)
observed_data$observed = fitted_data



##### cohensd




#number of observations by condition and training sessions
number_observations=observed_data %>%
     count(condition)


#### condition
nobs_condition=number_observations %>%
     group_by(condition) %>%
     summarise(n_obs = max(n), .groups = "drop")
emmean_condition = emmean_condition %>%
     left_join(nobs_condition, by = "condition")





####Standard deviation – sd

emmean_condition$sd=emmean_condition$SE*sqrt(emmean_condition$n)




# cohens d condition


post_hoc_condition_expanded <- post_hoc_condition %>%
  mutate(contrast_temp = contrast) %>%
  separate(contrast_temp, into = c("condition1", "condition2"), sep = " - ") %>%
  relocate(condition1, condition2, .after = last_col())

  post_hoc_condition_expanded<- post_hoc_condition_expanded %>%
  left_join(
    emmean_condition %>%
      select(condition, n_obs) %>%
      rename(condition1 = condition, n1 = n_obs),
    by = "condition1"
  ) %>%
  left_join(
    emmean_condition %>%
      select(condition, n_obs) %>%
      rename(condition2 = condition, n2 = n_obs),
    by = "condition2"
  ) %>%
  left_join(
    emmean_condition %>%
      select(condition, sd) %>%
      rename(condition1 = condition, sd1 = sd),
    by = "condition1"
  ) %>%
  left_join(
    emmean_condition %>%
      select(condition, sd) %>%
      rename(condition2 = condition, sd2 = sd),
    by = "condition2"
  )

#paired cohens d
  post_hoc_condition_expanded$cohens_d=(post_hoc_condition_expanded$estimate/(post_hoc_condition_expanded$SE*sqrt(min(post_hoc_condition_expanded$n1,post_hoc_condition_expanded$n2))))

  #CI cohens d

  post_hoc_condition_expanded$CI_cohens_d=1.96*sqrt(
    (1/min(post_hoc_condition_expanded$n1,post_hoc_condition_expanded$n2)) +
    (post_hoc_condition_expanded$cohens_d^2/(2*min(post_hoc_condition_expanded$n1,post_hoc_condition_expanded$n2)))
  )



    ##adding upper and lower CL.


  post_hoc_condition_expanded$lower_CI_cohens_d= post_hoc_condition_expanded$cohens_d -
  post_hoc_condition_expanded$CI_cohens_d

  post_hoc_condition_expanded$upper_CI_cohens_d= post_hoc_condition_expanded$cohens_d +
  post_hoc_condition_expanded$CI_cohens_d



###Removing unused columns and adjusting DFs for p<0.05



post_hoc_condition=post_hoc_condition_expanded[,names(post_hoc_condition_expanded) %in% c(
   "contrast"  ,        "estimate"   ,       "SE"  ,             
 "df"       ,         "z.ratio"     , "t.ratio",     "p.value"   ,       
 "cohens_d"  ,  "CI_cohens_d" , 
 "lower_CI_cohens_d","upper_CI_cohens_d" 
)]

#adding variable

variable=as.character(formula_model[2])

post_hoc_condition=cbind(variable,post_hoc_condition)

fixed_effects=cbind(variable,fixed_effects)





#no cohens d or CI
fixed_effects$cohens_d=NA
fixed_effects$CI_cohens_d=NA 
fixed_effects$upper_CI_cohens_d=NA 
fixed_effects$lower_CI_cohens_d=NA 



#comparison

fixed_effects$post_hoc="fixed_effect"

post_hoc_condition$post_hoc="condition"


###estimates CIs

post_hoc_condition$estimate_CI=post_hoc_condition$SE*1.96
post_hoc_condition$upperCI_estimate=post_hoc_condition$estimate + post_hoc_condition$estimate_CI
post_hoc_condition$lowerCI_estimate=post_hoc_condition$estimate - post_hoc_condition$estimate_CI


fixed_effects$estimate_CI=fixed_effects$`Std. Error`*1.96
fixed_effects$upperCI_estimate=fixed_effects$Estimate + fixed_effects$estimate_CI
fixed_effects$lowerCI_estimate=fixed_effects$Estimate - fixed_effects$estimate_CI


column_names=c("variable","tested_difference","estimate","SE","df","tvalue_zvalue","pvalue",       
 "cohens_d"  ,  "CI_cohens_d" ,
 "lower_CI_cohens_d","upper_CI_cohens_d","post_hoc","estimate_CI",
"upperCI_estimate","lowerCI_estimate")


 

names(post_hoc_condition)=column_names

names(fixed_effects)=column_names




#adjusting names on emmeans dfs


names(emmean_condition)=  c("variable","condition","emmean","SE","df","lower.CL","upper.CL","n_obs","sd")


###filtering p<0.05



results_all_p_values=rbind(fixed_effects,post_hoc_condition)
results_p_005=subset(results_all_p_values,results_all_p_values$pvalue < 0.05)

#removing intercept hypothesis test
results_p_005=subset(results_p_005,results_p_005$tested_difference != "(Intercept)")





#random effects
random_effects=as.data.frame(summ(model)$rcoeftable)



#results
results_model=list("model"=model_data,
"N obs" = number_observations,
"Fixed effects" = fixed_effects,
"Random effects"= random_effects,
"Predicted"= observed_data,

"Emm condition" = emmean_condition,



"Post hoc condition"= post_hoc_condition
)


file_name=paste(as.character(formula_model)[2])






write_xlsx(results_model,paste0(export_directory,"/",file_name,".xlsx"))

return(list(
emmean_condition = emmean_condition,
results_p_005=results_p_005))

}



#######################
######
###
##
#
##### Creating output DFs



emm_condition=data.frame(
    condition=character(0),
    variable=character(0),
    emmean=numeric(0),
    SE=numeric(0),
    df=numeric(0),
    lower.CL=numeric(0),
    upper.CL=numeric(0),
    n_obs=numeric(0),
    sd=numeric(0)
)

p_005=data.frame(variable=character(0),
    tested_difference=character(0),
    estimate=numeric(0),
    SE=numeric(0),
    df=numeric(0),
    tvalue_zvalue=numeric(0),
    pvalue=numeric(0),
    cohens_d=numeric(0),
    CI_cohens_d=numeric(0),
    lower_CI_cohends_model=numeric(0),
    upper_CI_cohends_model=numeric(0),
    post_hoc = character(0),
    estimate_CI=numeric(0),
    upperCI_estimate=numeric(0),
    lowerCI_estimate=numeric(0)
)




####output directory
model_output_directory=paste0(root,"/created models – training quality")

if (!dir.exists(model_output_directory)) {
  dir.create(model_output_directory)
}





for (i in 1:length(models_list)) { 

model=models_list[[i]]
formula_model=formula(model)
print(formula_model)

    output_model=results_single_model(model,model_output_directory)
    
    emm_condition=rbind(emm_condition,output_model[[1]])
    
    p_005=rbind(p_005,output_model[[2]])
    

}









model_summary_directory=paste0(root,"/model summary – training quality")

if (!dir.exists(model_summary_directory)) {
  dir.create(model_summary_directory)
}

write_xlsx(p_005,paste0(model_summary_directory,"/p_005.xlsx"))
write_xlsx(emm_condition,paste0(model_summary_directory,"/emms.xlsx"))



