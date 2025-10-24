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
source(paste0(root,"/chosen_models_training_load.r")) 

rm(list = setdiff(ls(), c("models_list","root")))





results_single_model = function(model, export_directory){
  formula_model=formula(model) 
variable=as.character(formula_model[2])


#estimated means
emmean_condition_training_session=(emmeans(model,~condition:training_session))
emmean_condition=(emmeans(model,~condition))
emmean_training_session=(emmeans(model,~training_session))

#comparisons and post-hoc tests
contrast_emmean_condition_training_session=as.data.frame(contrast(emmean_condition_training_session, method = "pairwise", interaction = TRUE,adjust="bonferroni"))
contrast_emmean_condition_training_session$contrast = paste(contrast_emmean_condition_training_session[[1]], contrast_emmean_condition_training_session[[2]])
contrast_emmean_condition_training_session <- contrast_emmean_condition_training_session[, !(names(contrast_emmean_condition_training_session) %in% names(contrast_emmean_condition_training_session)[1:2])]
contrast_emmean_condition_training_session <- contrast_emmean_condition_training_session[, c("contrast", setdiff(names(contrast_emmean_condition_training_session), "contrast"))]


post_hoc_condition_training_session=as.data.frame(pairs(emmean_condition_training_session,adjust="bonferroni"))
post_hoc_condition=as.data.frame(pairs(emmean_condition,adjust="bonferroni"))
post_hoc_training_session=as.data.frame(pairs(emmean_training_session,adjust="bonferroni"))

emmean_condition_training_session=as.data.frame(emmean_condition_training_session)
emmean_condition=as.data.frame(emmean_condition)
emmean_training_session=as.data.frame(emmean_training_session)

emmean_condition_training_session=cbind(variable,emmean_condition_training_session)
emmean_condition=cbind(variable,emmean_condition)
emmean_training_session=cbind(variable,emmean_training_session)


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


###for levels of condition and training session --> repeated measures
###for levels of condition only --> repeated measures, mean values for training session
###for levels of training session only --> repeated measures, mean values for condition 

#number of observations by condition and training sessions
number_observations=observed_data %>%
     count(training_session, condition)


#Adding number of observations on emms objects
#### training_session
nobs_training_session=number_observations %>%
     group_by(training_session) %>%
     summarise(n_obs = max(n), .groups = "drop")
emmean_training_session = emmean_training_session %>%
     left_join(nobs_training_session, by = "training_session")


#### condition
nobs_condition=number_observations %>%
     group_by(condition) %>%
     summarise(n_obs = max(n), .groups = "drop")
emmean_condition = emmean_condition %>%
     left_join(nobs_condition, by = "condition")



#training_session and condition
nobs_condition_training_session=number_observations %>%
     group_by(training_session,condition) %>%
     summarise(n_obs = n, .groups = "drop")
emmean_condition_training_session = emmean_condition_training_session %>%
     left_join(nobs_condition_training_session, by = c("training_session", "condition"))


####Standard deviation – sd
emmean_training_session$sd=emmean_training_session$SE*sqrt(emmean_training_session$n_obs)
emmean_condition$sd=emmean_condition$SE*sqrt(emmean_condition$n)
emmean_condition_training_session$sd=emmean_condition_training_session$SE*sqrt(emmean_condition_training_session$n)



# cohensd condition e training session

  post_hoc_condition_training_session_expanded <- post_hoc_condition_training_session %>%
  mutate(contrast_temp = contrast) %>%
  separate(contrast_temp, into = c("condition_training_session1", "condition_training_session2"), sep = " - ") %>%
  relocate(condition_training_session1, condition_training_session2, .after = last_col())

emmean_condition_training_session$condition_training_session=paste(emmean_condition_training_session$condition,emmean_condition_training_session$training_session)


  post_hoc_condition_training_session_expanded <- post_hoc_condition_training_session_expanded %>%
  left_join(
    emmean_condition_training_session %>%
      select(condition_training_session, n_obs) %>%
      rename(condition_training_session1 = condition_training_session, n1 = n_obs),
    by = "condition_training_session1"
  ) %>%
  left_join(
    emmean_condition_training_session %>%
      select(condition_training_session, n_obs) %>%
      rename(condition_training_session2 = condition_training_session, n2 = n_obs),
    by = "condition_training_session2"
  ) %>%
  left_join(
    emmean_condition_training_session %>%
      select(condition_training_session, sd) %>%
      rename(condition_training_session1 = condition_training_session, sd1 = sd),
    by = "condition_training_session1"
  ) %>%
  left_join(
    emmean_condition_training_session %>%
      select(condition_training_session, sd) %>%
      rename(condition_training_session2 = condition_training_session, sd2 = sd),
    by = "condition_training_session2"
  )  
  #
  #cohends
  post_hoc_condition_training_session_expanded$cohens_d=(post_hoc_condition_training_session_expanded$estimate/(post_hoc_condition_training_session_expanded$SE*sqrt(min(post_hoc_condition_training_session_expanded$n1,post_hoc_condition_training_session_expanded$n2))))

  #ci cohends  
  post_hoc_condition_training_session_expanded$CI_cohens_d=1.96*sqrt(
    (1/min(post_hoc_condition_training_session_expanded$n1,post_hoc_condition_training_session_expanded$n2)) +
    (post_hoc_condition_training_session_expanded$cohens_d^2/(2*min(post_hoc_condition_training_session_expanded$n1,post_hoc_condition_training_session_expanded$n2)))
  )




# cohens d training_session


post_hoc_training_session_expanded <- post_hoc_training_session %>%
  mutate(contrast_temp = contrast) %>%
  separate(contrast_temp, into = c("training_session1", "training_session2"), sep = " - ") %>%
  relocate(training_session1, training_session2, .after = last_col())

  post_hoc_training_session_expanded<- post_hoc_training_session_expanded %>%
  left_join(
    emmean_training_session %>%
      select(training_session, n_obs) %>%
      rename(training_session1 = training_session, n1 = n_obs),
    by = "training_session1"
  ) %>%
  left_join(
    emmean_training_session %>%
      select(training_session, n_obs) %>%
      rename(training_session2 = training_session, n2 = n_obs),
    by = "training_session2"
  ) %>%
  left_join(
    emmean_training_session %>%
      select(training_session, sd) %>%
      rename(training_session1 = training_session, sd1 = sd),
    by = "training_session1"
  ) %>%
  left_join(
    emmean_training_session %>%
      select(training_session, sd) %>%
      rename(training_session2 = training_session, sd2 = sd),
    by = "training_session2"
  )

#paired cohens d
  post_hoc_training_session_expanded$cohens_d=(post_hoc_training_session_expanded$estimate/(post_hoc_training_session_expanded$SE*sqrt(min(post_hoc_training_session_expanded$n1,post_hoc_training_session_expanded$n2))))

  #ci cohens d

  post_hoc_training_session_expanded$CI_cohens_d=1.96*sqrt(
    (1/min(post_hoc_training_session_expanded$n1,post_hoc_training_session_expanded$n2)) +
    (post_hoc_training_session_expanded$cohens_d^2/(2*min(post_hoc_training_session_expanded$n1,post_hoc_training_session_expanded$n2)))
  )


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


  post_hoc_condition_training_session_expanded$upper_CI_cohens_d= post_hoc_condition_training_session_expanded$cohens_d +
  post_hoc_condition_training_session_expanded$CI_cohens_d

  post_hoc_condition_training_session_expanded$lower_CI_cohens_d= post_hoc_condition_training_session_expanded$cohens_d -
  post_hoc_condition_training_session_expanded$CI_cohens_d


  post_hoc_training_session_expanded$lower_CI_cohens_d= post_hoc_training_session_expanded$cohens_d -
  post_hoc_training_session_expanded$CI_cohens_d

  post_hoc_training_session_expanded$upper_CI_cohens_d= post_hoc_training_session_expanded$cohens_d +
  post_hoc_training_session_expanded$CI_cohens_d

  post_hoc_condition_expanded$lower_CI_cohens_d= post_hoc_condition_expanded$cohens_d -
  post_hoc_condition_expanded$CI_cohens_d

  post_hoc_condition_expanded$upper_CI_cohens_d= post_hoc_condition_expanded$cohens_d +
  post_hoc_condition_expanded$CI_cohens_d



###Removing unused columns and adjusting DFs for p<0.05

 ##############
post_hoc_condition_training_session=post_hoc_condition_training_session_expanded[,names(post_hoc_condition_training_session_expanded) %in% c(
   "contrast"  ,        "estimate"   ,       "SE"  ,             
 "df"       ,         "z.ratio"     , "t.ratio",      "p.value"   ,       
 "cohens_d"  ,  "CI_cohens_d",
  "lower_CI_cohens_d","upper_CI_cohens_d"
)]


post_hoc_condition=post_hoc_condition_expanded[,names(post_hoc_condition_expanded) %in% c(
   "contrast"  ,        "estimate"   ,       "SE"  ,             
 "df"       ,         "z.ratio"     , "t.ratio",     "p.value"   ,       
 "cohens_d"  ,  "CI_cohens_d" , 
 "lower_CI_cohens_d","upper_CI_cohens_d" 
)]

post_hoc_training_session=post_hoc_training_session_expanded[,names(post_hoc_training_session_expanded) %in% c(
   "contrast"  ,        "estimate"   ,       "SE"  ,             
 "df"       ,         "z.ratio"     , "t.ratio",     "p.value"   ,       
 "cohens_d"  ,  "CI_cohens_d" ,
  "lower_CI_cohens_d","upper_CI_cohens_d"
)]


#adding variable

variable=as.character(formula_model[2])
post_hoc_condition_training_session=cbind(variable,post_hoc_condition_training_session)
post_hoc_condition=cbind(variable,post_hoc_condition)
post_hoc_training_session=cbind(variable,post_hoc_training_session)
fixed_effects=cbind(variable,fixed_effects)
contrast_emmean_condition_training_session=cbind(variable,contrast_emmean_condition_training_session)




#no cohens d or CI
fixed_effects$cohens_d=NA
fixed_effects$CI_cohens_d=NA 
fixed_effects$upper_CI_cohens_d=NA 
fixed_effects$lower_CI_cohens_d=NA 



contrast_emmean_condition_training_session$cohens_d=NA
contrast_emmean_condition_training_session$CI_cohens_d=NA 
contrast_emmean_condition_training_session$upper_CI_cohens_d=NA 
contrast_emmean_condition_training_session$lower_CI_cohens_d=NA 





#comparison

fixed_effects$post_hoc="fixed_effect"
post_hoc_condition_training_session$post_hoc="condition_training_session"
post_hoc_condition$post_hoc="condition"
post_hoc_training_session$post_hoc="training_session"
contrast_emmean_condition_training_session$post_hoc="slope"


###estimates CIs

post_hoc_condition_training_session$estimate_CI=post_hoc_condition_training_session$SE*1.96
post_hoc_condition_training_session$upperCI_estimate=post_hoc_condition_training_session$estimate + post_hoc_condition_training_session$estimate_CI
post_hoc_condition_training_session$lowerCI_estimate=post_hoc_condition_training_session$estimate - post_hoc_condition_training_session$estimate_CI

post_hoc_condition$estimate_CI=post_hoc_condition$SE*1.96
post_hoc_condition$upperCI_estimate=post_hoc_condition$estimate + post_hoc_condition$estimate_CI
post_hoc_condition$lowerCI_estimate=post_hoc_condition$estimate - post_hoc_condition$estimate_CI

post_hoc_training_session$estimate_CI=post_hoc_training_session$SE*1.96
post_hoc_training_session$upperCI_estimate=post_hoc_training_session$estimate + post_hoc_training_session$estimate_CI
post_hoc_training_session$lowerCI_estimate=post_hoc_training_session$estimate - post_hoc_training_session$estimate_CI

contrast_emmean_condition_training_session$estimate_CI=contrast_emmean_condition_training_session$SE*1.96
contrast_emmean_condition_training_session$upperCI_estimate=contrast_emmean_condition_training_session$estimate + contrast_emmean_condition_training_session$estimate_CI
contrast_emmean_condition_training_session$lowerCI_estimate=contrast_emmean_condition_training_session$estimate - contrast_emmean_condition_training_session$estimate_CI


fixed_effects$estimate_CI=fixed_effects$`Std. Error`*1.96
fixed_effects$upperCI_estimate=fixed_effects$Estimate + fixed_effects$estimate_CI
fixed_effects$lowerCI_estimate=fixed_effects$Estimate - fixed_effects$estimate_CI


column_names=c("variable","tested_difference","estimate","SE","df","tvalue_zvalue","pvalue",       
 "cohens_d"  ,  "CI_cohens_d" ,
 "lower_CI_cohens_d","upper_CI_cohens_d","post_hoc","estimate_CI",
"upperCI_estimate","lowerCI_estimate")


 
names(post_hoc_condition_training_session)=column_names
names(post_hoc_condition)=column_names
names(post_hoc_training_session)=column_names
names(fixed_effects)=column_names
names(contrast_emmean_condition_training_session)=column_names

emmean_condition_training_session=emmean_condition_training_session %>% select(-condition_training_session)

#adjusting names on emmeans dfs

names(emmean_condition_training_session)=  c("variable","condition","training_session","emmean","SE","df","lower.CL","upper.CL","n_obs","sd")
names(emmean_condition)=  c("variable","condition","emmean","SE","df","lower.CL","upper.CL","n_obs","sd")
names(emmean_training_session)=  c("variable","training_session","emmean","SE","df","lower.CL","upper.CL","n_obs","sd")

###filtering p<0.05



results_all_p_values=rbind(fixed_effects,post_hoc_condition_training_session,post_hoc_condition,post_hoc_training_session,contrast_emmean_condition_training_session)
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
"Emm condition training_session"=emmean_condition_training_session,
"Emm condition" = emmean_condition,
"Emm training_session"= emmean_training_session,
"Contr condition training_session"=contrast_emmean_condition_training_session,
"Post hoc condition training_session"= post_hoc_condition_training_session,
"Post hoc condition"= post_hoc_condition,
"Post hoc training_session"= post_hoc_training_session)


file_name=paste(as.character(formula_model)[2])






write_xlsx(results_model,paste0(export_directory,"/",file_name,".xlsx"))

return(list(emmean_condition_training_session=emmean_condition_training_session,
emmean_condition = emmean_condition,
emmean_training_session = emmean_training_session,
results_p_005=results_p_005))

}




#######################
######
###
##
#
##### Creating output DFs

emm_condition_training_session=data.frame(variable=character(0),
    condition=character(0),
    training_session=character(0),
    emmean=numeric(0),
    SE=numeric(0),
    df=numeric(0),
    lower.CL=numeric(0),
    upper.CL=numeric(0),
    n_obs=numeric(0),
    sd=numeric(0)
    )

emm_training_session=data.frame(
    training_session=character(0),
    variable=character(0),
    emmean=numeric(0),
    SE=numeric(0),
    df=numeric(0),
    lower.CL=numeric(0),
    upper.CL=numeric(0),
    n_obs=numeric(0),
    sd=numeric(0)
)


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
model_output_directory=paste0(root,"/created models – training load")

if (!dir.exists(model_output_directory)) {
  dir.create(model_output_directory)
}





for (i in 1:length(models_list)) { 

model=models_list[[i]]
formula_model=formula(model)
print(formula_model)

    output_model=results_single_model(model,model_output_directory)
    emm_condition_training_session=rbind(emm_condition_training_session,output_model[[1]])
    emm_condition=rbind(emm_condition,output_model[[2]])
    emm_training_session=rbind(emm_training_session,output_model[[3]])
    p_005=rbind(p_005,output_model[[4]])
    

}





estimated_means=list("Emm condition"=emm_condition,
"Emm condition training_session"=emm_condition_training_session,
"Emm training_session" = emm_training_session
)



model_summary_directory=paste0(root,"/model summary – training load")

if (!dir.exists(model_summary_directory)) {
  dir.create(model_summary_directory)
}

write_xlsx(p_005,paste0(model_summary_directory,"/p_005.xlsx"))
write_xlsx(estimated_means,paste0(model_summary_directory,"/emms.xlsx"))



