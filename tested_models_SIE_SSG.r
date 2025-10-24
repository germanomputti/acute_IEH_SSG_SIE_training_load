
###This code was used exclusively for testing the possible models, in accordance with the methodology described in the article.

library(readxl)
library(dplyr)
library(writexl)
library(lme4)
library(ggResidpanel)
library(lattice)
library(ggplot2)
library(lmerTest) 


rm(list=ls())

###CHANGE HERE FOR ACTUAL ROOT DIRECTORY
root=""

data_frame_directory=paste0(root,"/data/ieh ssg and sie data.xlsx")
data_frame=read_excel(data_frame_directory)


#Organizing DF

for (i in names(data_frame)[1:5]){
    
    data_frame[[i]]=as.factor(data_frame[[i]])
}


for (i in names(data_frame)[5:ncol(data_frame)]){
    
    data_frame[[i]]=as.numeric(data_frame[[i]])
}

#removing drop out 
data_frame=subset(data_frame,data_frame$id != "9")



levels(data_frame$condition)=c("HIE","NOR")

data_frame$condition <- factor(data_frame$condition, levels = c("NOR","HIE"))

levels(data_frame$training_session) = c("SSG","SIE")


##Function for exploratory visual analysis 

exploratory_graphs=function(variable,data) {

boxplot(data[[variable]] ~ training_session:condition,data=data,ylab=variable)





for (i in levels(data$training_session)) {

     data_subset <- subset(data,data$training_session == i)

grafico=ggplot(data_subset, 
aes(x = condition, y = .data[[variable]], group = id, color = id)) +
  geom_line() + 
  geom_point() +  
  labs(
    title = paste(variable,"graph for each id. Training session:", i),
    x = "condition",
    y = variable,
    color = "id"
  ) +
  theme_minimal()
print(grafico)


}}



###Checking model assumption. 
  #This function was used directly in R terminal, after the creation of each model. 

check_models = function (model,cooks=FALSE) {

print(model@call$data)
dot_plot=dotplot(ranef(model, condVar=TRUE), strip = FALSE)
print(dot_plot)

residpanel=resid_panel(model, smoother = TRUE, qqbands = TRUE)
print(residpanel)

print(AIC(model))

return()

}

#Control for glmer()
controle1 = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))






###lactate
exploratory_graphs("lactate",data_frame)
model_normal_lactate=lmer(lactate ~ condition + training_session + condition:training_session + (1|id), data=data_frame)
check_models(model_normal_lactate)


model_gamma_lactate=glmer(lactate ~ condition + training_session + condition:training_session + (1|id), 
data=data_frame,
family=Gamma(link="identity"),
control=controle1)
check_models(model_gamma_lactate)

lactate=model_normal_lactate


###effort
exploratory_graphs("effort",data_frame)
model_normal_effort=lmer(effort ~ condition + training_session + condition:training_session + (1|id), data=data_frame)
check_models(model_normal_effort)


model_gamma_effort=glmer(effort ~ condition + training_session + condition:training_session + (1|id), 
data=data_frame,
family=Gamma(link="identity"),
control=controle1)
check_models(model_gamma_effort)

effort=model_normal_effort



###discomfort
exploratory_graphs("discomfort",data_frame)
model_normal_discomfort=lmer(discomfort ~ condition + training_session + condition:training_session + (1|id), data=data_frame)
check_models(model_normal_discomfort)


model_gamma_discomfort=glmer(discomfort ~ condition + training_session + condition:training_session + (1|id), 
data=data_frame,
family=Gamma(link="identity"),
control=controle1)
check_models(model_gamma_discomfort)

discomfort=model_normal_discomfort


###breathing


exploratory_graphs("breathing",data_frame)
model_normal_breathing=lmer(breathing ~ condition + training_session + condition:training_session + (1|id), data=data_frame)
check_models(model_normal_breathing)


model_gamma_breathing=glmer(breathing ~ condition + training_session + condition:training_session + (1|id), 
data=data_frame,
family=Gamma(link="identity"),
control=controle1)
check_models(model_gamma_breathing)

breathing=model_normal_breathing


###lower_limb


exploratory_graphs("lower_limb",data_frame)
model_normal_lower_limb=lmer(lower_limb ~ condition + training_session + condition:training_session + (1|id), data=data_frame)
check_models(model_normal_lower_limb)


model_gamma_lower_limb=glmer(lower_limb ~ condition + training_session + condition:training_session + (1|id), 
data=data_frame,
family=Gamma(link="identity"),
control=controle1)
check_models(model_gamma_lower_limb)

lower_limb=model_normal_lower_limb






#####auc_hr 
exploratory_graphs("auc_hr",data_frame)

model_normal_auc_hr=lmer(auc_hr ~ condition + training_session + condition:training_session + (1|id),  data=data_frame)
model_gamma_auc_hr=glmer(auc_hr ~ condition + training_session + condition:training_session + (1|id),  data=data_frame,family=Gamma(link="identity"),control=controle1)


check_models(model_normal_auc_hr) 
check_models(model_gamma_auc_hr)

auc_hr=modelo_normal_auc_hr


####max_hr

graficos_exploratorios("max_hr",data_frame)
model_normal_max_hr=lmer(max_hr ~ condition + training_session + condition:training_session + (1|id),  data=data_frame)
model_gamma_max_hr=glmer(max_hr ~ condition + training_session + condition:training_session + (1|id),  data=data_frame, family=Gamma(link="identity"),control=controle1)


check_models(modelo_normal_max_hr) 
check_models(modelo_gamma_max_hr) 

max_hr=modelo_gamma_max_hr



training_load_list=list(lactate=lactate,
effort=effort,
discomfort=discomfort,
breathing=breathing,
lower_limb=lower_limb,
auc_hr=auc_hr,
max_hr=max_hr)

####
#####
#####
### SIE only

data_frame_SIE=subset(data_frame,data_frame$training_session == "SIE")


##### total_distance

exploratory_graphs("total_distance",data_frame_SIE)
model_normal_total_distance=lmer(total_distance ~ condition + (1|id),data=data_frame_SIE)
model_gamma_total_distance=glmer(total_distance ~ condition + (1|id),data=data_frame_SIE,family=Gamma(link="identity"),control=controle1)

check_models(model_normal_total_distance)
check_models(model_gamma_total_distance) 


total_distance=model_gamma_total_distance


####average_power

exploratory_graphs("average_power",data_frame_SIE)

model_normal_average_power=lmer(average_power ~ condition + (1|id),data=data_frame_SIE)

model_gamma_average_power=glmer(average_power ~ condition + (1|id),data=data_frame_SIE,family=Gamma(link="identity"),control=controle1)

check_models(model_normal_average_power) 
check_models(model_gamma_average_power) 

average_power=model_normal_average_power


#### max_power

exploratory_graphs("max_power",data_frame_SIE)
model_normal_max_power=lmer(max_power ~ condition + (1|id),data=data_frame_SIE)

model_gamma_max_power=glmer(max_power ~ condition + (1|id),data=data_frame_SIE,family=Gamma(link="identity"),control=controle1)

check_models(model_normal_max_power) 
check_models(model_gamma_max_power) 


max_power=model_gamma_max_power

##### fatigue_index

exploratory_graphs("fatigue_index",data_frame_SIE)

model_normal_fatigue_index=lmer(fatigue_index ~ condition + (1|id),data=data_frame_SIE)
model_gamma_fatigue_index=glmer(fatigue_index ~ condition + (1|id),data=data_frame_SIE,family=Gamma(link="identity"),control=controle1)


check_models(model_normal_fatigue_index) 
check_models(model_gamma_fatigue_index) 

fatigue_index=model_normal_fatigue_index

list_SIE_models=list(total_distance=total_distance,
max_power=max_power,
average_power=average_power,
fatigue_index=fatigue_index)


#
####
###### Only JR
data_frame_SSG=subset(data_frame,data_frame$training_session == "SSG")


#Removing negative values to fit gamma distribution. This needs to be reversed for data interpretation. 
data_frame_SSG$total_playerscore=3+data_frame_SSG$total_playerscore
data_frame_SSG$positive_playerscore=3+data_frame_SSG$positive_playerscore
data_frame_SSG$negative_playerscore=3+data_frame_SSG$negative_playerscore

#####total_playerscore

exploratory_graphs("total_playerscore",data_frame_SSG)

model_normal_total_playerscore=lmer(formula=total_playerscore ~ condition + (1|id),data=data_frame_SSG)

check_models(model_normal_total_playerscore)

model_gamma_total_playerscore=glmer(formula=total_playerscore ~ condition + (1|id),
data=data_frame_SSG,
family=Gamma(link="identity"),
control=controle1)

check_models(model_gamma_total_playerscore) 





#####positive_playerscore

exploratory_graphs("positive_playerscore",data_frame_SSG)

model_normal_positive_playerscore=lmer(formula=positive_playerscore ~ condition + (1|id),data=data_frame_SSG)

check_models(model_normal_positive_playerscore)

model_gamma_positive_playerscore=glmer(formula=positive_playerscore ~ condition + (1|id),
data=data_frame_SSG,
family=Gamma(link="identity"),
control=controle1)

check_models(model_gamma_positive_playerscore) 




#####negative_playerscore

exploratory_graphs("negative_playerscore",data_frame_SSG)

model_normal_negative_playerscore=lmer(formula=negative_playerscore ~ condition + (1|id),data=data_frame_SSG)

check_models(model_normal_negative_playerscore)

model_gamma_negative_playerscore=glmer(formula=negative_playerscore ~ condition + (1|id),
data=data_frame_SSG,
family=Gamma(link="identity"),
control=controle1)

check_models(model_gamma_negative_playerscore) 


list_ssg_models=list(total_playerscore=total_playerscore,
positive_playerscore=positive_playerscore,
negative_playerscore=negative_playerscore)