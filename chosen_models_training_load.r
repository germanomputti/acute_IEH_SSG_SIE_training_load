#depois, criar outros dois scripts: um apenas com os models que envolvem a comparacao entre tipos de treino e um sem os tipos de treino. 
#depois, dois que gerem o output dos models, um com comparaçao entre tipos e um sem os tipos.  


###This code was used exclusively for testing the possible models, in accordance with the methodology described in the article.

library(readxl)
library(writexl)
library(lme4)
library(ggplot2) 


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




controle1 = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))


chosen_models= matrix(
    data= c(
    "lactate", "normal",
    "effort","normal",
    "discomfort","normal",
    "breathing","normal",
    "lower_limb","normal",
    "auc_hr","normal",
    "max_hr","gamma"
    ),
    byrow=TRUE,ncol=2)

chosen_models=as.data.frame(chosen_models)
names(chosen_models)= c("variable","distribution")



create_model=function (variable,distribution,data_frame) {

if (distribution == "gamma") {
  
    model = glmer(formula=as.formula(paste0(variable, "~ condition + training_session + condition:training_session + (1|id)")),
data=data_frame,
family=Gamma(link="identity"),
control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
 
} else {
        

model=lmer(formula=as.formula(paste0(variable, "~ condition + training_session + condition:training_session + (1|id)")),data=data_frame)

}

return (model)
}

models_list=list()

for (i in c(1:nrow(chosen_models))) {

variable=  chosen_models$variable[i]
distribution=   chosen_models$distribution[i]
models_list[[variable]]=create_model(variable,distribution,data_frame)

}