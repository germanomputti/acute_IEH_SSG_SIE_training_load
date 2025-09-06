#depois, criar outros dois scripts: um apenas com os models que envolvem a comparacao entre tipos de treino e um sem os tipos de treino. 
#depois, dois que gerem o output dos models, um com comparaçao entre tipos e um sem os tipos.  


###This code was used exclusively for testing the possible models, in accordance with the methodology described in the article.

library(readxl)
library(writexl)
library(lme4)
library(ggplot2) 


rm(list=ls())

###CHANGE HERE FOR ACTUAL ROOT DIRECTORY
root="/Users/germano/Library/CloudStorage/OneDrive-Pessoal/XUSP/GECIFEX/Producoes/Artigos/artigo 1 mestrado - agudo sessao/data analysis – ieh – ssg and sit"

data_frame_directory=paste0(root,"/data/ieh ssg and sit data.xlsx")
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

levels(data_frame$training_session) = c("SSG","SIT")

data_frame_SIT=subset(data_frame,data_frame$training_session == "SIT")
data_frame_SSG=subset(data_frame,data_frame$training_session == "SSG")


#Removing negative values to fit gamma distribution. This needs to be reversed for data interpretation. 
data_frame_SSG$total_playerscore=3+data_frame_SSG$total_playerscore
data_frame_SSG$positive_playerscore=3+data_frame_SSG$positive_playerscore
data_frame_SSG$negative_playerscore=3+data_frame_SSG$negative_playerscore

###



controle1 = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))


chosen_models_SIT= matrix(
    data= c(
    "total_distance", "gamma",
    "average_power", "normal",
    "max_power", "gamma",
    "fatigue_index","normal"
    ),
    byrow=TRUE,ncol=2)

chosen_models_SIT=as.data.frame(chosen_models_SIT)
names(chosen_models_SIT)= c("variable","distribution")

chosen_models_SSG= matrix(
    data= c(
    "total_playerscore","gamma",
    "positive_playerscore","gamma",
    "negative_playerscore","gamma"
    ),
    byrow=TRUE,ncol=2)

chosen_models_SSG=as.data.frame(chosen_models_SSG)
names(chosen_models_SSG)= c("variable","distribution")




create_model=function (variable,distribution,data_frame) {

if (distribution == "gamma") {
  
    model = glmer(formula=as.formula(paste0(variable, "~ condition + (1|id)")),
data=data_frame,
family=Gamma(link="identity"),
control=glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
 
} else {
        

model=lmer(formula=as.formula(paste0(variable, "~ condition + (1|id)")),data=data_frame)

}

return (model)
}



models_list=list()


for (i in c(1:nrow(chosen_models_SIT))) {

variable=  chosen_models_SIT$variable[i]
distribution=  chosen_models_SIT$distribution[i]
models_list[[variable]]=create_model(variable,distribution,data_frame_SIT)

}

for (i in c(1:nrow(chosen_models_SSG))) {

variable=  chosen_models_SSG$variable[i]
distribution=  chosen_models_SSG$distribution[i]
models_list[[variable]]=create_model(variable,distribution,data_frame_SSG)

}


