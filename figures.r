library(ggplot2)
library(readxl)
library(extrafont)
library(dplyr)
library(gridExtra)
library(patchwork)

rm(list=ls())
root="/Users/germano/Library/CloudStorage/OneDrive-Pessoal/XUSP/GECIFEX/Producoes/Artigos/artigo 1 mestrado - agudo sessao/data analysis – ieh – ssg and sit"
pd <- position_dodge(0.1)


data_training_load=read_excel(paste0(root,"/model summary – training load/emms.xlsx"),
sheet="Emm condition training_session")

data_training_load$CI = data_training_load$emmean - data_training_load$lower.CL


for (i in names(data_training_load)[1:3]){
    
    data_training_load[[i]]=as.factor(data_training_load[[i]])
}

levels(data_training_load$condition)=c("IEH","NOR")

data_training_load$condition <- factor(data_training_load$condition, levels = c("NOR","IEH"))

data_training_load$variable=factor(data_training_load$variable,levels=c("auc_hr",    "lactate" ,"max_hr","discomfort" , "effort"   ,  "lower_limb","breathing"))    

levels(data_training_load$variable) = c("HR-AUC","Δ[La]","HR-MAX", "Overall discomfort","RPE","Lower limb discomfort","Difficulty breathing")

data_training_load$x_axis_level=paste0(data_training_load$training_session,"-",data_training_load$condition)

data_training_load$x_axis_level <- factor(data_training_load$x_axis_level, levels = c("SSG-NOR" ,"SSG-IEH","SIT-NOR","SIT-IEH"))

data_training_load$x_axis_level=as.factor(data_training_load$x_axis_level)

data_training_load_perceptual_responses=subset(data_training_load,data_training_load$variable %in% c("Overall discomfort","RPE","Lower limb discomfort","Difficulty breathing"))











graph_perceptual_responsess=ggplot(data_training_load_perceptual_responses, aes(x = x_axis_level, y = emmean, fill = variable)) +
  geom_bar(stat = "identity", 
  position = "dodge", 
  width = 0.5,
  color="black") +
  geom_errorbar(aes(ymin = emmean, ymax = emmean + CI), 
                position = position_dodge(width = 0.5), width = 0.15) +

  scale_fill_grey(start = 0, end = 1) +
  labs(x = "Training - Condition", y = "Perceptual responses (A.U.)", fill = "Variable") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),  
    axis.title = element_text(family = "Times New Roman", size = 12),  
    axis.text = element_text(family = "Times New Roman", size = 12),   
    legend.text = element_text(family = "Times New Roman", size = 12), 
    legend.title = element_text(family = "Times New Roman", size = 14), 
  panel.grid.major.x = element_blank()
  )

data_training_load_hr_lactate=subset(data_training_load,data_training_load$variable %in% c("Δ[La]","HR-MAX"))
data_training_load_hr_lactate <- data_training_load_hr_lactate %>%
  mutate(
    emmean = case_when(variable == "Δ[La]" ~ emmean * 10, TRUE ~ emmean),
    CI = case_when(variable == "Δ[La]" ~ CI * 10, TRUE ~ CI)
  )


graph_hr_lac=ggplot(data_training_load_hr_lactate, aes(x = x_axis_level, y = emmean, fill = variable)) +
   
  geom_bar(stat = "identity", 
  position = "dodge", 
  width = 0.5,
  color="black") +
    scale_fill_grey(start = 0.2, end = 0.8, labels = c( "Δ[La]",expression(HR[MAX]))) +

    geom_errorbar(aes(ymin = emmean, ymax = emmean + CI), 
                position = position_dodge(width = 0.5), width = 0.15) + 

  scale_y_continuous(
    name = expression(HR[MAX]~"(bpm)"),
    breaks = seq(0, max(data_training_load_hr_lactate$emmean), by = 20),  
    sec.axis = sec_axis(
      trans = ~ . /10, 
      name = expression("Δ[La] (" * mmol %.% L^{-1} * ")"),
      breaks = seq(0, max(data_training_load_hr_lactate$emmean) / 10, by = 2)
   ) )+
  theme_minimal()+
    labs(x = "Training - Condition", fill = "Variable") +
  theme(
    text = element_text(family = "Times New Roman", size = 12),  
    axis.title = element_text(family = "Times New Roman", size = 12),
    axis.text = element_text(family = "Times New Roman", size = 12), 
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_blank(), 
  panel.grid.major.x = element_blank(),

  ) 


##### distancia percorrida + distancia maxima 

data_training_quality=read_excel(paste0(root,"/model summary – training quality/emms.xlsx"))

for (i in names(data_training_quality)[1:2]){
    
    data_training_quality[[i]]=as.factor(data_training_quality[[i]])
}

levels(data_training_quality$condition)=c("IEH","NOR")

data_training_quality$condition <- factor(data_training_quality$condition, levels = c("NOR","IEH"))



 
data_training_quality_SIT=subset(data_training_quality,data_training_quality$variable %in% c("total_distance",	"average_power",	"max_power",	"fatigue_index"))     
data_training_quality_SIT$variable=as.factor(data_training_quality_SIT$variable)


#### fig1: distancia percorrida & potencia média & potencia máxima & IF 
#### fig3: IF - IF é media ou min?

individual_data=read_excel(paste0(root,"/data/ieh ssg and sit data.xlsx"))
individual_data=individual_data %>%
    select(id,condition,training_session,total_distance,max_power,average_power,fatigue_index ,total_playerscore,positive_playerscore,negative_playerscore)



for (i in names(individual_data)[1:3]){
    
    individual_data[[i]]=as.factor(individual_data[[i]])
}



levels(individual_data$condition)=c("IEH","NOR")

individual_data$condition <- factor(individual_data$condition, levels = c("NOR","IEH"))

individual_data_sit= individual_data %>%
  filter(training_session == "SIT")

###fatigue_index
graph_fatigue_index=ggplot() +
  

  geom_line(data = individual_data_sit, 
            aes(x = condition, y = fatigue_index, group = id), 
         position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_sit, 
             aes(x = condition, y = fatigue_index,group=id), 
            position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "fatigue_index"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +

geom_line(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "fatigue_index"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +


  geom_errorbar(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "fatigue_index"),
  aes(x=condition, ymin = emmean - CI, ymax = emmean + CI), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Fatigue index (%)", 
       ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),  
    axis.title = element_text(family = "Times New Roman", size = 12), 
    axis.text = element_text(family = "Times New Roman", size = 12),  
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_blank(), 
  panel.grid.major.x = element_blank(),

  )

###total_distance
graph_total_distance=ggplot() +
  

  geom_line(data = individual_data_sit, 
            aes(x = condition, y = total_distance, group = id), 
              position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_sit, 
             aes(x = condition, y = total_distance,group=id), 
            position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "total_distance"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +
              
  geom_line(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "total_distance"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +


  geom_errorbar(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "total_distance"),
  aes(x=condition, ymin = emmean - CI, ymax = emmean + CI), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Total distance  (m)", 
       ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12), 
    axis.title = element_text(family = "Times New Roman", size = 12),  
    axis.text = element_text(family = "Times New Roman", size = 12),   
    legend.text = element_text(family = "Times New Roman", size = 12), 
    legend.title = element_blank(),
  panel.grid.major.x = element_blank(),

  )

###average_power
graph_average_power=ggplot() +
  

  geom_line(data = individual_data_sit, 
            aes(x = condition, y = average_power, group = id), 
            position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_sit, 
             aes(x = condition, y = average_power,group=id), 
            position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "average_power"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +
              
  geom_line(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "average_power"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +


  geom_errorbar(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "average_power"),
  aes(x=condition, ymin = emmean - CI, ymax = emmean + CI), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Average power output (W)", 
       ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12), 
    axis.title = element_text(family = "Times New Roman", size = 12),
    axis.text = element_text(family = "Times New Roman", size = 12), 
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_blank(), 
  panel.grid.major.x = element_blank(),

  )



  ###max_power
graph_max_power=ggplot() +
  

  geom_line(data = individual_data_sit, 
            aes(x = condition, y = max_power, group = id), 
              position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_sit, 
             aes(x = condition, y = max_power,group=id), 
            position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "max_power"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +
              
  geom_line(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "max_power"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +


  geom_errorbar(data = subset(data_training_quality_SIT,data_training_quality_SIT$variable == "max_power"),
  aes(x=condition, ymin = emmean - CI, ymax = emmean + CI), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Maximum power output (W)", 
       ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),  
    axis.title = element_text(family = "Times New Roman", size = 12),  
    axis.text = element_text(family = "Times New Roman", size = 12),   
    legend.text = element_text(family = "Times New Roman", size = 12), 
    legend.title = element_blank(), 
  panel.grid.major.x = element_blank(),

  )






figure1=graph_hr_lac+graph_perceptual_responsess


figure2_2=(graph_total_distance+graph_average_power + graph_max_power + graph_fatigue_index) + plot_layout (ncol=4) 




######

individual_data_ssg= individual_data %>%
  filter(training_session == "SSG")

data_training_quality_playerscore=subset(data_training_quality,data_training_quality$variable %in% c("total_playerscore",	"positive_playerscore",	"negative_playerscore"))     
data_training_quality_playerscore$variable=as.factor(data_training_quality_playerscore$variable)

data_training_quality_playerscore$emmean=data_training_quality_playerscore$emmean-3
data_training_quality_playerscore$CI=data_training_quality_playerscore$CI-3
data_training_quality_playerscore$lower.CL=data_training_quality_playerscore$lower.CL-3
data_training_quality_playerscore$upper.CL=data_training_quality_playerscore$upper.CL-3

graph_total_playerscore=ggplot() +
  

  geom_line(data = individual_data_ssg, 
            aes(x = condition, y = total_playerscore, group = id), 
              position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_ssg, 
             aes(x = condition, y = total_playerscore,group=id), 
              position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_playerscore,data_training_quality_playerscore$variable == "total_playerscore"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +
  
  geom_line(data = subset(data_training_quality_playerscore,data_training_quality_playerscore$variable == "total_playerscore"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +

  geom_errorbar(data = subset(data_training_quality_playerscore,data_training_quality_playerscore$variable == "total_playerscore"),
  aes(x=condition, ymin = lower.CL, ymax = upper.CL), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Total score – PlayerScore (a.u.)", 
       ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),  
    axis.title = element_text(family = "Times New Roman", size = 12), 
    axis.text = element_text(family = "Times New Roman", size = 12),  
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_blank(),
  panel.grid.major.x = element_blank(),

  )



graph_positive_playerscore=ggplot() +
  

  geom_line(data = individual_data_ssg, 
            aes(x = condition, y = positive_playerscore, group = id), 
              position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_ssg, 
             aes(x = condition, y = positive_playerscore,group=id), 
              position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_playerscore,data_training_quality_playerscore$variable == "positive_playerscore"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +
  
  geom_line(data = subset(data_training_quality_playerscore,data_training_quality_playerscore$variable == "positive_playerscore"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +

  geom_errorbar(data = subset(data_training_quality_playerscore,data_training_quality_playerscore$variable == "positive_playerscore"),
  aes(x=condition, ymin = lower.CL, ymax = upper.CL), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Positive score – PlayerScore (a.u.)", 
       ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),  
    axis.title = element_text(family = "Times New Roman", size = 12),  
    axis.text = element_text(family = "Times New Roman", size = 12),   
    legend.text = element_text(family = "Times New Roman", size = 12), 
    legend.title = element_blank(), 
  panel.grid.major.x = element_blank(),

  )





graph_negative_playerscore=ggplot() +
  

  geom_line(data = individual_data_ssg, 
            aes(x = condition, y = negative_playerscore, group = id), 
              position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_ssg, 
             aes(x = condition, y = negative_playerscore,group=id), 
            position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_playerscore,data_training_quality_playerscore$variable == "negative_playerscore"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +
  
  geom_line(data = subset(data_training_quality_playerscore,data_training_quality_playerscore$variable == "negative_playerscore"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +

  geom_errorbar(data = subset(data_training_quality_playerscore,data_training_quality_playerscore$variable == "negative_playerscore"),
  aes(x=condition, ymin = lower.CL, ymax = upper.CL), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Negative score – PlayerScore (a.u.)", 
       ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12), 
    axis.title = element_text(family = "Times New Roman", size = 12), 
    axis.text = element_text(family = "Times New Roman", size = 12), 
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_blank(), 
  panel.grid.major.x = element_blank(),

  )





figure3=(graph_positive_playerscore + graph_negative_playerscore + graph_total_playerscore) + plot_layout (ncol=3)
