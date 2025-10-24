library(ggplot2)
library(readxl)
library(extrafont)
library(dplyr)
library(gridExtra)
library(patchwork)

rm(list=ls())
#Change here for actual root directory
root=""
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

data_training_load$x_axis_level <- factor(data_training_load$x_axis_level, levels = c("SSG-NOR" ,"SSG-IEH","SIE-NOR","SIE-IEH"))

data_training_load$x_axis_level=as.factor(data_training_load$x_axis_level)

data_training_load_perceptual_responses=subset(data_training_load,data_training_load$variable %in% c("Overall discomfort","RPE","Lower limb discomfort","Difficulty breathing"))



 standard_theme = theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),  
    axis.title.x = element_text(family = "Arial", size = 12),
    axis.title.y = element_text(
      family = "Arial", size = 12,
      margin = margin(t = 0, r = 20, b = 0, l = 0)  
    ),    axis.text = element_text(family = "Arial", size = 11),   
    legend.text = element_text(family = "Arial", size = 12),
    legend.title = element_blank(), 
  panel.grid.major.x = element_blank(),
  )





sig_perceptual_SIE_IEH <- data.frame(
  group = c("SIE-IEH", "SIE-IEH", "SIE-IEH", "SIE-IEH"),
  variable = c("Overall discomfort", "RPE", "Lower limb discomfort","Difficulty breathing"),
  y = c(10, 10, 10,10), 
  label = c("#","", "#", "#")
)

sig_perceptual_SSG_IEH <- data.frame(
  group = c("SSG-IEH", "SSG-IEH", "SSG-IEH", "SSG-IEH"),
  variable = c("Overall discomfort", "RPE", "Lower limb discomfort","Difficulty breathing"),
  y = c(9.8, 10, 10,9.8), 
  label = c("","", "", "*")
)

graph_perceptual_responsess=ggplot(data_training_load_perceptual_responses, aes(x = x_axis_level, y = emmean, fill = variable)) +
  geom_bar(stat = "identity", 
  position = "dodge", 
  width = 0.5,
  color="black") +
  geom_errorbar(aes(ymin = emmean, ymax = emmean + CI), 
                position = position_dodge(width = 0.5), width = 0.15) + 
  geom_text(data = sig_perceptual_SIE_IEH, aes(x = group, y = y, label = label, group = variable),
      position = position_dodge(width = 0.5),size=3) +
  geom_text(data = sig_perceptual_SSG_IEH, aes(x = group, y = y, label = label, group = variable),
    position = position_dodge(width = 0.5),size=7) +
  scale_fill_grey(start = 0, end = 1, 
                  labels = c(expression("Overall discomfort"^" a,b"),
                  expression("RPE"^" a"),
                  expression("Lower limb discomfort"^" a"),
                  expression("Difficulty breathing"^" b")
                  )
                  )+
  labs(x = "Training - Condition", y = "Perceptual responses (A.U.)", fill = NULL) 


data_training_load_hr_lactate=subset(data_training_load,data_training_load$variable %in% c("Δ[La]","HR-MAX"))
data_training_load_hr_lactate <- data_training_load_hr_lactate %>%
  mutate(
    emmean = case_when(variable == "Δ[La]" ~ emmean * 10, TRUE ~ emmean),
    CI = case_when(variable == "Δ[La]" ~ CI * 10, TRUE ~ CI)
  )




sig_lactate_hr_SIE_IEH <- data.frame(
  group = c("SIE-IEH", "SIE-IEH"),
  variable = c("Δ[La]", "HR-MAX"),
  y = c(190, 190), 
  label = c("#","#")
)
sig_lactate_hr_perceptual_SIE_NOR <- data.frame(
  group = c("SIE-NOR", "SIE-NOR"),
  variable = c("Δ[La]", "HR-MAX"),
  y = c(190, 190), 
  label = c("#","#")
)



graph_hr_lac=ggplot(data_training_load_hr_lactate, aes(x = x_axis_level, y = emmean, fill = variable)) +
   
  geom_bar(stat = "identity", 
  position = "dodge", 
  width = 0.5,
  color="black") +
    scale_fill_grey(start = 0.2, end = 0.8, labels = c( expression("Δ[La]"^ a),expression(HR[MAX]*""^a))) +

    geom_errorbar(aes(ymin = emmean, ymax = emmean + CI), 
                position = position_dodge(width = 0.5), width = 0.15) + 
  geom_text(data = sig_lactate_hr_SIE_IEH, aes(x = group, y = y, label = label, group = variable),
      position = position_dodge(width = 0.5),size=3) +
  geom_text(data = sig_lactate_hr_perceptual_SIE_NOR, aes(x = group, y = y, label = label, group = variable),
    position = position_dodge(width = 0.5),size=3) +

  scale_y_continuous(
    name = expression(HR[MAX]~"(bpm)"),
    breaks = seq(0, max(data_training_load_hr_lactate$emmean), by = 20),  
    sec.axis = sec_axis(
      trans = ~ . /10, 
      name = expression("Δ[La] (" * mmol %.% L^{-1} * ")"),
      breaks = seq(0, max(data_training_load_hr_lactate$emmean) / 10, by = 2)
   ) )+

    labs(x = "Training - Condition", fill = "Variable") 

##theme
  graph_list <- list(graph_hr_lac, graph_perceptual_responsess)
 graph_list <- lapply(graph_list, function(g) g + standard_theme)


##adding tags
 make_tag_plot <- function(tag) {
  ggplot() +
    annotate("label", x = 0.3, y = 0.5, label = tag,
             size = 4.5, fontface = "bold",
             family = "Arial",
             color = "black", fill = "white", 
             label.r = unit(0, "lines"))+ # sharp border
    theme_void()
 }




# Creating tags as miniplots
 tagA <- make_tag_plot("A")
 tagB <- make_tag_plot("B")

# Adding tags to main plots
 s1 <- graph_list[[1]] + inset_element(tagA, left = 0.01, bottom = 0.9, right = 0.09, top = 0.99,align_to="full")
 s2 <- graph_list[[2]] + inset_element(tagB, left = 0.01, bottom = 0.9, right = 0.09, top = 0.99,align_to="full")
 

# Combining plots
figure1 <- (s1 | s2 ) 


##### training quality

data_training_quality=read_excel(paste0(root,"/model summary – training quality/emms.xlsx"))

for (i in names(data_training_quality)[1:2]){
    
    data_training_quality[[i]]=as.factor(data_training_quality[[i]])
}

levels(data_training_quality$condition)=c("IEH","NOR")

data_training_quality$condition <- factor(data_training_quality$condition, levels = c("NOR","IEH"))



 
data_training_quality_SIE=subset(data_training_quality,data_training_quality$variable %in% c("total_distance",	"average_power",	"max_power",	"fatigue_index"))     
data_training_quality_SIE$variable=as.factor(data_training_quality_SIE$variable)




individual_data=read_excel(paste0(root,"/data/ieh ssg and sie data.xlsx"))


individual_data=individual_data %>%
    select(id,condition,training_session,total_distance,max_power,average_power,fatigue_index ,total_playerscore,positive_playerscore,negative_playerscore)



for (i in names(individual_data)[1:3]){
    
    individual_data[[i]]=as.factor(individual_data[[i]])
}



levels(individual_data$condition)=c("IEH","NOR")
levels(individual_data$training_session) = c("SSG","SIE")

individual_data$condition <- factor(individual_data$condition, levels = c("NOR","IEH"))
individual_data= individual_data %>%
  filter (id != "9")


individual_data_SIE= individual_data %>%
  filter(training_session == "SIE")


###fatigue_index
graph_fatigue_index=ggplot() +
  

  geom_line(data = individual_data_SIE, 
            aes(x = condition, y = fatigue_index, group = id), 
         position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_SIE, 
             aes(x = condition, y = fatigue_index,group=id), 
            position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "fatigue_index"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +

  geom_line(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "fatigue_index"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +


  geom_errorbar(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "fatigue_index"),
  aes(x=condition, ymin = lower.CL, ymax = upper.CL), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Fatigue index (%)", 
       ) 

###total_distance
graph_total_distance=ggplot() +
  

  geom_line(data = individual_data_SIE, 
            aes(x = condition, y = total_distance, group = id), 
              position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_SIE, 
             aes(x = condition, y = total_distance,group=id), 
            position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "total_distance"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +
              
  geom_line(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "total_distance"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +


  geom_errorbar(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "total_distance"),
  aes(x=condition, ymin = lower.CL, ymax = upper.CL), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Total distance  (m)", 
       )
###average_power
graph_average_power=ggplot() +
  

  geom_line(data = individual_data_SIE, 
            aes(x = condition, y = average_power, group = id), 
            position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_SIE, 
             aes(x = condition, y = average_power,group=id), 
            position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "average_power"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +
              
  geom_line(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "average_power"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +


  geom_errorbar(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "average_power"),
  aes(x=condition, ymin = lower.CL, ymax = upper.CL), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Average power output (W)", 
       ) 


###max_power
graph_max_power=ggplot() +
  

  geom_line(data = individual_data_SIE, 
            aes(x = condition, y = max_power, group = id), 
              position=pd,
            alpha = 0.3, size = 0.8) + 
  geom_point(data = individual_data_SIE, 
             aes(x = condition, y = max_power,group=id), 
            position=pd,
             alpha = 0.3, size = 2) + 

  
  geom_point(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "max_power"), 
            aes(x = condition, y = emmean), 
            size = 5,
            shape=17) +
              
  geom_line(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "max_power"), 
            aes(x = condition, y = emmean, group=variable), 
            size = 1
            ) +


  geom_errorbar(data = subset(data_training_quality_SIE,data_training_quality_SIE$variable == "max_power"),
  aes(x=condition, ymin = lower.CL, ymax = upper.CL), 
         width = 0.15) +


  
  labs(x = "Condition", y = "Maximum power output (W)", 
       ) 


##differences
    graph_max_power=graph_max_power +
  geom_text(aes(x = "IEH", y = 65, label = "*"), size = 10)

# adjusting y axis limits
    graph_total_distance = graph_total_distance +
        scale_y_continuous(limits = c(620, 720), breaks = seq(600, 725, 20))
  
    graph_average_power=  graph_average_power +
        scale_y_continuous(limits = c(46, 63), breaks = seq(46, 63, 4))

    graph_max_power=  graph_max_power +
        scale_y_continuous(limits = c(49, 69), breaks = seq(49, 69, 4)) 

    graph_fatigue_index=  graph_fatigue_index +
        scale_y_continuous(limits = c(2, 18), breaks = seq(2, 18, 4)) 



## adjusting theme 
 standard_theme = theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),  
    axis.title.x = element_text(family = "Arial", size = 12),
    axis.title.y = element_text(
      family = "Arial", size = 12,
      margin = margin(t = 0, r = 10, b = 0, l = 0)  
    ),    axis.text = element_text(family = "Arial", size = 11),   
    legend.text = element_text(family = "Arial", size = 12),
    legend.title = element_blank(), 
  panel.grid.major.x = element_blank(),
  )

 graph_list <- list(graph_total_distance, graph_average_power, graph_max_power, graph_fatigue_index)

 graph_list <- lapply(graph_list, function(g) g + standard_theme)

##adding tags
 make_tag_plot <- function(tag) {
  ggplot() +
    annotate("label", x = 0.3, y = 0.5, label = tag,
             size = 4.5, fontface = "bold",
             family = "Arial",
             color = "black", fill = "white", 
             label.r = unit(0, "lines"))+ # sharp border
    theme_void()
 }
# Creating tags as miniplots
 tagA <- make_tag_plot("A")
 tagB <- make_tag_plot("B")
 tagC <- make_tag_plot("C")
 tagD <- make_tag_plot("D")

# Adding tags to main plots
 p1 <- graph_list[[1]] + inset_element(tagA, left = 0.01, bottom = 0.9, right = 0.09, top = 0.99,align_to="full")
 p2 <- graph_list[[2]] + inset_element(tagB, left = 0.01, bottom = 0.9, right = 0.09, top = 0.99,align_to="full")
 p3 <- graph_list[[3]] + inset_element(tagC, left = 0.01, bottom = 0.9, right = 0.09, top = 0.99,align_to="full")
 p4 <- graph_list[[4]] + inset_element(tagD, left = 0.01, bottom = 0.9, right = 0.09, top = 0.99,align_to="full")

# Combining plots
figure2_2 <- (p1 | p2 | p3 | p4) 




######

individual_data_ssg= individual_data %>%
  filter(training_session == "SSG")

data_training_quality_playerscore=subset(data_training_quality,data_training_quality$variable %in% c("total_playerscore",	"positive_playerscore",	"negative_playerscore"))     
data_training_quality_playerscore$variable=as.factor(data_training_quality_playerscore$variable)

data_training_quality_playerscore$emmean=data_training_quality_playerscore$emmean-3

data_training_quality_playerscore$lower.CL=data_training_quality_playerscore$lower.CL-3
data_training_quality_playerscore$upper.CL=data_training_quality_playerscore$upper.CL-3

#total player score
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
       ) 






#


##differences
    graph_positive_playerscore=graph_positive_playerscore +
  geom_text(aes(x = "IEH", y = 3.75, label = "*"), size = 10)

# adjusting y axis limits
    graph_positive_playerscore = graph_positive_playerscore +
        scale_y_continuous(limits = c(1.5, 4.5), breaks = seq(1.5, 4.5, 0.5))
  
    graph_negative_playerscore=  graph_negative_playerscore +
        scale_y_continuous(limits = c(-2.5, -0.5), breaks = seq(-2.5, -0.5, 0.5))

    graph_total_playerscore=  graph_total_playerscore +
        scale_y_continuous(limits = c(-1, 4), breaks = seq(-1, 4, 1)) 




## adjusting theme 
 standard_theme = theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),  
    axis.title.x = element_text(family = "Arial", size = 12),
    axis.title.y = element_text(
      family = "Arial", size = 12,
      margin = margin(t = 0, r = 10, b = 0, l = 0)  
    ),    axis.text = element_text(family = "Arial", size = 11),   
    legend.text = element_text(family = "Arial", size = 12),
    legend.title = element_blank(), 
  panel.grid.major.x = element_blank(),
  )

 graph_list <- list(graph_positive_playerscore, graph_negative_playerscore, graph_total_playerscore)

 graph_list <- lapply(graph_list, function(g) g + standard_theme)

##adding tags
 make_tag_plot <- function(tag) {
  ggplot() +
    annotate("label", x = 0.3, y = 0.5, label = tag,
             size = 4.5, fontface = "bold",
             family = "Arial",
             color = "black", fill = "white", 
             label.r = unit(0, "lines"))+ # sharp border
    theme_void()
 }
# Creating tags as miniplots
 tagA <- make_tag_plot("A")
 tagB <- make_tag_plot("B")
 tagC <- make_tag_plot("C")


# Adding tags to main plots
 q1 <- graph_list[[1]] + inset_element(tagA, left = 0.01, bottom = 0.9, right = 0.09, top = 0.99,align_to="full")
 q2 <- graph_list[[2]] + inset_element(tagB, left = 0.01, bottom = 0.9, right = 0.09, top = 0.99,align_to="full")
 q3 <- graph_list[[3]] + inset_element(tagC, left = 0.01, bottom = 0.9, right = 0.09, top = 0.99,align_to="full")

# Combining plots
figure3 <- (q1 | q2 | q3) 


#saving
figures_directory=paste0(root,"/figures")

if(!dir.exists(figures_directory)) {
  dir.create(figures_directory)
}

ggsave(paste0(figures_directory,"/figure1.png"),plot=figure1, width = 12, height = 3.5,dpi=300)

ggsave(paste0(figures_directory,"/figure2.png"),plot=figure2_2, width = 16, height = 3.5,dpi=300)

ggsave(paste0(figures_directory,"/figure3.png"),plot=figure3, width = 12, height = 4,dpi=300)