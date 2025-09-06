library(readxl)
library(writexl)
library(dplyr)

rm(list=ls())
root="/Users/germano/Library/CloudStorage/OneDrive-Pessoal/XUSP/GECIFEX/Producoes/Artigos/artigo 1 mestrado - agudo sessao/data analysis – ieh – ssg and sit"


data=read_excel(paste0(root,"/ieh ssg and sit data.xlsx"))

selected_data=data %>%
    filter(training_order == 1 & id != 9) %>%
    select(heigth,	weight,	age,	lean_mass,	fat_percentage)


mean <- summarise_all(selected_data, mean, na.rm = TRUE)
sd <- summarise_all(selected_data, sd, na.rm = TRUE)


mean_sd <- Map(function(m, s) sprintf("%.2f ± %.2f", m, s), mean, sd)


final_table <- as.data.frame(mean_sd)

write_xlsx(final_table, path=paste0(root,"/participants characteristics.xlsx"))

