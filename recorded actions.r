library(readxl)
library (tidyr)
library(dplyr)
library(writexl)

rm(list=ls())

root="/Users/germano/Library/CloudStorage/OneDrive-Pessoal/XUSP/GECIFEX/Producoes/Artigos/artigo 1 mestrado - agudo sessao/data analysis – ieh – ssg and sit"

actions_recorded=read_excel(paste0(root,"/data/acoes contabilizadas.xlsx"))
player_score_points = read_excel( paste0(root,"/data/referencia pontuacao acoes.xlsx"))

actions_recorded=actions_recorded[,-1]

for (variable in names (actions_recorded)) {

actions_recorded[[variable]]=as.factor (actions_recorded[[variable]])
}

for (variable in c("acoes","situacao")) {

player_score_points[[variable]]=as.factor (player_score_points[[variable]])
}

actions_recorded$Jogo=factor(actions_recorded$Jogo,levels=c( "Jogo 1",   "Jogo 2",  "Jogo 3",  "Jogo 4",  "Jogo 5" , "Jogo 6"  ,"Jogo 7" , "Jogo 8" ,"Jogo 9" ,"Jogo 10"))
actions_recorded$day=ifelse(actions_recorded$Jogo %in% c("Jogo 1",   "Jogo 2",  "Jogo 3",  "Jogo 4",  "Jogo 5"),1,2)
actions_recorded$day=factor(actions_recorded$day)


team=data.frame(team= factor(c("B","B","A","A","B","B","A","A")),Jogador=factor(c("Jogador 1","Jogador 2","Jogador 3","Jogador 4","Jogador 5","Jogador 6","Jogador 7","Jogador 8")))
condition=as.data.frame(matrix(c(
1,	3,
1,	4,
1,	4,
1,	3,
1,	3,
1,	4,
1,	4,
1,	3,
2,	4,
2,	3,
2,	3,
2,	4,
2,	4,
2,	3,
2,	3,
2,	4), ncol=2, byrow=TRUE))
names(condition)=c("condition","day")
condition$day=condition$day-2
condition$Jogador=team$Jogador

condition$day=as.factor(condition$day)
condition$condition=as.factor(condition$condition)

levels(condition$condition) = c("IEH","NOR")

actions_recorded <- actions_recorded %>%
  left_join(team, by = "Jogador")


actions_recorded <- actions_recorded %>%
  left_join(condition, by = c("Jogador","day"))




action_number=as.data.frame(table(actions_recorded$Acao))
names(action_number)=c("acoes","frequencia")
action_number_condition=as.data.frame(table(actions_recorded$Acao,actions_recorded$condition))
names(action_number_condition)=c("acoes","condition","frequencia")



action_number_day=as.data.frame(table(actions_recorded$Acao,actions_recorded$day))
names(action_number_day)=c("acoes","day","frequencia")
action_number_day_condition=as.data.frame(table(actions_recorded$Acao,actions_recorded$day,actions_recorded$condition))
names(action_number_day_condition)=c("acoes","day","condition","frequencia")


action_number_jogador=as.data.frame(table(actions_recorded$Acao,actions_recorded$Jogador))
names(action_number_jogador)=c("acoes","jogador","frequencia")
action_number_jogador_day=as.data.frame(table(actions_recorded$Acao,actions_recorded$day,actions_recorded$Jogador))
names(action_number_jogador_day)=c("acoes","day","jogador","frequencia")


action_number_day_equipe=as.data.frame(table(actions_recorded$Acao,actions_recorded$day,actions_recorded$team))
names(action_number_day_equipe)=c("acoes","day","team","frequencia")

action_number=action_number[action_number$frequencia != 0,]
action_number_day=action_number_day[action_number_day$frequencia != 0,]
action_number_jogador=action_number_jogador[action_number_jogador$frequencia != 0,]
action_number_jogador_day=action_number_jogador_day[action_number_jogador_day$frequencia != 0,]
action_number_day_equipe=action_number_day_equipe[action_number_day_equipe$frequencia != 0,]
action_number_day_condition=action_number_day_condition[action_number_day_condition$frequencia != 0,]

scored_goals=action_number_day_equipe[action_number_day_equipe$acoes == "01-gol",]



action_number$condition="Total"

action_number=rbind(action_number,action_number_condition)

action_number$condition=as.factor(action_number$condition)
action_number$condition=factor(action_number$condition,levels=c("NOR","IEH","Total"))

action_number = action_number %>%
    arrange(acoes, condition)

action_number_wide= action_number %>%
    pivot_wider(
        names_from=acoes,
        values_from=frequencia
    )

action_number_table=list(action_number=action_number,
                        action_number_wide=action_number_wide)

    write_xlsx(action_number_table,path=paste0(root,"/actions recorded.xlsx"))