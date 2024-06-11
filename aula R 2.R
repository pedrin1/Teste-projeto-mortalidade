library(readxl)
dados <-  read_excel(path="Base_CTG_caracterizacao.xls")
dados
library(tidyverse)
library(janitor)
names(dados)
#para deixar os nomes entendiveis para todos
dados <- clean_names(dados)
names(dados)
#para remover linhas e colunas em branco
dados <- remove_empty(dados, "rows")
dados <- remove_empty(dados, "cols")
#identificacao em casos duplicados
get_dupes(dados, id)
library(dbplyr)
#atribuir os dados distrintos em uma nova avariável
dados1 <-  distinct(dados,id, .keep_all = TRUE)
dados1
#ver a estrutura dos dados
str(dados1)
#para transformar o tipo do dado
dados1$data_aval <- as.Date(dados1$data_aval)
dados1$data_nascimento <- as.Date(dados1$data_nascimento)
str(dados1)
#identificacao de erros
#tabela de frequencia
tabyl(dados1,corion)
#para variaveis de texto
library(stringr)
dados1$corion <- str_to_lower(dados1$corion)
tabyl(dados1$corion)
#para ignorar um dado
tabyl(dados1$cor_branco)
dados1$cor_branco <- ifelse(dados1$cor_branco==3,1,dados1$cor_branco)
tabyl(dados1$cor_branco)
#pacote para  manipular fatores : forcats
library(forcats)
#para fazer analises completa nas variaveis
#summarytools
#gtsummer
#skimr
library(skimr)
skim(dados1)
#transformacao de variaveis quantitativas
dados1$alt <- dados1$alt|> as.numeric()
dados1$peso_pre <- dados1$peso_pre |> as.numeric()
dados1<- dados1|> mutate( imc = peso_pre/alt^2)
skim(dados1,imc)
dados1$alt |> class()

#Exercicio
#crie a variavel igp (idade gestacional do parto) em semanas - obtida ao somar igp_semana e igp_dia
dados1<- dados1|> mutate( igp = igp_semana+igp_dia)
skim(dados1$igp)
dados1
#transformacao de variavel qualitativa
dados1$primigesta <- ifelse(dados1$gesta==1,1,0)
tabyl(dados1,primigesta)
dados1

dados1$primigesta <- ifelse(dados1$gesta==1,"prim.gest","n.prim.gest")
tabyl(dados1,primigesta)
dados1

#Exercicio
#1) Crie a variavel indicador_aborto(sim e nao) - sim se aborto >= 1 e nao se aborto = o e primipara(sim e nao)- sim se para >= 1 e nao se para = 0
dados1<- dados1|> mutate( indicador_aborto = ifelse(dados1$aborto>=1,'sim','nao'),
                          primipara= ifelse(dados1$para>=1,'sim','nao'))
names(dados1)

dados1$indi_aborto <- ifelse(dados1$aborto>=1,'sim','nao')
dados1$primipara <- ifelse(dados1$para>=1,'sim','nao')

dados1$indicador_aborto <- NULL
as.data.frame(dados1)
 #diferenca de datas
library(lubridate)
intervalo <- ymd(dados1$data_nascimento)%--% ymd(dados1$data_aval)
intervalo
dados1$idade <- intervalo/dyears(1)
idade
dados1<- dados1|> mutate( idade = (data_aval-data_nascimento)/365)
dados1$idade
#combinacao de bancos de dados
dados.ctg

#tratamento
names(Base_CTG)
dados_ctg <- Base_CTG
# 1 para remover linhas e colunas em branco
dados_ctg <- remove_empty(dados_ctg, "rows")
dados_ctg <- remove_empty(dados_ctg, "cols")
# identificacao em casos duplicados
get_dupes(dados_ctg, ID)
dados_ctg <- clean_names(dados_ctg)
dados_ctg
dados_ctg<-  distinct(dados_ctg,id, .keep_all = TRUE)
as.data.frame(dados_ctg)

dados.todos <- inner_join(dados1,dados_ctg,by=c("id"))
dados.todos
as.data.frame(dados.todos)

#duft
# escala de cores viridis

trees
hist()

tress|> esquisse::esquisse()
