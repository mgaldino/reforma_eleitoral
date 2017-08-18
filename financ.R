library(tidyverse)
library(data.table)
library(janitor)

setwd("C:\\Users\\mgaldino\\2017\\Geral TB\\Advocacy\\reforma_eleitoral")


###############
##### Importando tabelas
###############

receitas16 <- fread("2016/despesas_partidos_prestacao_contas_final_2016_brasil.txt")
# receitas14 <- fread("2016/despesas_partidos_prestacao_contas_final_2016_brasil.txt")
# receitas12 <- fread("2016/despesas_partidos_prestacao_contas_final_2016_brasil.txt")

receitas16 <- receitas16 %>%
  clean_names()

# cnpjs

cnpj2015 <- read_fwf(
  file="CNPJ_diretorios_partidarios_2015/CNPJ_diretorios_partidarios_2015.txt",   
  skip=1,
  fwf_widths(c(1, 2, 14, 150), 
             c("registro", "tipo", "cnpj", "nome_partido")))




###

receitas16_amostra %>%
  group_by(tipo_receita) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))



###############################
####2.2) Cálculo tipos de receita em 2012 (pronto!)
###############################

rec12 <- fread("receitas_candidatos_2012_brasil.txt")

##renoeando colunas
rec12_r <- rec12 %>%
  rename(valor_receita = "Valor receita",
         fonte_recurso = "Fonte recurso",
         tipo_receita = "Tipo receita")

##composição doações
rec12_r %>%
  group_by(tipo_receita) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))


###############
####2.3) Cálculo tipos de receita em 2014 (em andamento)
##############

rec14 <- fread("receitas_candidatos_2014_brasil.txt")

##renoeando colunas
rec14_r <- rec14 %>%
  rename(valor_receita = "Valor receita",
         fonte_recurso = "Fonte recurso",
         tipo_receita = "Tipo receita")

##composição doações
rec14_r %>%
  group_by(tipo_receita) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))

###checando doadores originários (filtro: recursos de partido político)

#renomeando coluna
rec14_r <- rec14_r %>%
  rename(cpf_cnpj_originario = "cpf_cnpj_originario")

##pegando CNPJ dos diretórios

cnpj_diretorios <- fread("CNPJ_diretorios_partidarios_2015.txt")
cnpj_diretorios <- read.table("CNPJ_diretorios_partidarios_2015.txt", header = T, sep = ";",
                       quote = "\"", comment.char = "", as.is  = T)

##PROBLEMA NA LEITURA!
##Falta bater o CNPJ dos doadores originários pra refazer os cálculos