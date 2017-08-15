library(tidyverse)
library(janitor)
# alterando pasta
setwd("C:\\Users\\mgaldino\\2017\\Geral TB\\Advocacy\\reforma_eleitoral\\prestacao_final_2014")

receitas <- read.table("receitas_candidatos_2014_brasil.txt", sep=";", header=T, as.is=T, colClasses = "character")
receitas <- receitas %>%
  clean_names()

partidos <- read.table("receitas_partidos_2014_brasil.txt", sep=";", header=T, as.is=T, colClasses = "character")
partidos <- partidos %>%
  clean_names() %>%
  mutate(valor_receita = as.numeric(gsub(",", "\\.", valor_receita)))

comite <- read.table("receitas_comites_2014_brasil.txt", sep=";", header=T, as.is=T, colClasses = "character")
comite <- comite %>%
  clean_names() %>%
  mutate(valor_receita = as.numeric(gsub(",", "\\.", valor_receita)))

  
  
receitas <- receitas %>%
  mutate(valor_receita = as.numeric(gsub(",", "\\.", valor_receita)),
         cnpj_doadores = ifelse(cpf_cnpj_do_doador_originario == "#NULO",
                                cpf_cnpj_do_doador, cpf_cnpj_do_doador_originario))

sum(receitas$valor_receita) ## 4391576939
sum(partidos$valor_receita) ## 1961678179
sum(comite$valor_receita) ## 738362751

despesas_partidos_2014_brasil

library(readr)

x <- read_fwf(
  file="CNPJ_diretorios_partidarios_2015.txt",   
  skip=1,
  fwf_widths(c(1, 2, 14, 150), 
             c("registro", "tipo", "cnpj", "nome_partido")))


## precisa ver a questão dos candidatos indeferidos e, dependendo de como fizer isso,
## evitar duplicadas de candidatos que foram para segundo turno
receitas %>%
  group_by(numero_partido_doador) %>%
  summarise(total = sum(valor_receita)) %>%
  ungroup() %>%
  mutate(bol_partido_doador = ifelse(numero_partido_doador == "#NULO", 0, 1)) %>%
  group_by(bol_partido_doador) %>%
  summarise(total = sum(total))

comite %>%
  group_by(fonte_recurso) %>%
  summarise(total = sum(valor_receita))

receitas %>%
  group_by(tipo_receita) %>%
  summarise(total = sum(valor_receita))

y <- receitas %>%
  group_by(fonte_recurso, tipo_receita) %>%
  summarise(num_linhas = n(), total = sum(valor_receita))
View(y)
x <- receitas %>%
  filter(fonte_recurso == "Fundo Partidario" & tipo_receita == "Recursos de pessoas jurídicas")
x

Valor.receita
unique(receitas$Fonte.recurso)
unique(receitas$Tipo.receita)


## comparando recursos que vieram de origem pública, privada PF e PJ em 14 e 16


## números de candidatos por cadeira em 2014 médio e por distrito

## gasto de campanha por partido que lançou por executivo e não lançou

