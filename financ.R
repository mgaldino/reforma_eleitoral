library(tidyverse)
library(data.table)
library(janitor)

setwd("C:/Users/mgaldino/2017/Geral TB/Advocacy/reforma_eleitoral")

## carregando bases de dados
# 12

# 16
receitas16 <- fread("2016/receitas_candidatos_prestacao_contas_final_2016_brasil.txt")
receitas16 <- receitas16 %>%
  clean_names()

# 14
rec14 <- fread("2014/receitas_candidatos_2014_brasil.txt")

##renoeando colunas
rec14 <- rec14 %>%
  clean_names()


# cnpj
cnpj2015 <- read_fwf(
  file="CNPJ_diretorios_partidarios_2015/CNPJ_diretorios_partidarios_2015.txt",   
  skip=1,
  fwf_widths(c(1, 2, 14, 150), 
                 c("registro", "tipo", "cnpj", "nome_partido")))


###############
#####1) C�lculo tipos de receita em 2016 (amostra - pronto)
###############

rec16_r <- receitas16 %>%
  select(sequencial_candidato, numero_recibo_eleitoral, cpf_do_candidato, nome_do_doador_originario, nome_do_doador,
         valor_receita, tipo_receita, fonte_recurso, cpf_cnpj_do_doador,
         cpf_cnpj_do_doador_originario) %>%
  mutate(cnpj_final = ifelse(cpf_cnpj_do_doador_originario == "#NULO",
                             cpf_cnpj_do_doador, cpf_cnpj_do_doador_originario)) %>%
  left_join(cnpj2015, 
            by=c('cnpj_final'='cnpj')) %>%
  mutate(bol_partido = as.numeric(!is.na(tipo)))


rec16_r <- rec16_r %>%
  mutate(bol_juridica = ifelse(nchar(cnpj_final) <= 11, "PF", "PJ"),
         comite = ifelse(grepl("comit", tolower(nome_do_doador_originario)), "comit�", "outro"),
         comite2 = ifelse(grepl("comit", tolower(nome_do_doador)) & bol_juridica == "PJ" & cpf_cnpj_do_doador_originario == "#NULO", "comit�", "outro"),
         tipo_recurso_tb = case_when( bol_juridica == "PJ" & bol_partido == 0 ~ "partido",
                                      bol_juridica == "PJ" & bol_partido == 1 ~ "partido",
                                      bol_juridica == "PF" & tipo_receita == "Recursos pr�prios" ~ "recursos pr�prios",
                                      bol_juridica == "PF" & tipo_receita != "Recursos pr�prios" ~ "PF outros"))


resultado16 <- rec16_r%>%
  group_by(tipo_recurso_tb) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))

resultado16

###############################
####2) C�lculo tipos de receita em 2012 (pronto)
###############################

rec12 <- fread("2012/receitas_candidatos_2012_brasil.txt")

##renomeando colunas
rec12 <- rec12 %>%
  clean_names()

##composi��o doa��es

rec12_r <- rec12 %>%
  select(sequencial_candidato, numero_recibo_eleitoral, cpf_do_candidato, nome_do_doador,
         valor_receita, tipo_receita, fonte_recurso, cpf_cnpj_do_doador, setor_economico_do_doador,
         nome_do_doador_receita_federal) %>%
  mutate(bol_juridica = ifelse(nchar(cpf_cnpj_do_doador) <= 11, "PF", "PJ")) %>%
  left_join(cnpj2015, 
            by=c('cpf_cnpj_do_doador'='cnpj')) %>%
  mutate(bol_partido = as.numeric(!is.na(tipo)),
         comite = ifelse(grepl("comit", tolower(nome_do_doador_receita_federal)), "comite", "outro"),
         comite2 = ifelse(grepl("comit", tolower(nome_do_doador_receita_federal)) & bol_juridica == "PJ", "comite", "outro"),
         tipo_recurso_tb = case_when( tipo_receita == "Recursos pr�prios" ~ "recursos pr�prios",
                                      bol_juridica == "PJ" & bol_partido == 1 ~ "partido",
                                      comite == "comite" & comite2 == "outro" ~ "partido",
                                      comite == "outro" & comite2 == "comite" ~ "partido",
                                      comite == "comite" & comite2 == "comite" ~ "partido",
                                      bol_juridica == "PF" & bol_partido == 0 & comite2 == "outro" & comite == "outro" ~ 'PF outros',
                                      tipo_receita == 'Recursos de outros candidatos/comit�s' & bol_juridica == "PJ" ~ 'partido',
                                      tipo_receita != "Recursos de outros candidatos/comit�s" & bol_juridica == "PJ" & bol_partido == 0 & comite2 == "outro" & comite == "outro" ~ 'empresas'))


##selecionando rec12 e combinando cnpj2015
resultado12 <- rec12_r%>%
  group_by(tipo_recurso_tb) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))

resultado12


###n�o esquecer de projetar outros cen�rios deslocando os
#fonte_recurso == "Outros recursos n�o descritos" dos tipo_recurso_tb == "partido" para empresa.


###############
####3) C�lculo tipos de receita em 2014 (pronto)
##############


##composi��o doa��es
rec14 %>%
  group_by(tipo_receita) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))

##pegando CNPJ dos diret�rios

rec14_r <- rec14 %>%
  select(sequencial_candidato, numero_recibo_eleitoral, cpf_do_candidato, nome_do_doador_originario, nome_do_doador,
         valor_receita, tipo_receita, fonte_recurso, cpf_cnpj_do_doador,
         cpf_cnpj_do_doador_originario) %>%
  mutate(cnpj_final = ifelse(cpf_cnpj_do_doador_originario == "#NULO",
                             cpf_cnpj_do_doador, cpf_cnpj_do_doador_originario)) %>%
  left_join(cnpj2015, 
             by=c('cnpj_final'='cnpj')) %>%
  mutate(bol_partido = as.numeric(!is.na(tipo)))

rec14_r <- rec14_r %>%
  mutate(bol_juridica = ifelse(nchar(cnpj_final) <= 11, "PF", "PJ"),
         comite = ifelse(grepl("comit", tolower(nome_do_doador_originario)), "comit�", "outro"),
         comite2 = ifelse(grepl("comit", tolower(nome_do_doador)) & bol_juridica == "PJ" & cpf_cnpj_do_doador_originario == "#NULO", "comit�", "outro"),
         tipo_recurso_tb = case_when( bol_juridica == "PJ" & bol_partido == 0  & grepl("outro", comite) & grepl("outro", comite2) ~ "empresas", # & grepl("outro", comite) & grepl("outro", comite2) 
                                      tipo_receita != "Recursos pr�prios" & bol_juridica == "PF" & grepl("outro", comite) & grepl("outro", comite2) ~ "PF outros",
                                      tipo_receita == "Recursos pr�prios" & bol_juridica == "PF" ~ "Recursos Pr�prios",
                                      bol_juridica == "PJ" & bol_partido == 1 ~ "partidos",
                                      bol_juridica == "PJ" & bol_partido == 0 & (grepl("outro", comite) | grepl("outro", comite2)) ~"partidos",
                                      T ~ "PF outros")) 

resultado <- rec14_r%>%
  group_by(tipo_receita, bol_partido, bol_juridica, comite, comite2, tipo_recurso_tb) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))

resultado14 <- rec14_r%>%
  group_by(tipo_recurso_tb) %>%
  summarise(contagem = n(),
            gasto = sum(as.numeric(gsub(",", "\\.", valor_receita)))) %>%
  ungroup() %>%
  mutate(total_gasto = sum(gasto),
         perc_gasto = round(gasto/total_gasto, 2))

resultado14
