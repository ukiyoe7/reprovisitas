## NOVA ESTRUTURA DE CARTEIRAS

library(DBI)
library(tidyverse)
library(xlsx)
library(readr)
library(googlesheets4)
library(lubridate)
library(reshape2)
con2 <- dbConnect(odbc::odbc(), "reproreplica")

## CLIENTES GERAL =========================================================================


cli <- dbGetQuery(con2, statement = read_file('NOVAS_CARTEIRAS_JUN23\\CLIENTS.sql'))


inativos <- dbGetQuery(con2, statement = read_file('NOVAS_CARTEIRAS_JUN23\\INATIVOS.sql'))




clien <- anti_join(cli,inativos,by="CLICODIGO") 

View(clien)


clien %>% filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>% View()

check_cli <-
anti_join(
clien %>% filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>% 
  filter(CIDNOME=='FLORIANOPOLIS'), setor1_rev , by="CLICODIGO") 


View(check_cli)

write.csv2(check_cli,file = "check_cli.csv")


## SETOR 1 ================================================================================================


SETOR1_REV <- save(SETOR1_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR1_REV.RData")

setor1_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR1_REV.RData"))

View(setor1_rev)

setor1_rev %>% distinct(SITUAÇÃO)


setor1_rev_inativos <-
  setor1_rev %>% rename(STATUS=SITUAÇÃO) %>% filter(STATUS=='SEM VENDA'| STATUS=='INATIVO' | STATUS=='inativo') 

View(setor1_rev_inativos)

## ativos

setor1_rev_ativos <-
  setor1_rev %>% rename(STATUS=SITUAÇÃO) %>% filter(is.na(STATUS) | STATUS=='FRITZEN') 

View(setor1_rev_ativos)



## SETOR 2 ================================================================================================

SETOR2_REV <- save(SETOR2_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR2_REV.RData")

setor2_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR2_REV.RData"))

View(setor2_rev)

setor2_rev %>% distinct(STATUS)

## inativos

setor2_rev_inativos <-
setor2_rev %>% filter(STATUS=='SEM VENDA'| STATUS=='INATIVO') 

View(setor2_rev_inativos)

## ativos

setor2_rev_ativos <-
  setor2_rev %>% filter(is.na(STATUS) | STATUS=='ESPELHO' | STATUS=='DIEGO') 

View(setor2_rev_ativos)


## SETOR 3 ================================================================================================


SETOR3_REV <- save(SETOR3_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR3_REV.RData")

setor3_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR3_REV.RData"))

setor3_rev %>% distinct(STATUS)


## revisar inativos

setor3_rev_inativos <-
  setor3_rev %>% filter(STATUS=="FECHADO" | STATUS=="SEM VENDA") 

View(setor3_rev_inativos)

## ativos

setor3_rev_ativos <-
  setor3_rev %>% filter(STATUS=='OK' | STATUS=='ESPELHO') 

View(setor3_rev_ativos)


## SETOR 4 ================================================================================================


SETOR4_REV <- save(SETOR4_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR4_REV.RData")

setor4_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR4_REV.RData"))

View(setor4_rev)

setor4_rev %>% distinct(Coluna1)

## revisar inativos

setor4_rev_inativos <-
  setor4_rev %>% rename(STATUS=Coluna1) %>% filter(STATUS=="FECHOU" | STATUS=="fechou" | STATUS=="SEM VENDAS" ) 

View(setor4_rev_inativos)


## ativos

setor4_rev_ativos <-
  setor4_rev %>% rename(STATUS=Coluna1) %>%  filter(STATUS=='ok' | STATUS=='OK' | STATUS=='ESPELHO') 

View(setor4_rev_ativos)



## SETOR 5 ================================================================================================


SETOR5_REV <- save(SETOR5_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR5_REV.RData")

setor5_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR5_REV.RData"))


View(setor5_rev)

setor5_rev %>% distinct(STATUS)

## revisar inativos

setor5_rev_inativos <-
  setor5_rev %>% filter(STATUS=="INATIVO" | STATUS=="fechou" | STATUS=="Trocou CNPJ" | STATUS=="Sem Venda" | STATUS=="SEM VENDA") 

View(setor5_rev_inativos)

## ativos

setor5_rev_ativos <-
  setor5_rev  %>%  filter(is.na(STATUS) | STATUS=='ESPELHO') 

View(setor5_rev_ativos)


## SETOR 6 ================================================================================================


SETOR6_REV <- save(SETOR6_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR6_REV.RData")

setor6_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR6_REV.RData"))

View(setor6_rev)

setor6_rev %>% distinct(STATUS)


## revisar inativos

setor6_rev_inativos <-
  setor6_rev %>% filter(STATUS=="FECHOU" | STATUS=="SEM RECEITA") %>% .[,c(1:10,15)]

View(setor6_rev_inativos)


## sales

vendas <- dbGetQuery(con2, statement = read_file('NOVAS_CARTEIRAS_JUN23\\VENDAS.sql'))

View(vendas)

vendas_ano <-
vendas %>% group_by(ANO=format(floor_date(PEDDTBAIXA,"year"),"%Y"),CLICODIGO) %>% summarize(V=sum(VRVENDA)) %>% 
  dcast(CLICODIGO ~ ANO) 

View(vendas_ano)

vendas_ano2 <- 
left_join(setor6_rev,vendas_ano,by="CLICODIGO") 

View(vendas_ano2)

write.csv2(vendas_ano2,file = "vendas_ano2.csv")


## ativos

setor6_rev_ativos <-
  setor6_rev %>%  filter(is.na(STATUS) | STATUS=='ESPELHO' | STATUS=='VISITA MENSAL') %>% 
  .[,c(1:10,15)]

View(setor6_rev_ativos)



## SETOR 7 ================================================================================================


SETOR7_REV <- save(SETOR7_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR7_REV.RData")

setor7_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR7_REV.RData"))


View(setor7_rev)

setor7_rev %>% distinct(OBSERVAÇÃO)


## revisar inativos

setor7_rev_inativos <-
  setor7_rev %>%  rename(STATUS=OBSERVAÇÃO) %>% filter(STATUS=="INATIVO" | STATUS=="FECHOU" | STATUS=="SEM VENDA") 

View(setor7_rev_inativos)


## ativos

setor7_rev_ativos <-
  setor7_rev  %>% rename(STATUS=OBSERVAÇÃO) %>%  filter(is.na(STATUS) | STATUS=='ESPELHO') 

View(setor7_rev_ativos)



## WRITE SHEETS ===================================================================================

setores_rev_inativos <- rbind(setor1_rev_inativos,
                              setor2_rev_inativos,
                              setor3_rev_inativos,
                              setor4_rev_inativos,
                              setor5_rev_inativos,
                              setor6_rev_inativos,
                              setor7_rev_inativos)

View(setores_rev_inativos)

# split dataframe

num_rows <- nrow(setores_rev_inativos)

rows_per_part <- num_rows %/% 4

group_variable <- cut(seq_len(num_rows), breaks = 4, labels = FALSE)

split_data <- split(setores_rev_inativos, group_variable)


View(split_data)

## access each part

part1 <- split_data[[1]]
part2 <- split_data[[2]]
part3 <- split_data[[3]]
part4 <- split_data[[4]]
  
  
## write sheets  

range_write(part1,ss="1rhqjF4c_bKm4Utemn1DtSqL8S4VGOIWTfKAywHarGJ4", sheet = "GRUPOA")
range_write(part2,ss="1rhqjF4c_bKm4Utemn1DtSqL8S4VGOIWTfKAywHarGJ4", sheet = "GRUPOB")
range_write(part3,ss="1rhqjF4c_bKm4Utemn1DtSqL8S4VGOIWTfKAywHarGJ4", sheet = "GRUPOC")
range_write(part4,ss="1rhqjF4c_bKm4Utemn1DtSqL8S4VGOIWTfKAywHarGJ4", sheet = "GRUPOD")

split_data_csv <-
  rbind(part1,part2,part3,part4) %>% write_csv2(.,file="split_data_csv.csv")







