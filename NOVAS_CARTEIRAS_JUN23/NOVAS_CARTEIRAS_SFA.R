## NOVA ESTRUTURA DE CARTEIRAS

library(DBI)
library(tidyverse)
library(xlsx)
library(readr)
library(googlesheets4)
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
  setor1_rev %>% rename(STATUS=SITUAÇÃO) %>% filter(STATUS=='SEM VENDA'| STATUS=='INATIVO') 

View(setor1_rev_inativos)


## SETOR 2 ================================================================================================

SETOR2_REV <- save(SETOR2_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR2_REV.RData")

setor2_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR2_REV.RData"))

View(setor2_rev)

setor2_rev %>% distinct(STATUS)


setor2_rev_inativos <-
setor2_rev %>% filter(STATUS=='SEM VENDA'| STATUS=='INATIVO') 

View(setor2_rev_inativos)


## SETOR 3 ================================================================================================


SETOR3_REV <- save(SETOR3_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR3_REV.RData")

setor3_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR3_REV.RData"))

setor3_rev %>% distinct(STATUS)


## revisar inativos

setor3_rev_inativos <-
  setor3_rev %>% filter(STATUS=="FECHADO" | STATUS=="SEM VENDA") 

View(setor3_rev_inativos)


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



## SETOR 6 ================================================================================================


SETOR6_REV <- save(SETOR6_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR6_REV.RData")

setor6_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR6_REV.RData"))

View(setor6_rev)



## SETOR 7 ================================================================================================


SETOR7_REV <- save(SETOR7_REV,file = "C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR7_REV.RData")

setor7_rev <-
  get(load("C:\\Users\\Repro\\Documents\\R\\ADM\\VISITAS\\NOVAS_CARTEIRAS_JUN23\\SETOR7_REV.RData"))


View(setor7_rev)

setor7_rev %>% distinct(OBSERVAÇÃO)


## revisar inativos

setor7_rev_inativos <-
  setor7_rev %>%  rename(STATUS=OBSERVAÇÃO) %>% filter(STATUS=="INATIVO" | STATUS=="FECHOU" | STATUS=="Trocou CNPJ" | STATUS=="SEM VENDA") 

View(setor7_rev_inativos)



## WRITE SHEETS ===================================================================================

setores_rev_inativos <- rbind(setor1_rev_inativos,
                              setor2_rev_inativos,
                              setor3_rev_inativos,
                              setor4_rev_inativos,
                              setor5_rev_inativos,
                              setor7_rev_inativos)

View(setores_rev_inativos)

parts <- setores_rev_inativos %>% 
  slice(n = nrow(setores_rev_inativos), groups = rep(1:4, length.out = nrow(setores_rev_inativos)))

# Access each part
part1 <- parts[[1]]
part2 <- parts[[2]]
part3 <- parts[[3]]
part4 <- parts[[4]]
  
  
  

range_write(setor7_rev_inativos,ss="1rhqjF4c_bKm4Utemn1DtSqL8S4VGOIWTfKAywHarGJ4", sheet = "SETOR7")





