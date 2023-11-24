
## ANALISE SFA
## 24.11.2023


library(DBI)
library(tidyverse)


con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")


## CLIENTE

cli <- dbGetQuery(con2, statement = read_file('CLIENTS.sql'))

View(cli)

inativos <- dbGetQuery(con2, statement = read_file('INATIVOS.sql'))

View(inativos)

clien <- anti_join(cli,inativos,by="CLICODIGO") 

View(clien)


cli_setor <-
clien %>% 
group_by(SETOR) %>% summarize(QTD=n_distinct(CNPJ)) 

View(cli_setor)

## SFA


sfa_setor <-
SFA_171123 %>% group_by(NOME_SETOR,SETOR) %>% summarize(QTD=n_distinct(CNPJ))

View(sfa_setor)



