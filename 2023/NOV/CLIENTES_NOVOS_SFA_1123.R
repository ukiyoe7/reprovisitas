## COMERCIAL PROCESS AUTOMATION
## 24.11.2023
## VERIFACAO DE NOVOS CLIENTES PARA INCLUSAO NA BASE SFA

library(googlesheets4)
library(DBI)
library(tidyverse)
library(glue)
con2 <- dbConnect(odbc::odbc(), "repro",encoding="Latin1")

## =========================================================================================

aux_setores <- 
  read_sheet(ss="1m0VNU9HTRx_-bLRwLGljrIE7ZS5GmhM10LHXeyt9P4g",sheet = "AUX") %>% 
  select(SETOR,SFA)


cli_novo <- dbGetQuery(con2,"SELECT 
                           DISTINCT C.CLICODIGO,
                            REPLACE(REPLACE(REPLACE(CLICNPJCPF,'/',''),'-',''),'.','') CNPJ,
                             CLINOMEFANT NOMEFANTASIA,
                              CLIRAZSOCIAL RAZAOSOCIAL,
                               SETOR
                                   FROM CLIEN C
                                    INNER JOIN (SELECT CLICODIGO,
                                                     E.ZOCODIGO,
                                                      ZODESCRICAO SETOR,
                                                       ENDCODIGO
                                                         FROM ENDCLI E
                                    INNER JOIN (SELECT ZOCODIGO,
                                                        ZODESCRICAO 
                                                        FROM ZONA 
                                                         WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28))Z ON 
                                                           E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                      WHERE CLICLIENTE='S' AND CLIDTCAD>='01.09.2023'")


inativos <- dbGetQuery(con2,"
SELECT DISTINCT SITCLI.CLICODIGO,SITCODIGO FROM SITCLI
INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,MAX(SITDATA)ULTIMA FROM SITCLI
GROUP BY 1)A ON SITCLI.CLICODIGO=A.CLICODIGO AND A.ULTIMA=SITCLI.SITDATA 
INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,SITDATA,MAX(SITSEQ)USEQ FROM SITCLI
GROUP BY 1,2)MSEQ ON A.CLICODIGO=MSEQ.CLICODIGO AND MSEQ.SITDATA=A.ULTIMA 
AND MSEQ.USEQ=SITCLI.SITSEQ WHERE SITCODIGO=4
")

email <- dbGetQuery(con2,"
SELECT CLINET.CLICODIGO,CLINET.NETENDERECO EMAIL FROM CLINET
LEFT JOIN PARAMCLINET ON CLINET.CLICODIGO=PARAMCLINET.CLICODIGO AND CLINET.NETCODIGO=PARAMCLINET.NETCODIGO
WHERE PARNOME='Fatura' AND CLINET.NETCODIGO=1
")


## =========================================================================================
## cross join data and write on google sheets

clien_novo <- anti_join(cli_novo,inativos,by="CLICODIGO")


corder1 <- c("CIA","EQUIPE","SETOR","CNPJ","LOTR_CODIGO","INSCRICAO","RAZAOSOCIAL","NOMEFANTASIA","EMAIL","FREQUENCIA")


ESTAB_PDV <- anti_join(clien_novo,SFA_241123 %>% 
                         distinct(CNPJ),by="CNPJ") %>% 
  left_join(.,aux_setores,by="SETOR") %>%
  mutate(SETOR=SFA) %>% 
  mutate(NOMEFANTASIA=paste0(NOMEFANTASIA," (",CLICODIGO,")")) %>% 
  mutate(CIA='A') %>% 
  mutate(EQUIPE=21) %>% 
  mutate(LOTR_CODIGO=5) %>% 
  mutate(INSCRICAO='') %>% 
  mutate(FREQUENCIA=0) %>% left_join(.,email,by="CLICODIGO") %>% 
  arrange(desc(CLICODIGO)) %>% 
  .[,corder1] 


## ENDERECO =========================================================================================


edx <- anti_join(clien_novo,SFA_241123 %>% distinct(CNPJ)) %>% select(CLICODIGO)


endcli_sql <- glue_sql("
SELECT DISTINCT C.CLICODIGO,
                 CLINOMEFANT,
                  CLIRAZSOCIAL, 
                   REPLACE(REPLACE(REPLACE(CLICNPJCPF,'/',''),'-',''),'.','') CNPJ,
                    ENDTPRUA TP_LOGRAD,
                     ENDENDERECO LOGRAD, 
                      ENDNR NUMERO, 
                       ENDCOMPLE COMPL, 
                        ENDCEP CEP,  
                         ENDBAIRRO BAIRRO,
                          CIDNOME CIDADE,
                           CIDUF UF,
                            ENDDDD1 DDD, 
                             ENDFONE1 TELEF1
                              FROM CLIEN C
                               LEFT JOIN ENDCLI ED ON C.CLICODIGO=ED.CLICODIGO
                                LEFT JOIN CIDADE CD ON ED.CIDCODIGO=CD.CIDCODIGO
                                 WHERE
                                  CLICLIENTE='S' AND ENDFAT='S' AND C.CLICODIGO IN ({edx$CLICODIGO*})")

endcli<-  dbGetQuery(con2,endcli_sql) 


corder <- c("CIA","EQUIPE","SETOR","CNPJ","REGIAO","TP_LOGRAD","LOGRAD","NUMERO","COMPL","CEP","BAIRRO","CIDADE","UF","DDD","TELEF1")


ESTAB_LOCAL_PDV <- left_join(endcli,cli_novo %>% select(CNPJ,SETOR) %>% as.data.frame(),by="CNPJ") %>%
  left_join(.,aux_setores,by="SETOR") %>% 
  mutate(SETOR=SFA) %>%
  mutate(CIA='A') %>% 
  mutate(EQUIPE=21) %>% 
  mutate(REGIAO='') %>% 
  arrange(desc(CLICODIGO)) %>% 
  .[,corder] 


View(ESTAB_LOCAL_PDV)


## ESPELHOS =========================================================================================


ESTAB_PDV_ESPELHOS <-
left_join(ESTAB_PDV,AUX_ESPELHOS_SETORES,by="SETOR") %>% 
   mutate(SETOR=SETOR_ESPELHO) %>% 
    filter(!is.na(SETOR)) %>% 
     select(-ESPELHO,-SETOR_ESPELHO) %>% 
      mutate(EQUIPE=80)


View(ESTAB_PDV_ESPELHOS)



ESTAB_LOCAL_PDV_ESPELHOS <-
  left_join(ESTAB_LOCAL_PDV,AUX_ESPELHOS_SETORES,by="SETOR") %>% 
  mutate(SETOR=SETOR_ESPELHO) %>% 
  filter(!is.na(SETOR)) %>% 
  select(-ESPELHO,-SETOR_ESPELHO) %>% 
  mutate(EQUIPE=80)


View(ESTAB_LOCAL_PDV_ESPELHOS)


## UNE BASES =========================================================================================

ESTAB_PDV_ALL <-
union_all(ESTAB_PDV,ESTAB_PDV_ESPELHOS) 

ESTAB_LOCAL_PDV_ALL <- 
  union_all(ESTAB_LOCAL_PDV,ESTAB_LOCAL_PDV_ESPELHOS) 

write.csv2(ESTAB_PDV_ALL,file = "ESTAB_PDV_ALL.csv",row.names = FALSE,na="")

write.csv2(ESTAB_LOCAL_PDV_ALL,file = "ESTAB_LOCAL_PDV_ALL.csv",row.names = FALSE,na="")

