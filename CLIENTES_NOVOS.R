library(googlesheets4)
library(DBI)
library(tidyverse)


aux_setores <- 
  read_sheet(ss="1m0VNU9HTRx_-bLRwLGljrIE7ZS5GmhM10LHXeyt9P4g",sheet = "AUX") %>% select(SETOR,SFA)


cli_novo <- dbGetQuery(con2,"SELECT 
                           DISTINCT C.CLICODIGO,
                            REPLACE(REPLACE(REPLACE(CLICNPJCPF,'/',''),'-',''),'.','') CNPJ,
                             CLINOMEFANT ,
                              CLIRAZSOCIAL,
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
                                      WHERE CLICLIENTE='S' AND CLIDTCAD>='01.10.2022'")


inativos <- dbGetQuery(con2,"
SELECT DISTINCT SITCLI.CLICODIGO,SITCODIGO FROM SITCLI
INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,MAX(SITDATA)ULTIMA FROM SITCLI
GROUP BY 1)A ON SITCLI.CLICODIGO=A.CLICODIGO AND A.ULTIMA=SITCLI.SITDATA 
INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,SITDATA,MAX(SITSEQ)USEQ FROM SITCLI
GROUP BY 1,2)MSEQ ON A.CLICODIGO=MSEQ.CLICODIGO AND MSEQ.SITDATA=A.ULTIMA 
AND MSEQ.USEQ=SITCLI.SITSEQ WHERE SITCODIGO=4
")

email <- dbGetQuery(con2,"
SELECT CLINET.CLICODIGO,CLINET.NETENDERECO FROM CLINET
LEFT JOIN PARAMCLINET ON CLINET.CLICODIGO=PARAMCLINET.CLICODIGO AND CLINET.NETCODIGO=PARAMCLINET.NETCODIGO
WHERE PARNOME='Fatura' AND CLINET.NETCODIGO=1
")


clien_novo <- anti_join(cli_novo,inativos,by="CLICODIGO")

View(clien_novo)


novos_sfa <- 
anti_join(clien_novo,SFA_100123 %>% 
                   distinct(CNPJ)) %>%  
                    left_join(.,aux_setores,by="SETOR") %>% 
                     mutate(NOMEFANTASIA=paste0(CLINOMEFANT," (",CLICODIGO,")")) %>% 
                      mutate(CIA=1) %>% 
                       mutate(EQUIPE=4) %>% 
                        mutate(LOTR_CODIGO='') %>% 
                         mutate(INSCRICAO='') %>% 
                          mutate(FREQUENCIA='EVENTUAL') %>% left_join(.,email,by="CLICODIGO") %>% 
                           .[,c(8,9,6,2,10,11,4,7,13,12)]
                           
                       
         

View(novos_sfa)







## ENDERECO

endcli <- 
  dbGetQuery(con2,"
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
CLICLIENTE='S'
AND ENDFAT='S' ")

View(endcli)