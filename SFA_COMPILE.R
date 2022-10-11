library(DBI)
library(tidyverse)
library(googlesheets4)


con2 <- dbConnect(odbc::odbc(), "reproreplica")

## CLIENTS

cli <- dbGetQuery(con2,"SELECT DISTINCT C.CLICODIGO,
                          CLINOMEFANT,
                           CLICNPJCPF,
                            REPLACE(
                             REPLACE(
                              REPLACE(CLICNPJCPF,'.',''),'/',''),'-','') CNPJ,
                                CIDNOME,
                                 C.GCLCODIGO,
                                  GCLNOME,  
                                   IIF(C.GCLCODIGO IS NULL,C.CLICODIGO || ' ' || CLINOMEFANT,'G' || C.GCLCODIGO || ' ' || GCLNOME) CLIENTE,
                                    SETOR
                                     FROM CLIEN C
                                      LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,CIDNOME,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                                       LEFT JOIN CIDADE CID ON E.CIDCODIGO=CID.CIDCODIGO
                                        LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28))Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                         LEFT JOIN GRUPOCLI GR ON C.GCLCODIGO=GR.GCLCODIGO
                                          WHERE CLICLIENTE='S'
                                    ") %>% mutate(CNPJ2=substr(CNPJ,nchar(CNPJ)-10,nchar(CNPJ)))




## COMPILE ALL SECTORS

SFA_COMPILE <- union_all(SFA_SETOR1,SFA_SETOR2) %>% 
                union_all(.,SFA_SETOR3) %>% 
                 union_all(.,SFA_SETOR4) %>% 
                  union_all(.,SFA_SETOR5) %>% 
                   union_all(.,SFA_SETOR6) %>% 
                    union_all(.,SFA_SETOR7) %>% 
                     union_all(.,SFA_SETOR8) %>% .[,c(3,4)] %>% 
   mutate(CNPJ2=substr(CNPJ,nchar(CNPJ)-10,nchar(CNPJ))) %>% rename(CLICODIGO=1)

View(SFA_COMPILE)


SFA_COMPILE %>% .[duplicated(.$CLICODIGO),]

## AJUSTE SETOR 3


PREF_RS <- PREF_RS[1:45,1] %>% mutate(CNPJ2=substr(CNPJ,nchar(CNPJ)-10,nchar(CNPJ)))

SFA_SETOR3_AJUSTADO <- union_all(
                           SFA_SETOR3 %>%  filter(ESTADO !='RS'),
  
                             SFA_SETOR3 %>%
                               mutate(CNPJ2=substr(CNPJ,nchar(CNPJ)-10,nchar(CNPJ))) %>%  
                                left_join(.,PREF_RS,by="CNPJ2") %>% 
                                  filter(ESTADO=='RS' &  !is.na(CNPJ.y))  %>% .[,c(-15,-16)])

View(SFA_SETOR3_AJUSTADO)


## COMPILE ALL ================================================================

SFA_SETOR1_COMP <-  left_join(SFA_SETOR1 %>% 
                      mutate(`CÓDIGO REPRO`=as.integer(`CÓDIGO REPRO`)) %>% 
                       rename(CLICODIGO=3),cli,by="CLICODIGO") %>% 
                        .[,c(3,5,16,17,7,8)]


View(SFA_SETOR1_COMP)

SFA_SETOR2_COMP <-  left_join(SFA_SETOR2 %>% 
                                mutate(`CÓDIGO REPRO`=as.integer(`CÓDIGO REPRO`)) %>% 
                                rename(CLICODIGO=3),cli,by="CLICODIGO") %>% 
  .[,c(3,5,16,17,7,8)]


View(SFA_SETOR2_COMP)


SFA_SETOR3_COMP <-  left_join(SFA_SETOR3_AJUSTADO %>% 
                                mutate(`CÓDIGO REPRO`=as.integer(`CÓDIGO REPRO`)) %>% 
                                rename(CLICODIGO=3),cli,by="CLICODIGO") %>% 
  .[,c(3,5,16,17,7,8)]


View(SFA_SETOR3_COMP)


SFA_SETOR4_COMP <-  left_join(SFA_SETOR4 %>% 
                                mutate(`CÓDIGO REPRO`=as.integer(`CÓDIGO REPRO`)) %>% 
                                rename(CLICODIGO=3),cli,by="CLICODIGO") %>% 
  .[,c(3,5,16,17,7,8)]


View(SFA_SETOR4_COMP)


SFA_SETOR5_COMP <-  left_join(SFA_SETOR5 %>% 
                                mutate(`CÓDIGO REPRO`=as.integer(`CÓDIGO REPRO`)) %>% 
                                rename(CLICODIGO=3),cli,by="CLICODIGO") %>% 
  .[,c(3,5,16,17,7,8)]


View(SFA_SETOR5_COMP)


SFA_SETOR6_COMP <-  left_join(SFA_SETOR6 %>% 
                                mutate(`CÓDIGO REPRO`=as.integer(`CÓDIGO REPRO`)) %>% 
                                rename(CLICODIGO=3),cli,by="CLICODIGO") %>% 
  .[,c(3,5,16,17,7,8)]


View(SFA_SETOR6_COMP)


SFA_SETOR7_COMP <-  left_join(SFA_SETOR7 %>% 
                                mutate(`CÓDIGO REPRO`=as.integer(`CÓDIGO REPRO`)) %>% 
                                rename(CLICODIGO=3),cli,by="CLICODIGO") %>% 
  .[,c(3,5,16,17,7,8)]


View(SFA_SETOR7_COMP)









                
