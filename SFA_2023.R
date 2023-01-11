
library(googlesheets4)
library(DBI)
library(tidyverse)
con2 <- dbConnect(odbc::odbc(), "reproreplica")


cli <- dbGetQuery(con2,"SELECT 
                           DISTINCT C.CLICODIGO,
                            REPLACE(REPLACE(REPLACE(CLICNPJCPF,'/',''),'-',''),'.','') CNPJ,
                            CLINOMEFANT,
                              C.GCLCODIGO,
                               GCLNOME,
                                SETOR,
                                 CIDNOME
                                  FROM CLIEN C
                                   INNER JOIN (SELECT CLICODIGO,
                                                     E.ZOCODIGO,
                                                      ZODESCRICAO SETOR,
                                                       ENDCODIGO,
                                                        CIDNOME 
                                                         FROM ENDCLI E
                                   LEFT JOIN CIDADE CD ON E.CIDCODIGO=CD.CIDCODIGO
                                    INNER JOIN (SELECT ZOCODIGO,
                                                      ZODESCRICAO 
                                                        FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28))Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                    LEFT JOIN GRUPOCLI GR ON C.GCLCODIGO=GR.GCLCODIGO
                                      WHERE CLICLIENTE='S'")

inativos <- dbGetQuery(con2,"
SELECT DISTINCT SITCLI.CLICODIGO,SITCODIGO FROM SITCLI
INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,MAX(SITDATA)ULTIMA FROM SITCLI
GROUP BY 1)A ON SITCLI.CLICODIGO=A.CLICODIGO AND A.ULTIMA=SITCLI.SITDATA 
INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,SITDATA,MAX(SITSEQ)USEQ FROM SITCLI
GROUP BY 1,2)MSEQ ON A.CLICODIGO=MSEQ.CLICODIGO AND MSEQ.SITDATA=A.ULTIMA 
AND MSEQ.USEQ=SITCLI.SITSEQ WHERE SITCODIGO=4
")

clien <- anti_join(cli,inativos,by="CLICODIGO")

clieninativos <- inner_join(cli,inativos,by="CLICODIGO")

View(clieninativos)

View(clien)

left_join(clien,SFA_100123 %>% distinct(CNPJ) %>% mutate(SFA='SFA')) %>% filter(is.na(SFA)) %>% View()





left_join(SFA_100123 %>% distinct(CNPJ),clien,by="CNPJ")%>% View()

## EXCLUIR SFA CLIENTES QUE NAO CONSTAM NO SGO

left_join(SFA_100123 %>% distinct(CNPJ),clien,by="CNPJ") %>% filter(is.na(CLICODIGO)) %>% write_sheet(.,ss="13DCmIXs9mRio2_yH6R5Vn80nXpYT--laiewdD3jql9g",sheet="EXCLUIR SFA")


## SETOR 1

SFA_100123 %>% filter(SETOR==120102) %>% View()
clien %>% filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>% View()


left_join(clien %>%  filter(SETOR=='SETOR 1 - FLORIANOPOLIS REDES') %>% select(CNPJ) ,SFA_100123 %>% filter(!SETOR %in% c(120102,121002))  %>% 
            select(SETOR,CNPJ),by="CNPJ") %>% filter(!is.na(SETOR)) %>% 

write_sheet(.,ss="13DCmIXs9mRio2_yH6R5Vn80nXpYT--laiewdD3jql9g",sheet="SETOR1 120102")

## SETOR 2


left_join(clien %>%  filter(SETOR=='SETOR 2 - CRICIUMA - SUL') %>% select(CNPJ) ,SFA_100123 %>% filter(!SETOR %in% c(120101,121002)) %>% 
            select(SETOR,CNPJ),by="CNPJ") %>% filter(!is.na(SETOR)) %>% 
  
  write_sheet(.,ss="13DCmIXs9mRio2_yH6R5Vn80nXpYT--laiewdD3jql9g",sheet="SETOR2 120101")


## SETOR 3

left_join(clien %>%  filter(SETOR=='SETOR 3 - CHAPECO - OESTE - RS') %>% select(CNPJ) ,SFA_100123 %>% filter(!SETOR %in% c(120105,121001)) %>% 
            select(SETOR,CNPJ),by="CNPJ") %>% filter(!is.na(SETOR)) %>% 
  
  write_sheet(.,ss="13DCmIXs9mRio2_yH6R5Vn80nXpYT--laiewdD3jql9g",sheet="SETOR3 120105")

## SETOR 4

left_join(clien %>%  filter(SETOR=='SETOR 4 - JOINVILLE - NORTE') %>% select(CNPJ) ,SFA_100123 %>% filter(!SETOR %in% c(120103,121001)) %>% 
            select(SETOR,CNPJ),by="CNPJ") %>% filter(!is.na(SETOR)) %>% 
  
  write_sheet(.,ss="13DCmIXs9mRio2_yH6R5Vn80nXpYT--laiewdD3jql9g",sheet="SETOR4 120103")

## SETOR 5

left_join(clien %>%  filter(SETOR=='SETOR 5 - BLUMENAU - VALE') %>% select(CNPJ) ,SFA_100123 %>% filter(!SETOR %in% c(120104,121004)) %>% 
            select(SETOR,CNPJ),by="CNPJ") %>% filter(!is.na(SETOR)) %>% 
  
  write_sheet(.,ss="13DCmIXs9mRio2_yH6R5Vn80nXpYT--laiewdD3jql9g",sheet="SETOR5 120104")

## SETOR 6

left_join(clien %>%  filter(SETOR=='SETOR 6 - B CAMBORIU - LITORAL') %>% select(CNPJ) ,SFA_100123 %>% filter(!SETOR %in% c(120106,121004)) %>% 
            select(SETOR,CNPJ),by="CNPJ") %>% filter(!is.na(SETOR)) %>% 
  
  write_sheet(.,ss="13DCmIXs9mRio2_yH6R5Vn80nXpYT--laiewdD3jql9g",sheet="SETOR6 120106")


## SETOR 7

left_join(clien %>%  filter(SETOR=='SETOR 7 - FLORIANOPOLIS  LOJAS') %>% select(CNPJ) ,SFA_100123 %>% filter(!SETOR %in% c(120107,121002)) %>% 
            select(SETOR,CNPJ),by="CNPJ") %>% filter(!is.na(SETOR)) %>% 
  
  write_sheet(.,ss="13DCmIXs9mRio2_yH6R5Vn80nXpYT--laiewdD3jql9g",sheet="SETOR7 120107")


## SETOR 8

left_join(clien %>%  filter(SETOR=='SETOR 8 - JOACABA - MEIO OESTE') %>% select(CNPJ) ,SFA_100123 %>% filter(!SETOR %in% c(120108,121001)) %>% 
            select(SETOR,CNPJ),by="CNPJ") %>% filter(!is.na(SETOR)) %>% 
  
  write_sheet(.,ss="13DCmIXs9mRio2_yH6R5Vn80nXpYT--laiewdD3jql9g",sheet="SETOR8 120108")


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


