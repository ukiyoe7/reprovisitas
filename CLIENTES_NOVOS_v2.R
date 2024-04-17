# CLIENTES NOVOS SFA
# 16.04.2024

## LOAD =================

library(DBI)
library(dplyr)
library(xlsx)
library(readxl)
library(openxlsx)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")

## SFA =================

SFA <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\SFA\\2024\\SFA.xlsx") 

aux_setores <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\SFA\\2024\\AUX_SETORES.xlsx") %>% mutate(SETOR=as.character(SETOR))


## SGO =================

cli_novo <- dbGetQuery(con2,"SELECT DISTINCT C.CLICODIGO, GCLCODIGO,REPLACE(REPLACE(REPLACE(CLICNPJCPF,'/',''),'-',''),'.','') CNPJ, CLINOMEFANT NOMEFANTASIA, CLIRAZSOCIAL RAZAOSOCIAL, SETOR,CLIDTCAD
FROM CLIEN C
INNER JOIN (SELECT CLICODIGO, E.ZOCODIGO, ZODESCRICAO SETOR, ENDCODIGO
            FROM ENDCLI E
            INNER JOIN (SELECT ZOCODIGO, ZODESCRICAO 
                        FROM ZONA 
                        WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28)) Z 
            ON E.ZOCODIGO=Z.ZOCODIGO 
            WHERE ENDFAT='S') A 
ON C.CLICODIGO=A.CLICODIGO
WHERE CLICLIENTE='S' AND CLIDTCAD>=CURRENT_DATE - 60")


inativos <- dbGetQuery(con2,"
SELECT DISTINCT SITCLI.CLICODIGO,SITCODIGO FROM SITCLI
INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,MAX(SITDATA)ULTIMA FROM SITCLI
GROUP BY 1)A ON SITCLI.CLICODIGO=A.CLICODIGO AND A.ULTIMA=SITCLI.SITDATA 
INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,SITDATA,MAX(SITSEQ)USEQ FROM SITCLI
GROUP BY 1,2)MSEQ ON A.CLICODIGO=MSEQ.CLICODIGO AND MSEQ.SITDATA=A.ULTIMA 
AND MSEQ.USEQ=SITCLI.SITSEQ WHERE SITCODIGO=4
")

clien_novo <- anti_join(cli_novo,inativos,by="CLICODIGO")


email <- dbGetQuery(con2,"
SELECT CLINET.CLICODIGO,CLINET.NETENDERECO EMAIL FROM CLINET
LEFT JOIN PARAMCLINET ON CLINET.CLICODIGO=PARAMCLINET.CLICODIGO AND CLINET.NETCODIGO=PARAMCLINET.NETCODIGO
WHERE PARNOME='Fatura' AND CLINET.NETCODIGO=1
")

endcli_sql <- dbGetQuery(con2,"
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
                                  CLICLIENTE='S' AND ENDFAT='S'") 


## ANTI JOIN SFA SGO =============

cli_novo_SFA <- 
anti_join(clien_novo,SFA,by="CNPJ") 

View(cli_novo_SFA)

## CONSULTORES ============================

corder1 <- c("CIA","EQUIPE","SETOR","CNPJ","LOTR_CODIGO","INSCRICAO","RAZAOSOCIAL","NOMEFANTASIA","EMAIL","FREQUENCIA")

ESTAB_PDV_consultores <- cli_novo_SFA %>% 
  left_join(.,aux_setores,by="SETOR") %>% 
  mutate(SETOR=SFA) %>% 
  mutate(NOMEFANTASIA=paste0(NOMEFANTASIA," (",CLICODIGO,")")) %>% 
  mutate(CIA=1) %>% 
  mutate(EQUIPE=4) %>% 
  mutate(LOTR_CODIGO='') %>% 
  mutate(INSCRICAO='') %>% 
  mutate(FREQUENCIA='EVENTUAL') %>% left_join(.,email,by="CLICODIGO") %>% 
  arrange(desc(CLICODIGO)) %>% 
  filter(!is.na(SETOR)) %>% 
  .[,corder1] 

View(ESTAB_PDV_consultores)  

corder2 <- c("CIA","EQUIPE","SETOR","CNPJ","REGIAO","TP_LOGRAD","LOGRAD","NUMERO","COMPL","CEP","BAIRRO","CIDADE","UF","DDD","TELEF1")

ESTAB_LOCAL_PDV_consultores <- inner_join(cli_novo_SFA,endcli_sql,by="CLICODIGO") %>%
  left_join(.,aux_setores,by="SETOR") %>% 
  mutate(SETOR=SFA) %>%
  mutate(CIA=1) %>% 
  mutate(EQUIPE=4) %>% 
  mutate(REGIAO='') %>% 
  arrange(desc(CLICODIGO)) %>% 
  rename(CNPJ=CNPJ.x) %>% 
  filter(!is.na(SETOR)) %>%
  .[,corder2] 

View(ESTAB_LOCAL_PDV_consultores)


## ESPELHO 1 ============================

ESTAB_PDV_espelho1 <-
cli_novo_SFA %>%  left_join(.,aux_setores,by="SETOR") %>% filter(is.na(GCLCODIGO)) %>% 
  mutate(SETOR=SFA) %>% 
  filter(SETOR %in% c(120102,120101,120107)) %>%
  mutate(NOMEFANTASIA=paste0(NOMEFANTASIA," (",CLICODIGO,")")) %>% 
  mutate(CIA=1) %>% 
  mutate(EQUIPE=4) %>% 
  mutate(LOTR_CODIGO='') %>% 
  mutate(INSCRICAO='') %>% 
  mutate(FREQUENCIA='EVENTUAL') %>% left_join(.,email,by="CLICODIGO") %>% 
  arrange(desc(CLICODIGO)) %>%
  filter(!is.na(SETOR)) %>%
  .[,corder1]

View(ESTAB_PDV_espelho1)


ESTAB_LOCAL_PDV_espelho1 <- 
  inner_join(cli_novo_SFA,endcli_sql,by="CLICODIGO") %>%
  left_join(.,aux_setores,by="SETOR") %>% filter(is.na(GCLCODIGO)) %>%
  mutate(SETOR=SFA) %>%
  filter(SETOR %in% c(120102,120101,120107)) %>%
  mutate(CIA=1) %>% 
  mutate(EQUIPE=4) %>% 
  mutate(REGIAO='') %>% 
  arrange(desc(CLICODIGO)) %>% 
  rename(CNPJ=CNPJ.x) %>% 
  filter(!is.na(SETOR)) %>%
  .[,corder2] 

View(ESTAB_LOCAL_PDV_espelho1)


## ESPELHO 2 ===========================

ESTAB_PDV_espelho2 <-
cli_novo_SFA %>%  left_join(.,aux_setores,by="SETOR") %>% filter(is.na(GCLCODIGO)) %>% 
  mutate(SETOR=SFA) %>% 
  filter(SETOR %in% c(120104,120106)) %>%
  mutate(NOMEFANTASIA=paste0(NOMEFANTASIA," (",CLICODIGO,")")) %>% 
  mutate(CIA=1) %>% 
  mutate(EQUIPE=4) %>% 
  mutate(LOTR_CODIGO='') %>% 
  mutate(INSCRICAO='') %>% 
  mutate(FREQUENCIA='EVENTUAL') %>% left_join(.,email,by="CLICODIGO") %>% 
  arrange(desc(CLICODIGO)) %>% 
  filter(!is.na(SETOR)) %>%
  .[,corder1]

View(ESTAB_PDV_espelho2)

ESTAB_LOCAL_PDV_espelho2 <- 
  inner_join(cli_novo_SFA,endcli_sql,by="CLICODIGO") %>%
  left_join(.,aux_setores,by="SETOR") %>% filter(is.na(GCLCODIGO)) %>%
  mutate(SETOR=SFA) %>%
  filter(SETOR %in% c(120104,120106)) %>%
  mutate(CIA=1) %>% 
  mutate(EQUIPE=4) %>% 
  mutate(REGIAO='') %>% 
  arrange(desc(CLICODIGO)) %>% 
  rename(CNPJ=CNPJ.x) %>% 
  filter(!is.na(SETOR)) %>%
  .[,corder2] 

View(ESTAB_LOCAL_PDV_espelho2)


## ESPELHO 3 ===================================

ESTAB_PDV_espelho3 <-
cli_novo_SFA %>%  left_join(.,aux_setores,by="SETOR") %>% filter(is.na(GCLCODIGO)) %>% 
  mutate(SETOR=SFA) %>% 
  filter(SETOR %in% c(120105,120103)) %>%
  mutate(NOMEFANTASIA=paste0(NOMEFANTASIA," (",CLICODIGO,")")) %>% 
  mutate(CIA=1) %>% 
  mutate(EQUIPE=4) %>% 
  mutate(LOTR_CODIGO='') %>% 
  mutate(INSCRICAO='') %>% 
  mutate(FREQUENCIA='EVENTUAL') %>% left_join(.,email,by="CLICODIGO") %>% 
  arrange(desc(CLICODIGO)) %>% 
  filter(!is.na(SETOR)) %>%
  .[,corder1]
  
  View(ESTAB_PDV_espelho3)

  ESTAB_LOCAL_PDV_espelho3 <- 
    inner_join(cli_novo_SFA,endcli_sql,by="CLICODIGO") %>%
    left_join(.,aux_setores,by="SETOR") %>% filter(is.na(GCLCODIGO)) %>%
    mutate(SETOR=SFA) %>%
    filter(SETOR %in% c(120105,120103)) %>%
    mutate(CIA=1) %>% 
    mutate(EQUIPE=4) %>% 
    mutate(REGIAO='') %>% 
    arrange(desc(CLICODIGO)) %>% 
    rename(CNPJ=CNPJ.x) %>% 
    filter(!is.na(SETOR)) %>%
    .[,corder2] 
  
  View(ESTAB_LOCAL_PDV_espelho3)
  
  ## WRITE EXCEL =====================
  
  wbksfa <- createWorkbook()
  
  addWorksheet(wbksfa, "ESTAB_PDV_consultores")
  
  addWorksheet(wbksfa, "ESTAB_LOCAL_PDV_consultores")
  
  addWorksheet(wbksfa, "ESTAB_PDV_espelho1")
  
  addWorksheet(wbksfa, "ESTAB_LOCAL_PDV_espelho1")
  
  addWorksheet(wbksfa, "ESTAB_PDV_espelho2")
  
  addWorksheet(wbksfa, "ESTAB_LOCAL_PDV_espelho2")
  
  addWorksheet(wbksfa, "ESTAB_PDV_espelho3")
  
  addWorksheet(wbksfa, "ESTAB_LOCAL_PDV_espelho3")
  
  writeDataTable(wbksfa, "ESTAB_PDV_consultores", ESTAB_PDV_consultores, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  writeDataTable(wbksfa, "ESTAB_LOCAL_PDV_consultores", ESTAB_LOCAL_PDV_consultores, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  writeDataTable(wbksfa, "ESTAB_PDV_espelho1", ESTAB_PDV_espelho1, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  writeDataTable(wbksfa, "ESTAB_LOCAL_PDV_espelho1", ESTAB_LOCAL_PDV_espelho1, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  writeDataTable(wbksfa, "ESTAB_PDV_espelho2", ESTAB_PDV_espelho2, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  writeDataTable(wbksfa, "ESTAB_LOCAL_PDV_espelho2", ESTAB_LOCAL_PDV_espelho2, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  writeDataTable(wbksfa, "ESTAB_PDV_espelho3",  ESTAB_PDV_espelho3, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  writeDataTable(wbksfa, "ESTAB_LOCAL_PDV_espelho3", ESTAB_LOCAL_PDV_espelho3, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
    
  SFA_NOVOS <-  paste0("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\SFA\\2024\\SFA_NOVOS","_",format(Sys.Date(),"%d_%m_%y"),".xlsx")
  
  saveWorkbook(wbksfa, file = SFA_NOVOS, overwrite = TRUE)
  
  
  
  