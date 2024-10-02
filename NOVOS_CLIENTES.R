# CLIENTES NOVOS SFA

## LOAD =================

library(DBI)
library(dplyr)
library(xlsx)
library(readxl)
library(openxlsx)
library(lubridate)

con2 <- dbConnect(odbc::odbc(), "repro",encoding = "latin1")

## SFA =================

SFA <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\SFA\\2024\\SFA.xlsx") 

SFA_ESPELHOS <- read_excel("C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\SFA\\2024\\SFA_ESPELHOS.xlsx") 

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
  
  
  cli_novo_SFA_ESPELHOS <- 
    anti_join(clien_novo,SFA_ESPELHOS,by="CNPJ") 
  
  
  cli_novo_SFA <-
  union_all(cli_novo_SFA,cli_novo_SFA_ESPELHOS) %>% distinct(.) %>% mutate(NOME_SETOR='')
  
  
  # Get current year, month, and day
  current_year <- format(Sys.Date(), "%Y")
  current_month <- toupper(format(Sys.Date(), "%b"))
  current_day <- format(Sys.Date(), "%d")
  
  
  # Construct file path with current year, month, and day
  file_path <- paste0(
    "C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\SFA\\",
    current_year, "\\", current_month, "\\NOVOS_SFA_", 
    current_day, "_", current_month, "_", current_year, ".xlsx"
  )
  
  
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  
  # Write CSV with updated file path
  write.csv2(cli_novo_SFA,
             file = file_path,
             row.names = FALSE,
             quote = FALSE)
  
  
  ## EXCEL

  wbx <- createWorkbook()
  
  addWorksheet(wbx, "SFA")
  
  # Set column widths
  setColWidths(wbx, sheet = "SFA", cols = 1, widths = 10)
  
  setColWidths(wbx, sheet = "SFA", cols = 2, widths = 10)
  
  setColWidths(wbx, sheet = "SFA", cols = 3, widths = 15)
  
  setColWidths(wbx, sheet = "SFA", cols = 4, widths = 30)
  
  setColWidths(wbx, sheet = "SFA", cols = 5, widths = 30)
  
  setColWidths(wbx, sheet = "SFA", cols = 6, widths = 30)

  setColWidths(wbx, sheet = "SFA", cols = 7, widths = 15)
  
  setColWidths(wbx, sheet = "SFA", cols = 8, widths = 20)
  
  datesty <- createStyle(numFmt = "dd/MM/yyyy")
  
  writeDataTable(wbx, "SFA",cli_novo_SFA, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium2",tableName = NULL, headerStyle = NULL, withFilter = FALSE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  addStyle(wbx, sheet = "SFA", style = datesty, cols = 7, rows = 1:nrow(cli_novo_SFA)+1, gridExpand = TRUE)
  
  dataValidation(wbx, sheet = "SFA", col = 8, rows = 2:20, type = "list", value = "'a','b','c'",allowBlank = TRUE, showInputMsg = TRUE, showErrorMsg = TRUE)

  saveWorkbook(wbx, file_path, overwrite = TRUE)  


## get validated data =============================================

  
sfa_valid <-   
read_excel(file_path) %>% left_join(.,AUX_SFA_SGO %>% select(FUNNOME,SETORSFA),by=c("CONSULTOR"="FUNNOME")) %>% select(-NOME_SETOR,-CONSULTOR,-ATENDENTE,-SETOR) 
  
  
  ## CONSULTORES ============================
  
  corder1 <- c("CIA","EQUIPE","SETOR","CNPJ","LOTR_CODIGO","INSCRICAO","RAZAOSOCIAL","NOMEFANTASIA","EMAIL","FREQUENCIA")
  
  ESTAB_PDV_consultores <- sfa_valid %>% 
    mutate(SETOR=SETORSFA) %>% 
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
  
  ESTAB_LOCAL_PDV_consultores <- inner_join(sfa_valid,endcli_sql,by="CLICODIGO") %>%
    mutate(SETOR=SETORSFA) %>%
    mutate(CIA=1) %>% 
    mutate(EQUIPE=4) %>% 
    mutate(REGIAO='') %>% 
    arrange(desc(CLICODIGO)) %>% 
    rename(CNPJ=CNPJ.x) %>% 
    filter(!is.na(SETOR)) %>%
    .[,corder2] 
  
  View(ESTAB_LOCAL_PDV_consultores)
  
  file_path2 <- paste0(
    "C:\\Users\\REPRO SANDRO\\OneDrive - Luxottica Group S.p.A (1)\\SFA\\",
    current_year, "\\", current_month, "\\NOVOS_SFA_VALID_", 
    current_day, "_", current_month, "_", current_year, ".xlsx"
  )
  
  ## WRITE EXCEL =====================
  
  wbksfa <- createWorkbook()
  
  addWorksheet(wbksfa, "ESTAB_PDV_consultores")
  
  addWorksheet(wbksfa, "ESTAB_LOCAL_PDV_consultores")
  
  
  writeDataTable(wbksfa, "ESTAB_PDV_consultores", ESTAB_PDV_consultores, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  writeDataTable(wbksfa, "ESTAB_LOCAL_PDV_consultores", ESTAB_LOCAL_PDV_consultores, startCol = 1, startRow = 1, xy = NULL, colNames = TRUE, rowNames = FALSE, tableStyle = "TableStyleMedium2", tableName = NULL, headerStyle = NULL, withFilter = TRUE, keepNA = FALSE, na.string = NULL, sep = ", ", stack = FALSE, firstColumn = FALSE, lastColumn = FALSE, bandedRows = TRUE, bandedCols = FALSE)
  
  saveWorkbook(wbksfa, file_path2, overwrite = TRUE) 
  

  
  
  
  
  
  
  
  
  
  
  
  
  
