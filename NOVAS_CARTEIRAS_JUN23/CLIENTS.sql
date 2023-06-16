SELECT DISTINCT C.CLICODIGO,
                  CLINOMEFANT,
                   CLIRAZSOCIAL,                
                      CLICNPJCPF,
                       CLIINSCEST,
                                    REPLACE(REPLACE(REPLACE(CLICNPJCPF,'.',''),'/',''),'-','') CNPJ,
                                     ENDCODIGO,
                                      CIDNOME,
                                      ENDBAIRRO,
                                       C.GCLCODIGO,
                                        CLIPCDESCPRODU,
                                         CLIDTCAD,
                                          GCLNOME,  
                                           IIF(C.GCLCODIGO IS NULL,C.CLICODIGO || ' ' || CLINOMEFANT,'G' || C.GCLCODIGO || ' ' || GCLNOME) CLIENTE,
                                            SETOR
                                             FROM CLIEN C
                                              INNER JOIN (SELECT CLICODIGO,E.ZOCODIGO,CIDNOME,ENDBAIRRO,ZODESCRICAO SETOR,ENDCODIGO FROM ENDCLI E
                                               LEFT JOIN CIDADE CID ON E.CIDCODIGO=CID.CIDCODIGO
                                                LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,26,27,28))Z ON 
                                                 E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
                                                  LEFT JOIN GRUPOCLI GR ON C.GCLCODIGO=GR.GCLCODIGO
                                                   WHERE CLICLIENTE='S'