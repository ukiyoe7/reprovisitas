library(DBI)
library(tidyverse)
library(googlesheets4)


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

## AJUSTE STEOR 3


PREF_RS <- PREF_RS[1:45,1] %>% mutate(CNPJ2=substr(CNPJ,nchar(CNPJ)-10,nchar(CNPJ)))

SFA_SETOR3_AJUSTADO <- union_all(
                           SFA_SETOR3 %>%  filter(ESTADO !='RS'),
  
                             SFA_SETOR3 %>%
                               mutate(CNPJ2=substr(CNPJ,nchar(CNPJ)-10,nchar(CNPJ))) %>%  
                                left_join(.,PREF_RS,by="CNPJ2") %>% 
                                  filter(ESTADO=='RS' &  !is.na(CNPJ.y))  %>% .[,c(-15,-16)])

View(SFA_SETOR3_AJUSTADO)



                
