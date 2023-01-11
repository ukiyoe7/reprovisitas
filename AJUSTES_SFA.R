library(googlesheets4)
library(DBI)
library(tidyverse)



left_join(BASE_SFA_301122,cli2,by="CNPJ") %>% select(CNPJ,SETOR,COD_REPRO) %>% View()


SFA_S1 <- read_sheet("1GR3MkzdnW8jCYk5tIzLS2hk0HoJrMI5j3YXXJu_ciPg",sheet = "SFA")

View(SFA_S1)

SFA_ESP1 <- read_sheet("1GR3MkzdnW8jCYk5tIzLS2hk0HoJrMI5j3YXXJu_ciPg",sheet = "SFA")

View(SFA_S1)