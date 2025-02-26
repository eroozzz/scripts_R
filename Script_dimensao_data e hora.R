# Librarys (bibliotecas)
library(data.table)
library(RMySQL)
library(dplyr)
library(lubridate)
library(chron)
library(svMisc)
library(readxl)
library(stringr)
library(tidyr)
library(reshape2)
library(bit64)
library(hms)
library(stringr)
library(purrr)
library(tidyr)
library(taskscheduleR)
library(readxl)

# Atribui��o de diret�rio dos arquivos de sa�da
setwd("C:/caminho")

#----------------------------------------------------------------------------------------------------------------------------
# Carregamento da base de dados do Lookup
#----------------------------------------------------------------------------------------------------------------------------

# Carregamento dos arquivos no diret�rio
filelist = list.files(pattern = ".*.csv") 

# Fun��o para importa��o dos arquivos
datalist = lapply(filelist, function(x)fread(x, header=T, encoding = "Latin-1"))

# Empilhamento dos arquivos
BASE = do.call("rbind", datalist)
rm(datalist, filelist)
gc()

# Tratamento da data
BASE$data <- dmy(BASE$data)
BASE$data <- as.Date(format(BASE$data,"%Y-%m-%d"))

# Dimes�o para an�lise de quantidade de grupos
DIMENSAO <- distinct(BASE) %>%
  group_by(grupo) %>% 
  summarise(QTDE = n())

# Quantidade de linhas da tabela dimens�o
QTDE_LINHAS <- nrow(DIMENSAO)

# Quatidade de horas
QTDE_HORAS <- 24

# Quantidade final de linhas da tabela final
QTDE_LINHAS <- QTDE_LINHAS*QTDE_HORAS

# Cria��o das vari�veis para c�lculo da quantidade de linhas da dimens�o da data
DATA_MIN <- min(BASE$data)
DATA_MAX <- max(BASE$data)
DIF_DATA <- as.numeric(DATA_MAX-DATA_MIN)+1

# Cria��o da tabela de dimens�o de grupos
GRUPOS <- as.data.frame(lapply(DIMENSAO, rep, QTDE_LINHAS*DIF_DATA))
GRUPOS$QTDE <- NULL

# Ordena��o da tabela de grupos para cria��o da coluna de id
GRUPOS <- GRUPOS[order(GRUPOS$grupo, decreasing = FALSE),]
GRUPOS <- as.data.table(GRUPOS)
GRUPOS$ID <- seq.int(QTDE_LINHAS*DIF_DATA)

# Cria��o da tabela com todos os dias existentes da dimens�o de data
#DIMENSAO_DATA <- DIMENSAO[rep(seq_len(QTDE_LINHAS), each=DIF_DATA),]
DATAS <- as.data.frame(replicate(QTDE_LINHAS,seq(as.Date(DATA_MIN), as.Date(DATA_MAX),1)))
DATAS <- stack(DATAS) # faz o empilhamento das colunas em linhas !
DATAS$ind <- NULL
DATAS$values <- as_date(DATAS$values)

# Ordena��o da tabela de datas para cria��o da coluna de id
DATAS <- DATAS[order(DATAS$values, decreasing = FALSE),]
DATAS <- as.data.table(DATAS)
DATAS$ID <- seq.int(QTDE_LINHAS*DIF_DATA)

# Cria��o da tabela com todos os hor�rios da dimens�o de data
HORAS <- as.data.frame(replicate(DIF_DATA,seq(0, 23,1)))
HORAS <- stack(HORAS) # faz o empilhamento das colunas em linhas !
HORAS$ind <- NULL
HORAS <- rename(HORAS, "hora" = "values")
HORAS <- as.data.table(HORAS)

# cria��o do ID do hor�rio, n�o deve ser ordenado
HORAS$ID <- seq.int(QTDE_LINHAS*DIF_DATA)

# Concatena��o das dimens�es de grupo, data e hor�rio
BASE_FINAL <- left_join(GRUPOS,DATAS, by = "ID")
BASE_FINAL <- left_join(BASE_FINAL,HORAS, by = "ID")
BASE_FINAL$ID <- NULL
BASE_FINAL <- rename(BASE_FINAL,"grupo" = "GRUPOS")
BASE_FINAL <- rename(BASE_FINAL,"data"="DATAS")

# Finaliza��o do processo de cria��o das colunas vazias
BASE_FINAL <- left_join(BASE_FINAL,BASE, 
                        by= c ("grupo"="grupo",
                               "data"="data",
                               "hora"="hora"))
# Substitui��o do valor NA para 0
BASE_FINAL$total_tempo2 <- ifelse(is.na(BASE_FINAL$total_tempo),0, BASE_FINAL$total_tempo)
BASE_FINAL$total_tempo <- BASE_FINAL$total_tempo2
BASE_FINAL$total_tempo2 <- NULL

