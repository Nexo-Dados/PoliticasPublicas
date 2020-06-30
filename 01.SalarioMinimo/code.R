## 01. Salário Mínimo 
## Autor: Gabriel Zanlorenssi, Nexo Jornal
## Contato: dados@nexojornal.com.br

# Library -----------------------------------------------------------------

## bibliotecas utilizadas
library(tidyverse)
library(readxl)
library(lubridate)
## pacote com funcoes do nexo
library(nexo.utils)
## caso nao tenha instalado
## devtools::install_github('Nexo-Dados/nexo.utils')

# Dados -------------------------------------------------------------------

## Ler cada aba como um elemento da lista
tabela <- map(excel_sheets("tabela.xlsx"), function(x) read_excel("tabela.xlsx", x))


# Graficos ----------------------------------------------------------------

#-- valorizacao real do salario minimo ao longo do tempo
tabela[[1]] %>% 
  mutate(data = ymd(paste0(periodo, ".1"))) %>% 
  ggplot(aes(x=data, y=sm)) +
  geom_step() +
  scale_y_continuous(labels=scales::dollar_format(prefix = "R$"))+
  scale_x_date(date_breaks = "2 year", date_labels = "%y")

plot.export('salario minimo real ao longo do tempo', 1, 1)

#-- valorizacao real do salario minimo ao longo do tempo
#-- diferença %: valor hoje / valor de ontem
tabela[[2]] %>% 
  mutate(data = ymd(paste0(periodo, ".1"))) %>% 
  mutate(diff = (sm/lag(sm))-1) %>% 
  ggplot(aes(x=data, y=diff)) +
  geom_col() +
  scale_y_continuous(labels=scales::percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y")

plot.export('valorizacao real do salario minimo', 2, 1)

#-- em valores totais, número de pessoas que recebem aposentadoria e categoria
tabela[[3]] %>% 
  mutate(outros = total-sm1) %>% 
  gather(var, val, sm1, outros) %>% 
  ggplot(aes(x=ano, y=val, fill=var)) +
  geom_col() +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks=c(1992:2019)) +
  coord_flip()


plot.export('pessoas que recebem aposentadoria', 3, 1)


#-- em percentual, número de pessoas que recebem aposentadoria e categoria
tabela[[3]] %>% 
  mutate(outros = total-sm1) %>% 
  gather(var, val, sm1, outros) %>% 
  ggplot(aes(x=ano, y=val, fill=var)) +
  geom_col(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1992:2019)) +
  coord_flip()

plot.export('pessoas que recebem aposentadoria- %', 4, 1)




