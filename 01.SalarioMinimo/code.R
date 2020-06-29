
# Library -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

# Data --------------------------------------------------------------------

tabela <- map(excel_sheets("tabela.xlsx"), function(x) read_excel("tabela.xlsx", x))


# Charts ------------------------------------------------------------------

tabela[[1]] %>% 
  mutate(data = ymd(paste0(periodo, ".1"))) %>% 
  ggplot(aes(x=data, y=sm)) +
  geom_step() +
  scale_y_continuous(labels=scales::dollar_format(prefix = "R$"))+
  scale_x_date(date_breaks = "2 year", date_labels = "%y")

ext.functions::extplot('salario minimo real ao longo do tempo', 1, 1)

tabela[[2]] %>% 
  mutate(data = ymd(paste0(periodo, ".1"))) %>% 
  mutate(diff = (sm/lag(sm))-1) %>% 
  ggplot(aes(x=data, y=diff)) +
  geom_col() +
  scale_y_continuous(labels=scales::percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y")

ext.functions::extplot('valorizacao real do salario minimo', 2, 1)

tabela[[3]] %>% 
  mutate(outros = total-sm1) %>% 
  gather(var, val, sm1, outros) %>% 
  ggplot(aes(x=ano, y=val, fill=var)) +
  geom_col() +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks=c(1992:2019)) +
  coord_flip()


ext.functions::extplot('pessoas que recebem aposentadoria', 3, 1)


#-- ?
tabela[[3]] %>% 
  mutate(outros = total-sm1) %>% 
  gather(var, val, sm1, outros) %>% 
  ggplot(aes(x=ano, y=val, fill=var)) +
  geom_col(position="fill") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1992:2019)) +
  coord_flip()

ext.functions::extplot('pessoas que recebem aposentadoria- %', 4, 1)




