#-- library
library(readxl)
library(tidyverse)
library(lubridate)

#-- graficos
grf1  <- read_excel("Desig_Racial.xlsx", 
                           sheet = "GRF1", col_types = c("text", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric"))

grf2 <- read_excel("Desig_Racial.xlsx", sheet="GRF2")

#-- imagens
grf1 %>% 
  rename(trimestre = 1) %>% 
  mutate(trimestre = dmy(paste(1,
              as.numeric(str_sub(trimestre, 6))*3,
              str_sub(trimestre,1,4), sep="/"))) %>% 
  ggplot(aes(x=trimestre)) +
  geom_line(aes(y=Branco), color="orange") +
  geom_line(aes(y=Branco_Movel), color="red") +
  geom_line(aes(y=Negro), color="blue") +
  geom_line(aes(y=Negro_Movel), color="purple") +
  scale_y_continuous(limits=c(0,2500))

ggsave("grafico1.pdf",height=6,width=8)

grf1 %>% 
  rename(trimestre = 1) %>% 
  mutate(trimestre = dmy(paste(1,
                               as.numeric(str_sub(trimestre, 6))*3,
                               str_sub(trimestre,1,4), sep="/"))) %>% 
  ggplot(aes(x=trimestre)) +
  geom_line(aes(y=Desigualdade), color="green") +
  geom_line(aes(y=Desigualdade_Movel), color="darkgreen") +
  scale_y_continuous(limits=c(0.5,1),
                     labels=scales::percent)

ggsave("grafico2.pdf",height=6,width=8)

#--- renda
grf2 %>% 
  ggplot(aes(x=reorder(Origem, Percentual),
             y=Percentual)) +
  geom_col() +
  geom_text(aes(label=paste0(round(Percentual*100,1), "%"))) +
  coord_flip() +
  scale_y_continuous(labels=scales::percent)

ggsave("grafico3.pdf",height=6,width=8)

  