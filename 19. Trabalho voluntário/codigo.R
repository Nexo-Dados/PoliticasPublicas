
# Bibliotecas -------------------------------------------------------------

library(PNADcIBGE)
library(srvyr)
library(tidyverse)
library(readxl)
library(janitor)

# Download dados ----------------------------------------------------------

#-- ATENCAO ARQUIVO PESADO
#-- baixar dados
url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Dados/PNADC_2022_visita5_20231222.zip"

options(timeout=6000)

download.file(url, destfile="PNADC_2022_visita5_20231222.zip")

#-- unzip
unzip("PNADC_2022_visita5_20231222.zip", exdir = ".")

#-- dicionario de dados
url_dic <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_5/Documentacao/dicionario_PNADC_microdados_2022_visita5_20231220.xls"

download.file(url_dic, destfile="dicionario_PNADC_microdados_2022_visita5_20231220.xls")

dic <- read_excel("dicionario_PNADC_microdados_2022_visita5_20231220.xls", 
                  sheet = "dicionário pnad contínua ", 
                  skip = 3) %>% 
  clean_names() %>% 
  select(1,2,3) %>% 
  set_names("start", "length", "variavel") %>% 
  mutate_at(vars(1,2), as.numeric) %>% 
  mutate(end = start+length-1) %>% 
  drop_na()

#-- ler pnad
pnad_2022 <- read_fwf("PNADC_2022_visita5.txt", 
                  fwf_positions(start = dic$start, end = dic$end, col_names = dic$variavel))


# Analise survey ----------------------------------------------------------

dados <- pnad_2022 %>%  
  mutate(V1032 = as.numeric(V1032),
         V4112 = as.numeric(V4112),
         idade = as.numeric(V2009)) %>% 
  select(UPA, Estrato, V1032, idade, V4111, V4111A, V4112, V41111:V41116) %>% 
  as_survey_design(
    ids    = UPA,
    strata = Estrato,
    weights= V1032,
    nest   = TRUE
  )


#--- contagem de trabalho voluntario / sim nao
contagem0 <- dados %>%
  group_by(V4111) %>%
  summarise(total = survey_total(vartype = c("se", "ci")))

#-- media nacional 
contagem0 %>% 
  drop_na(V4111) %>% 
  mutate(p = total/sum(total, na.rm=TRUE))

#-- trabalho voluntario por idade / sim nao
contagem1 <- dados %>%
  mutate(idade = ifelse(idade >= 75, 75, idade)) %>% 
  group_by(idade, V4111) %>%
  summarise(total = survey_total(vartype = c("se", "ci")))

#-- grafico 1 - trabalho voluntario por idade
contagem1 %>% 
  drop_na(V4111) %>% 
  group_by(idade) %>% 
  mutate(p = total/sum(total, na.rm=TRUE)) %>% 
  filter(V4111 == 1) %>% 
  ggplot(aes(x=idade, y=p)) +
  geom_smooth(se=FALSE) +
  geom_point() +
  geom_hline(yintercept=.042) +
  scale_y_continuous(labels=scales::percent)

#-- media nacional de trabalho voluntario
dados %>%
  filter(V4111 ==1) %>% 
  summarise(media = survey_mean(V4112, vartype = c("se", "ci"), na.rm = TRUE))

#-- media por idade 
media1 <- dados %>%
  filter(V4111 ==1) %>% 
  mutate(idade = ifelse(idade >= 75, 75, idade)) %>% 
  group_by(idade) %>%
  summarise(
    media = survey_mean(V4112, vartype = c("se", "ci"), na.rm = TRUE)
  )

#-- grafico da media por idade
media1 %>% 
  ggplot(aes(x=idade, y=media)) +
  geom_smooth(se=FALSE) +
  geom_point() +
  geom_hline(yintercept=6.6) +
  scale_y_continuous(labels=scales::comma)

#-- limpar arquivos grandes
file.remove("PNADC_2022_visita5.txt")
file.remove("PNADC_2022_visita5_20231222.zip")




