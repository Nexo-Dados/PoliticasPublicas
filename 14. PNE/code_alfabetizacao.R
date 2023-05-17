setwd("1151. PP Dados Alfabetizacao")

library(tidyverse)
library(readxl)
library(sf)
library(RColorBrewer)

alfabetizacao1 <- read_excel("Alfabetização.xlsx", sheet="Alfabetizacao")

alfabetizacao2 <- read_excel("Alfabetização.xlsx" , sheet="Alfabetizacao_UF", skip=1) |> 
  select(-`2021-2012`)

analfabetismo_funcional1 <- read_excel("Alfabetização.xlsx", sheet="Analfabetismo_funcional")

analfabetismo_funcional2 <- read_excel("Alfabetização.xlsx", sheet="Analfabfuncional_UF", skip=1) |> 
  select(-`2012-2021`)


# Mapas --------------------------------------------------------------------

alfab_por_estado <- nexo.utils::infoState |> 
  select(uf, state) |> 
  left_join(alfabetizacao2, by=c("state" = "Territorio")) |> 
  select(1,2,4, "2021") |> 
  mutate(Diferenca = `2021` - `2012`)

nexo.utils::mapState |> 
  left_join(alfab_por_estado, by=c("uf")) |> 
  pivot_longer(`2012`:`2021`, names_to="Ano", values_to="Alfabetizados") |> 
  mutate("Taxa de alfabetização" = cut(Alfabetizados, breaks=c(.8, .85,.9,.95,1),
                              labels=c("Menos de 85%", "De 85 a 90%", "De 90 a 95%", "Mais de 95%")),
         Diferenca = Diferenca*100) |> 
  ggplot()+
  geom_sf(aes(fill=`Taxa de alfabetização`))+
  geom_sf_text(aes(label=round(Alfabetizados*100, 1)))+
  scale_fill_brewer(palette="Purples")+
  facet_wrap(~Ano)

nexo.utils::plot.export(title = "Taxa de alfabetização",
                        subtitle = "Entre maiores de 15 anos, por UF",
                        id = 1, project = 1151, map=T)


funcional_por_estado <- nexo.utils::infoState |> 
  select(uf, state) |> 
  left_join(analfabetismo_funcional2, by=c("state" = "Territorio")) |> 
  select(1,2,4, "2021") |> 
  mutate(Diferenca = `2021` - `2012`)

nexo.utils::mapState |> 
  left_join(funcional_por_estado, by=c("uf")) |> 
  pivot_longer(`2012`:`2021`, names_to="Ano", values_to="Analf_funcional") |> 
  mutate("Analfabetismo funcional" = cut(Analf_funcional, breaks=c(0, .1,.15,.2, .25, 1),
                                       labels=c("Até 10%", "Entre 10 e 15%", "Entre 15 e 20%", "Entre 20 e 25%", "Mais de 25%")),
         Diferenca = Diferenca*100) |> 
  ggplot()+
  geom_sf(aes(fill=`Analfabetismo funcional`))+
  geom_sf_text(aes(label=round(Analf_funcional*100, 1)))+
  scale_fill_brewer(palette="PuRd")+
  facet_wrap(~Ano)

nexo.utils::plot.export(title = "Taxa de analfabetismo funcional",
                        subtitle = "Entre maiores de 15 anos, por UF",
                        id = 2, project = 1151, map=T)


# Dados por região -----------------------------------------------------

alfabetizacao1 |> 
  filter(Categoria == "Regiao") |> 
  ggplot(aes(x=reorder(Tipo, `2021`)))+
  geom_col(aes(y=`2021`), fill="orange")+
  geom_col(aes(y=`2012`), fill="purple")+
  labs(x="Região", y="Taxa de alfabetização")+
  geom_text(aes(y=`2021`, label=format(`2021`, decimal.mark=",")))+
  geom_text(aes(y=`2012`, label=format(`2012`, decimal.mark=",")), col="white")
  
nexo.utils::plot.export(title = "Taxa de alfabetização",
                        subtitle = "Entre maiores de 15 anos, por região",
                        id = 3, project = 1151)

analfabetismo_funcional2 |> 
  filter(Categoria == "Regiao" | Categoria == "Pais") |> 
  mutate(`2012` = `2012` * 100,
          `2021` = `2021` * 100) |> 
  ggplot(aes(x=reorder(Territorio, `2012`)))+
  geom_col(aes(y=`2012`), fill="purple")+
  geom_col(aes(y=`2021`), fill="orange")+
  labs(x="Região", y="Taxa de analfabetismo funcional")+
  geom_text(aes(y=`2021`, label=format(`2021`, decimal.mark=",")))+
  geom_text(aes(y=`2012`, label=format(`2012`, decimal.mark=",")))+
  geom_hline(yintercept = 9.8)

nexo.utils::plot.export(title = "Taxa de analfabetismo funcional",
                        subtitle = "Entre maiores de 15 anos, por região",
                        id = 4, project = 1151)

# Linhas --------------------------------------------------------------------

alfabetizacao1 |> 
  filter(Categoria != "Regiao") |> 
  pivot_longer(`2012`:`2021`, names_to="Ano", values_to="Percentual") |> 
  mutate(Ano = as.integer(Ano)) |> 
  ggplot(aes(x=Ano))+
  geom_line(aes(y=Percentual, col=Tipo))+
  ylim(79.5,100) +
  facet_wrap(~Categoria)

nexo.utils::plot.export(title = "Taxa de alfabetização",
                        subtitle = "Entre maiores de 15 anos, entre 2012 e 2021",
                        id = 5, project = 1151)

analfabetismo_funcional1 |> 
  filter(Categoria != "Regiao") |> 
  pivot_longer(`2012`:`2021`, names_to="Ano", values_to="Percentual") |> 
  mutate(Ano = as.integer(Ano)) |> 
  ggplot(aes(x=Ano))+
  geom_line(aes(y=Percentual, col=Tipo))+
  ylim(0,40) +
  facet_wrap(~Categoria)

nexo.utils::plot.export(title = "Taxa de analfabetismo funcional",
                        subtitle = "Entre maiores de 15 anos, entre 2012 e 2021",
                        id = 6, project = 1151)


# Renda -------------------------------------------------------------------

alfabetizacao1 |> 
  filter(Categoria== "Renda") |> 
  ggplot(aes(y=Tipo))+
  geom_col(aes(x=`2019`))+
  geom_text(aes(x=`2019`-10, label=format(`2019`, decimal.mark=",", digits=3)))

nexo.utils::plot.export(title = "Taxa de alfabetização",
                        subtitle = "Entre maiores de 15 anos, em 2019",
                        id = 6, project = 1151)

