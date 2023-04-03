setwd("1148. PP Dados Confessionalidade")

library(tidyverse)

deputados_eleitos <- readxl::read_excel("deputados_eleitos.xlsx")


deputados_eleitos |> 
  group_by(Confessionalidade) |> 
  summarise(n=n()) |> 
  ggplot(aes(y=reorder(Confessionalidade,n)))+
  geom_col(aes(x=n))+
  geom_text(aes(x=-10, label=n))+
  labs(x="Número de deputados", y="Confessionalidade")

waffle::waffle(c("Católica (233)" = 233, "Cristã (83)" = 83, "Evangélica (76)" = 76, "Espírita (3)"=3, "Afrorreligioso (3)"=3,
                 "Espiritualidades indígenas (1)" = 1,"Sem religião (12)" = 12, "Não identificada (102)" = 102), rows=19)


nexo.utils::plot.export(title = "Confessionalidade dos deputados federais",
                        subtitle = "Eleitos em 2022",
                        id = 1, project = 1148)


# Conversa com pesquisadores ---------------------------------------------------------


deputados_eleitos |> 
  filter(Vinculação == "Igreja Católica") |> 
  group_by(Vinculação, Confessionalidade) |> 
  count()

deputados_eleitos |> 
  filter(Vinculação == "Igreja Evangélica não identificada" |
           (Confessionalidade == "Evangélica" & Vinculação == "Não identificada") |
           (Confessionalidade == "Evangélica" & Vinculação == "Sem denominação"))|> 
  group_by(Vinculação, Confessionalidade) |> 
  count()

deputados_eleitos |> 
  filter(str_detect(Vinculação, "Assembleia de Deus"))|> 
  group_by(Vinculação, Confessionalidade) |> 
  count()

deputados_eleitos |> 
  filter(str_detect(Vinculação, "Batista"))|> 
  group_by(Vinculação, Confessionalidade) |> 
  count()

deputados_eleitos |> 
  filter(str_detect(Vinculação, "Universal"))|> 
  group_by(Vinculação, Confessionalidade) |> 
  count()

# Treemap geral -----------------------------------------------------------

dados3 <- readxl::read_xlsx("dados3.xlsx")

dados3 |> 
  group_by(Categoria2, Categoria3, Categoria4) |> 
  summarise(n= sum(n)) |> 
  ggplot(aes(area=n, subgroup=Categoria4, subgroup2=Categoria3, subgroup3=Categoria2, fill=Categoria3))+
  treemapify::geom_treemap()+
  treemapify::geom_treemap_subgroup_border(colour = "white", size = 5)+
  treemapify::geom_treemap_subgroup2_border(colour = "white", size = 3)+
  treemapify::geom_treemap_subgroup3_border(colour = "white", size = 1)+
  treemapify::geom_treemap_text(aes(label=paste0(Categoria2, "\n", n)))

nexo.utils::plot.export(title = "Vinculação religiosa dos deputados federais",
                        subtitle = "Eleitos em 2022",
                        id = 2, project = 1148)

# Cristãos ----------------------------------------------------------------

cristaos <- deputados_eleitos |> 
  filter(Confessionalidade == "Cristã")

cristaos |> 
  group_by(sigla_partido, ideologia) |> 
  summarise(n=n()) |> 
  ggplot(aes(y=reorder(sigla_partido,n)))+
  geom_col(aes(x=n, fill=ideologia))

#writexl::write_xlsx(cristaos |> 
 #                     group_by(sigla_partido, ideologia) |> 
  #                    summarise(n=n()), "cristaos-partido.xlsx")


# Raça cor ----------------------------------------------------------------

deputados_eleitos |> 
  group_by(raca, Confessionalidade) |> 
  summarise(n=n()) |> ungroup() |> 
  filter(raca != "Não informado") |> 
  mutate(raca = factor(raca, levels=c("Branca", "Amarela", "Indígena", "Parda", "Preta")),
         Confessionalidade = factor(Confessionalidade, levels=c("Católica", "Evangélica", "Cristã", "Espírita",
                                                                "Afrorreligioso", "Espiritualidades Indígenas", "Sem Religião", "Não identificada")),
         Confessionalidade = fct_rev(Confessionalidade)) |>
  ggplot(aes(y=reorder(raca, desc(raca))))+
  geom_col(aes(x=n, fill=Confessionalidade), position="fill")+
  labs(x="Percentual", y="Raça/cor")

nexo.utils::plot.export(title = "Confessionalidade dos deputados federais",
                        subtitle = "Eleitos em 2022, por raça/cor",
                        id = 3, project = 1148)

deputados_eleitos |> 
  group_by(ideologia, Confessionalidade) |> 
  summarise(n=n()) |> ungroup() |> 
  mutate(Confessionalidade = factor(Confessionalidade, levels=c("Católica", "Evangélica", "Cristã", "Espírita",
                                                                "Afrorreligioso", "Espiritualidades Indígenas", "Sem Religião", "Não identificada")),
         ideologia = factor(ideologia, c("Direita", "Centro", "Esquerda")),
         Confessionalidade = fct_rev(Confessionalidade)) |>
  ggplot(aes(y=reorder(ideologia, desc(ideologia))))+
  geom_col(aes(x=n, fill=Confessionalidade), position="fill")+
  labs(x="Percentual", y="Ideologia política")

nexo.utils::plot.export(title = "Confessionalidade dos deputados federais",
                        subtitle = "Eleitos em 2022, por ideologia",
                        id = 5, project = 1148)


# Outro partidos ----------------------------------------------------------

deputados_eleitos |> 
  group_by(Confessionalidade, sigla_partido) |> 
  summarise(n = n()) |> ungroup() |> 
  pivot_wider(names_from=Confessionalidade, values_from=n, values_fill=0) |> 
  mutate(Ordem = Católica) |> 
  pivot_longer(Afrorreligioso:`Sem Religião`, names_to="Confessionalidade", values_to="n") |> 
  mutate(Confessionalidade = factor(Confessionalidade, levels=c("Católica", "Evangélica", "Cristã", "Espírita",
                                                                "Afrorreligioso", "Espiritualidades Indígenas", "Sem Religião", "Não identificada")),
         Confessionalidade = fct_rev(Confessionalidade)) |> 
  ggplot(aes(y=reorder(sigla_partido, n)))+
  geom_col(aes(x=n, fill=Confessionalidade))+
  labs(x="Deputados eleitos", y="Partido")

nexo.utils::plot.export(title = "Confessionalidade dos deputados federais",
                        subtitle = "Eleitos em 2022, por partido",
                        id = 4, project = 1148)

deputados_eleitos |> 
  group_by(Confessionalidade, sigla_partido) |> 
  summarise(n = n()) |> ungroup() |> 
  group_by(sigla_partido) |> 
  mutate(percentual = n*100/sum(n)) |> ungroup() |> 
  filter(Confessionalidade == "Evangélica") |> 
  arrange(desc(percentual))
