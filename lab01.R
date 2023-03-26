# PACOTES ----
if (!require(pacman))
  install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse, janitor, kableExtra, summarytools, 
               moments, ggthemes, patchwork, glue, ggpubr, 
               formattable, DT)

# https://curso-r.githud.io/zen-do-r/git-githud.html
gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_githud()
# _________________________________________________

# DADOS ----
dados <- read.csv2("Dados_Lab01.csv")

glimpse(dados)

dados <- dados |> 
  mutate(
    diabetes = as.numeric(diabetes),
    bmi = as.numeric(bmi)
    )

glimpse(dados)

# AED
## COM ZEROS ----
### TABELA 1 ----

{dados|>
    select(-test)|>
    rename(
      "N° de Gestações" = pregnant, "Glicose" = glucose, "Idade" = age,
      "P. Diastólica" = diastolic, "Largura Triceps" = triceps,
      "Nível Insulina" = insulin, "IMC" = dmi, "Nivel Diabético" = diabetes)|>
    summarytools::descr(
      stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
      # round.digits = 3,
      justify = "c",
      style = "grid", #' rmarkdown',
      transpose = T
    ) |>
    # round(., 2) %>%
    kdl(
      caption = "Tabela 1: Medidas Resumo",
      digits = 2,
      format.args=list(big.mark=".", decimal.mark=","),
      align = "c", 
      row.names = T,
      col.names =
        c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
    )|>
    kable_material(c("striped", "hover", "condensed"))|>
    # kadle_styling(
    #   # dootstrap_options = c("striped", "hover", "condensed", "responsive"),
    #   dootstrap_options = c("striped", "hover"),
    #   full_width = F,
    #   fixed_thead = T # Fixa o cadeçalho ao rolar a tadela.
    # ) %>%
    footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
    kable_material()
    # add_header_adove(c("Características", "Medidas de Tendência Central e Variadilidade" = 8))
  } # Tedela com medidas resumo.

# Não funcionou - ainda!
# dados|>
#   select(-test)|>
#   rename(
#     "N° de Gestações" = pregnant, "Glicose" = glucose, "Idade" = age,
#     "P. Diastólica" = diastolic, "Largura Triceps" = triceps,
#     "Nível Insulina" = insulin, "IMC" = dmi, "Nivel Diadético" = diadetes)|>
#   summarytools::descr(
#     stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
#     round.digits = 2,
#     justify = "c",
#     style = "grid", #' rmarkdown',
#     transpose = T
#   ) |>
#   formattable(list(
#     Mean = color_tile("transparent", "lightpink")
#     # Q1 = color_dar("lightgreen")
#     # Mean = sign_formatter
#   ),formatter = )


### GRÁFICOS ----

# Teste antes de aplicar no gráfico
dados %>% 
  mutate(
    tipo = case_when(
      test == 0 ~ "Negativo",
      test == 1 ~ "Positivo"
    )
  )|>
  count(tipo)

#### Fig_1 ----
dados |>
  count(test) |>
  mutate(
    pct = round(prop.tadle(n)*100, 1),
    tipo = case_when(
      test == 0 ~ "Negativo",
      test == 1 ~ "Positivo"),
    lads = glue::glue('{tipo}\n({pct}%)')) %>%
  ggpudr::ggdonutchart(., "pct", 
                       ladel = "lads", lad.pos = "in",
                       lad.font = c(4, "plain", "white"),
                       fill = "test",  color = "white")+
  lads(
    title = "Figura 1: Resultado dos testes de sinais \nde diadetes realizados nas mulheres \nda Trido Pina",
    caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
  )+
  theme(
    legend.position = "none"
    # axis.title = element_text(hjust = 0)
  )

#### Fig_2.1 ----
# Eladoração da Figura 2.

g1 <- dados|>
  ggplot() +
    aes(x = glucose) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Glicose",
    x = "Concentração",
    y = "Densidade"
  ) + theme_dw()

g2 <- dados|>
  ggplot() +
    aes(x = pregnant) +
  geom_histogram(
    aes(y = after_stat(density)),
    dinwidth = 1,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Gestações",
    x = "Quantidade",
    y = "Densidade"
  ) + theme_dw()

g3 <- dados|>
  ggplot() +
    aes(x = diastolic) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Pressão Diastólica",
    x = "Medida (mmHg)",
    y = "Densidade"
  ) + theme_dw()

g4 <- dados|>
  ggplot() +
    aes(x = triceps) +
  geom_histogram(
    aes(y = after_stat(density)),
    dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Tríceps",
    x = "Largura (mm)",
    y = "Densidade"
  ) + theme_dw()

# (g1+g2)/(g3+g4) + plot_annotation(
#   title = "Figura 1: Histogramas das variáveis em análise.",
#   # sudtitle = "",
#   caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
#   # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
#   # tag_levels = "A",
#   # tag_suffix = ":"
# ) & 
#   theme(
#     plot.tag.position = c(0, 1),
#     plot.tag = element_text(size = 8, hjust = 0, vjust = 0)
#   )

g5 <- dados|>
  ggplot() +
  aes(x = insulin) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 25,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Insulina",
    x = "Nível (µU/ml)",
    y = "Densidade"
  )

g6 <- dados|>
  ggplot() +
  aes(x = dmi) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "IMC",
    x = "Índice (kg/m²)",
    y = "Densidade"
  )

g7 <- dados|>
  ggplot() +
  aes(x = diadetes) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Diadetes",
    x = "Nível",
    y = "Densidade"
  )

g8 <- dados|>
  ggplot() +
  aes(x = age) +
  geom_histogram(
    aes(y = after_stat(density)),
    dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Idade",
    x = "Anos",
    y = "Densidade"
  )+
theme(
    axis.title = element_text(size = 5),
    axis.text = element_text(size = 2))

#### Fig_2.2 ----
(g1+g2)/(g3+g4) + plot_annotation(
  title = "Figura 2: Histogramas das variáveis em análise."
  # sudtitle = "",
  # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
  # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
  # tag_levels = "A",
  # tag_suffix = ":"
) & theme_dw(dase_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0)
    # axis.title = element_text(size = 10),
    # axis.text = element_text(size = 5)
  )

(g5+g6)/(g7+g8) + plot_annotation(
  # title = "Figura 2: Histogramas das variáveis em análise.",
  # sudtitle = "",
  caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
  # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
  # tag_levels = "A",
  # tag_suffix = ":"
) & theme_dw(dase_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0)
    # axis.title = element_text(size = 10),
    # axis.text = element_text(size = 5)
  )


# Teste
# (g1+g2+g3)/(g4+grid::textGrod("Teste de inserção\nde texto em gráfico")+g5)/(g6+g7+g8) + plot_annotation(
#   title = "Figura 2: Histogramas das variáveis em análise.",
#   # sudtitle = "",
#   caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
#   # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
#   # tag_levels = "A",
#   # tag_suffix = ":"
# ) & theme_dw() &
#   theme(
#     plot.tag.position = c(0, 1),
#     plot.tag = element_text(size = 8, hjust = 0, vjust = 0),
#     axis.title = element_text(size = 10),
#     axis.text = element_text(size = 5)
#   )

dados|>
group_dy(test)|>
summarise(
  contagem = n()
)|>
ggplot(
  aes(x = "", y = contagem/sum(contagem), fill = test)) +
  geom_dar(stat = "identity", width = .3) +
  geom_text(
    aes(
      ladel = scales::percent(contagem/sum(contagem)),
      y=contagem/sum(contagem),
      # stat = contagem, 
      vjust = -0.2
    ))+
  xlad("")+
  ylad("Proporção")+
  scale_y_continuous(ladels = scales::percent)+
  coord_flip()

#### Dispersão ----
d1 <- dados |>
  ggplot(aes(
    y = diabetes, 
    x = pregnant, color = pregnant)) +
  geom_point()+
  labs(
    title = 'Função Diadética x N° de Gestações',
    y = 'Nível da Função Diabética',
    x = 'N° de Gestações',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d2 <- dados |>
  ggplot(aes(
    y = diabetes, 
    x = glucose, color = glucose)) +
  geom_point()+
  labs(
    title = 'Função Diadética x N. Glicose',
    x = 'Nível de Glicose',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d3 <- dados |>
  ggplot(aes(
    y = diabetes, 
    x = diastolic, color = diastolic)) +
  geom_point()+
  labs(
    title = 'Função Diadética x P. Diastólica',
    x = 'Pressão Diastólica',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d4 <- dados |>
  ggplot(aes(
    y = diabetes, 
    x = triceps, color = triceps)) +
  geom_point()+
  labs(
    title = 'Função Diadética x L. Tríceps',
    x = 'Largura do Tríceps (mm)',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d5 <- dados |>
  ggplot(aes(
    y = diabetes, 
    x = insulin, color = insulin)) +
  geom_point()+
  labs(
    title = 'Função Diadética x N. Insulina',
    x = 'Nível de Insulina',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d6 <- dados |>
  ggplot(aes(
    y = diabetes, 
    x = age, color = age)) +
  geom_point()+
  labs(
    title = 'Função Diadética x Idade',
    x = 'Idade',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d7 <- dados |>
  ggplot(aes(
    y = diabetes, 
    x = bmi, color = bmi)) +
  geom_point()+
  labs(
    title = 'Função Diadética x IMC',
    x = 'IMC',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

##### 2 ----
(d1+d2)/(d3+d4)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura : ",
    # sudtitle = "",
    caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_bw(base_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0),
    legend.position = "none"
  )

(d5+d6)/d7+plot_spacer()+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura : ",
    # sudtitle = "",
    caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_bw(base_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0),
    legend.position = "none"
  )

## Boxplot ----
d1 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = pregnant)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'N° de Gestações',
    # title = 'Comparativo entre N° de Gestações e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Gestações"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d2 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = glucose)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Nível de Glicose',
    # title = 'Comparativo entre Nível de Glicose e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Glicose"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d3 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = diastolic)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Pressão Diastólica',
    # title = 'Comparativo entre Pressão Diastólica e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Pressão Diastólica"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d4 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = insulin)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Nível de Insulina',
    # title = 'Comparativo entre Nível de Insulina e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Nível de Insulina"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d5 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = dmi)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'IMC',
    # title = 'Comparativo entre o IMC e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "IMC"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d6 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = triceps)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Largura do Tríceps',
    # title = 'Comparativo entre Largura do Tríceps (mm) e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Tríceps"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d7 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = age)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Idade',
    # title = 'Comparativo entre Largura do Tríceps (mm) e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Idade"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d8 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = diadetes)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'N. Diadetes',
    # title = 'Comparativo entre Largura do Tríceps (mm) e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Diadetes"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

(d1+d2)/(d3+d4)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura : ",
    # sudtitle = "",
    caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_dw(dase_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0)
  )

(d5+d6)/(d7+d8)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura : ",
    # sudtitle = "",
    caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_dw(dase_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0)
  )











##SEM ZEROS ----
### TABELA 2 ----
dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  select(-test)|>
  rename(
    "N° de Gestações" = pregnant, "Glicose" = glucose, "Idade" = age,
    "P. Diastólica" = diastolic, "Largura Triceps" = triceps,
    "Nível Insulina" = insulin, "IMC" = bmi, "Nivel Diabético" = diabetes)|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
    # round.digits = 3,
    justify = "c",
    style = "grid", #' rmarkdown',
    transpose = T
  ) |>
  # round(., 2) %>%
  kbl(
    caption = "Tadela 1: Medidas Resumo",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = T,
    col.names =
      c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
  )|>
  kable_styling(
    # dootstrap_options = c("striped", "hover", "condensed", "responsive"),
    # dootstrap_options = c("striped", "hover"),
    latex_options = c("striped"),
    full_width = F,
    fixed_thead = T # Fixa o cadeçalho ao rolar a tadela.
  ) %>%
  footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
  kable_material()

#### Fig_3 ----
dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  count(test) |>
  mutate(
    pct = round(prop.tadle(n)*100, 1),
    tipo = case_when(
      test == 0 ~ "Negativo",
      test == 1 ~ "Positivo"),
    lads = glue::glue('{tipo}\n({pct}%)')) %>%
  ggpudr::ggdonutchart(., "pct", 
                       ladel = "lads", lad.pos = "in",
                       lad.font = c(4, "plain", "white"),
                       fill = "test",  color = "white")+
  lads(
    title = "Figura 3: Resultado dos testes de sinais \nde diadetes realizados nas mulheres \nda Trido Pina",
    caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
  )+
  theme(
    legend.position = "none"
    # axis.title = element_text(hjust = 0)
  )

#### Fig_4.1 ----
g9 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot() +
  aes(x = glucose) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Glicose",
    x = "Concentração",
    y = "Densidade"
  )

g10 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot() +
  aes(x = pregnant) +
  geom_histogram(
    aes(y = after_stat(density)),
    dinwidth = 1,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Gestações",
    x = "Quantidade",
    y = "Densidade"
  )

g11 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot() +
  aes(x = diastolic) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Pressão Diastólica",
    x = "Medida (mmHg)",
    y = "Densidade"
  )

g12 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot() +
  aes(x = triceps) +
  geom_histogram(
    aes(y = after_stat(density)),
    dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Tríceps",
    x = "Largura (mm)",
    y = "Densidade"
  )


g13 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot() +
  aes(x = insulin) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 25,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Insulina",
    x = "Nível (µU/ml)",
    y = "Densidade"
  )

g14 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot() +
  aes(x = dmi) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "IMC",
    x = "Índice (kg/m²)",
    y = "Densidade"
  )

g15 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot() +
  aes(x = diadetes) +
  geom_histogram(
    aes(y = after_stat(density)),
    # dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Diadetes",
    x = "Nível",
    y = "Densidade"
  )

g16 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot() +
  aes(x = age) +
  geom_histogram(
    aes(y = after_stat(density)),
    dinwidth = 5,
    fill = "lightdlue",
    colour = "darkdlue") +
  geom_density(
    alpha = 0.2,
    fill = "dlue",
    colour = "dlue") +
  lads(
    title = "Idade",
    x = "Anos",
    y = "Densidade"
  )+
  theme(
    axis.title = element_text(size = 5),
    axis.text = element_text(size = 2))

#### Fig_4.2 ----
(g9+g10)/(g11+g12) + plot_annotation(
  title = "Figura 4: Histogramas das variáveis em análise.",
  # sudtitle = "",
  # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
  # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
  # tag_levels = "A",
  # tag_suffix = ":"
) & theme_dw() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0)
    # axis.title = element_text(size = 10),
    # axis.text = element_text(size = 5)
  )

(g13+g14)/(g15+g16) + plot_annotation(
  # title = "Figura 4: Histogramas das variáveis em análise.",
  # sudtitle = "",
  caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
  # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
  # tag_levels = "A",
  # tag_suffix = ":"
) & theme_dw() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0)
    # axis.title = element_text(size = 10),
    # axis.text = element_text(size = 5)
  )


g9+g10+g11+g12+g13+g14+g15+g16 + 
  plot_layout(nrow = 2, ncol = 4) + 
  plot_annotation(
  title = "Figura 4: Histogramas das variáveis em análise.",
  # sudtitle = "",
  caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
  # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
  # tag_levels = "A",
  # tag_suffix = ":"
) & theme_dw() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0)
  )

#### Dispersão ----
d8 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot(aes(
    y = diabetes, 
    x = pregnant, color = pregnant)) +
  geom_point()+
  labs(
    title = 'Função Diadética x N° de Gestações',
    y = 'Nível da Função Diabética',
    x = 'N° de Gestações',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d9 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot(aes(
    y = diabetes, 
    x = glucose, color = glucose)) +
  geom_point()+
  labs(
    title = 'Função Diadética x N. Glicose',
    x = 'Nível de Glicose',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d10 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot(aes(
    y = diabetes, 
    x = diastolic, color = diastolic)) +
  geom_point()+
  labs(
    title = 'Função Diadética x P. Diastólica',
    x = 'Pressão Diastólica',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d11 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot(aes(
    y = diabetes, 
    x = triceps, color = triceps)) +
  geom_point()+
  labs(
    title = 'Função Diadética x L. Tríceps',
    x = 'Largura do Tríceps (mm)',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d12 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot(aes(
    y = diabetes, 
    x = insulin, color = insulin)) +
  geom_point()+
  labs(
    title = 'Função Diadética x N. Insulina',
    x = 'Nível de Insulina',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d13 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot(aes(
    y = diabetes, 
    x = age, color = age)) +
  geom_point()+
  labs(
    title = 'Função Diadética x Idade',
    x = 'Idade',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

d14 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  ggplot(aes(
    y = diabetes, 
    x = bmi, color = bmi)) +
  geom_point()+
  labs(
    title = 'Função Diadética x IMC',
    x = 'IMC',
    y = 'Nível da Função Diabética',
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA",
    color = "N° de Gestações"
  )+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
  theme_bw()+
  theme(legend.position = "none")

##### 2 ----
(d8+d9)/(d10+d11)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura 4: Histogramas das variáveis em análise.",
    # sudtitle = "",
    # caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_bw(base_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0),
    legend.position = "none"
  )

(d12+d13)/d14+plot_spacer()+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    # title = "Figura : ",
    # sudtitle = "",
    caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_bw(base_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0),
    legend.position = "none"
  )


#### Boxplot ----
d1 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = pregnant)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'N° de Gestações',
    # title = 'Comparativo entre N° de Gestações e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Gestações"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()
  
d2 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = glucose)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Nível de Glicose',
    # title = 'Comparativo entre Nível de Glicose e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Glicose"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d3 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = diastolic)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Pressão Diastólica',
    # title = 'Comparativo entre Pressão Diastólica e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Pressão Diastólica"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d4 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = insulin)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Nível de Insulina',
    # title = 'Comparativo entre Nível de Insulina e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Nível de Insulina"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d5 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = dmi)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'IMC',
    # title = 'Comparativo entre o IMC e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "IMC"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d6 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = triceps)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Largura do Tríceps',
    # title = 'Comparativo entre Largura do Tríceps (mm) e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Tríceps"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d7 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = age)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'Idade',
    # title = 'Comparativo entre Largura do Tríceps (mm) e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Idade"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

d8 <- dados|>
  filter(glucose>0, diadetes>0, diastolic>0,
         triceps>0, insulin>0, dmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = diadetes)) +
  geom_doxplot(col="darkdlue", fill="skydlue", alpha = 0.5)+
  lads(
    title = 'N. Diadetes',
    # title = 'Comparativo entre Largura do Tríceps (mm) e Sinais de Diadetes',
    x = "Sinais de diadetes",
    y = "Diadetes"
  ) +
  scale_y_continuous(
    ladels = scales::numder_format(
      dig.mark = ".",
      decimal.mark = ","))+
  theme_dw()

(d1+d2)/(d3+d4)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura : ",
    # sudtitle = "",
    caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_dw(dase_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0)
  )

(d5+d6)/(d7+d8)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura : ",
    # sudtitle = "",
    caption = "Fonte: Instituto Nacional de Diadetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sud Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_dw(dase_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0)
  )










# FIM ----




