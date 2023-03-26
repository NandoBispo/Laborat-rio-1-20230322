# PACOTES ----
if (!require(pacman))
  install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse, janitor, kableExtra, summarytools, 
               moments, ggthemes, patchwork, glue, ggpubr, formattable)

# DADOS ----
dados <- read.csv2("Dados_Lab01.csv")

glimpse(dados)

dados <- dados |> 
  mutate(
    diabetes = as.numeric(diabetes),
    bmi = as.numeric(bmi)
    )

glimpse(dados)

# TABELA 1 ----

{dados|>
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
      caption = "Tabela 1: Medidas Resumo",
      digits = 2,
      format.args=list(big.mark=".", decimal.mark=","),
      align = "c", 
      row.names = T,
      col.names =
        c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
    )|>
    kable_material(c("striped", "hover", "condensed"))|>
    # kable_styling(
    #   # bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    #   bootstrap_options = c("striped", "hover"),
    #   full_width = F,
    #   fixed_thead = T # Fixa o cabeçalho ao rolar a tabela.
    # ) %>%
    footnote(general = "Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
    kable_material()
    # add_header_above(c("Características", "Medidas de Tendência Central e Variabilidade" = 8))
  } # Tebela com medidas resumo.

# Não funcionou - ainda!
# dados|>
#   select(-test)|>
#   rename(
#     "N° de Gestações" = pregnant, "Glicose" = glucose, "Idade" = age,
#     "P. Diastólica" = diastolic, "Largura Triceps" = triceps,
#     "Nível Insulina" = insulin, "IMC" = bmi, "Nivel Diabético" = diabetes)|>
#   summarytools::descr(
#     stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
#     round.digits = 2,
#     justify = "c",
#     style = "grid", #' rmarkdown',
#     transpose = T
#   ) |>
#   formattable(list(
#     Mean = color_tile("transparent", "lightpink")
#     # Q1 = color_bar("lightgreen")
#     # Mean = sign_formatter
#   ),formatter = )


# GRÁFICOS ----

# Teste antes de aplicar no gráfico
dados %>% 
  mutate(
    tipo = case_when(
      test == 0 ~ "Negativo",
      test == 1 ~ "Positivo"
    )
  )|>
  count(tipo)

## Fig_1 ----
dados |>
  count(test) |>
  mutate(
    pct = round(prop.table(n)*100, 1),
    tipo = case_when(
      test == 0 ~ "Negativo",
      test == 1 ~ "Positivo"),
    labs = glue::glue('{tipo}\n({pct}%)')) %>%
  ggpubr::ggdonutchart(., "pct", 
                       label = "labs", lab.pos = "in",
                       lab.font = c(4, "plain", "white"),
                       fill = "test",  color = "white")+
  labs(
    title = "Figura 1: Resultado dos testes de sinais \nde diabetes realizados nas mulheres \nda Tribo Pina",
    caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
  )+
  theme(
    legend.position = "none"
    # axis.title = element_text(hjust = 0)
  )

## Fig_2.1 ----
# Elaboração da Figura 2.

g1 <- dados|>
  ggplot() +
    aes(x = glucose) +
  geom_histogram(
    aes(y = after_stat(density)),
    # binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Glicose",
    x = "Concentração",
    y = "Densidade"
  ) + theme_bw()

g2 <- dados|>
  ggplot() +
    aes(x = pregnant) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Gestações",
    x = "Quantidade",
    y = "Densidade"
  ) + theme_bw()

g3 <- dados|>
  ggplot() +
    aes(x = diastolic) +
  geom_histogram(
    aes(y = after_stat(density)),
    # binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Pressão Diastólica",
    x = "Medida (mmHg)",
    y = "Densidade"
  ) + theme_bw()

g4 <- dados|>
  ggplot() +
    aes(x = triceps) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Tríceps",
    x = "Largura (mm)",
    y = "Densidade"
  ) + theme_bw()

# (g1+g2)/(g3+g4) + plot_annotation(
#   title = "Figura 1: Histogramas das variáveis em análise.",
#   # subtitle = "",
#   caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
#   # tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
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
    # binwidth = 25,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Insulina",
    x = "Nível (µU/ml)",
    y = "Densidade"
  )

g6 <- dados|>
  ggplot() +
  aes(x = bmi) +
  geom_histogram(
    aes(y = after_stat(density)),
    # binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "IMC",
    x = "Índice (kg/m²)",
    y = "Densidade"
  )

g7 <- dados|>
  ggplot() +
  aes(x = diabetes) +
  geom_histogram(
    aes(y = after_stat(density)),
    # binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Diabetes",
    x = "Nível",
    y = "Densidade"
  )

g8 <- dados|>
  ggplot() +
  aes(x = age) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Idade",
    x = "Anos",
    y = "Densidade"
  )+
theme(
    axis.title = element_text(size = 5),
    axis.text = element_text(size = 2))

## Fig_2.2 ----
(g1+g2+g3)/(g4+plot_spacer()+g5)/(g6+g7+g8) + plot_annotation(
  title = "Figura 2: Histogramas das variáveis em análise.",
  # subtitle = "",
  caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
  # tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  # tag_levels = "A",
  # tag_suffix = ":"
) & theme_bw() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 5)
  )
# Teste
# (g1+g2+g3)/(g4+grid::textGrob("Teste de inserção\nde texto em gráfico")+g5)/(g6+g7+g8) + plot_annotation(
#   title = "Figura 2: Histogramas das variáveis em análise.",
#   # subtitle = "",
#   caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
#   # tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
#   # tag_levels = "A",
#   # tag_suffix = ":"
# ) & theme_bw() &
#   theme(
#     plot.tag.position = c(0, 1),
#     plot.tag = element_text(size = 8, hjust = 0, vjust = 0),
#     axis.title = element_text(size = 10),
#     axis.text = element_text(size = 5)
#   )

dados|>
group_by(test)|>
summarise(
  contagem = n()
)|>
ggplot(
  aes(x = "", y = contagem/sum(contagem), fill = test)) +
  geom_bar(stat = "identity", width = .3) +
  geom_text(
    aes(
      label = scales::percent(contagem/sum(contagem)),
      y=contagem/sum(contagem),
      # stat = contagem, 
      vjust = -0.2
    ))+
  xlab("")+
  ylab("Proporção")+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()


#SEM ZEROS ---- 
## Fig_3 ----
dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  count(test) |>
  mutate(
    pct = round(prop.table(n)*100, 1),
    tipo = case_when(
      test == 0 ~ "Negativo",
      test == 1 ~ "Positivo"),
    labs = glue::glue('{tipo}\n({pct}%)')) %>%
  ggpubr::ggdonutchart(., "pct", 
                       label = "labs", lab.pos = "in",
                       lab.font = c(4, "plain", "white"),
                       fill = "test",  color = "white")+
  labs(
    title = "Figura 3: Resultado dos testes de sinais \nde diabetes realizados nas mulheres \nda Tribo Pina",
    caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
  )+
  theme(
    legend.position = "none"
    # axis.title = element_text(hjust = 0)
  )

## Fig_4.1 ----
g9 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  ggplot() +
  aes(x = glucose) +
  geom_histogram(
    aes(y = after_stat(density)),
    # binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Glicose",
    x = "Concentração",
    y = "Densidade"
  )

g10 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  ggplot() +
  aes(x = pregnant) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Gestações",
    x = "Quantidade",
    y = "Densidade"
  )

g11 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  ggplot() +
  aes(x = diastolic) +
  geom_histogram(
    aes(y = after_stat(density)),
    # binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Pressão Diastólica",
    x = "Medida (mmHg)",
    y = "Densidade"
  )

g12 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  ggplot() +
  aes(x = triceps) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Tríceps",
    x = "Largura (mm)",
    y = "Densidade"
  )


g13 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  ggplot() +
  aes(x = insulin) +
  geom_histogram(
    aes(y = after_stat(density)),
    # binwidth = 25,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Insulina",
    x = "Nível (µU/ml)",
    y = "Densidade"
  )

g14 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  ggplot() +
  aes(x = bmi) +
  geom_histogram(
    aes(y = after_stat(density)),
    # binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "IMC",
    x = "Índice (kg/m²)",
    y = "Densidade"
  )

g15 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  ggplot() +
  aes(x = diabetes) +
  geom_histogram(
    aes(y = after_stat(density)),
    # binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Diabetes",
    x = "Nível",
    y = "Densidade"
  )

g16 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  ggplot() +
  aes(x = age) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 5,
    fill = "lightblue",
    colour = "darkblue") +
  geom_density(
    alpha = 0.2,
    fill = "blue",
    colour = "blue") +
  labs(
    title = "Idade",
    x = "Anos",
    y = "Densidade"
  )+
  theme(
    axis.title = element_text(size = 5),
    axis.text = element_text(size = 2))

## Fig_4.2 ----
(g9+g10+g11)/(g12+plot_spacer()+g13)/(g14+g15+g16) + plot_annotation(
  title = "Figura 4: Histogramas das variáveis em análise.",
  # subtitle = "",
  caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
  # tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  # tag_levels = "A",
  # tag_suffix = ":"
) & theme_bw() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 5)
  )

g9+g10+g11+g12+g13+g14+g15+g16 + 
  plot_layout(nrow = 2, ncol = 4) + 
  plot_annotation(
  title = "Figura 4: Histogramas das variáveis em análise.",
  # subtitle = "",
  caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
  # tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
  # tag_levels = "A",
  # tag_suffix = ":"
) & theme_bw() &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0)
  )
# TABELA 2 ----

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
      caption = "Tabela 1: Medidas Resumo",
      digits = 2,
      format.args=list(big.mark=".", decimal.mark=","),
      align = "c", 
      row.names = T,
      col.names =
        c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
    )|>
    kable_styling(
      # bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      # bootstrap_options = c("striped", "hover"),
      latex_options = c("striped"),
      full_width = F,
      fixed_thead = T # Fixa o cabeçalho ao rolar a tabela.
    ) %>%
    footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
    kable_material()

## Dispersão ----
dados |>
  ggplot(aes(
    y = diabetes, 
    x = pregnant, color = pregnant)) +
  geom_point()+
  labs(
    title = 'Função Diabética x N° de Gestações',
    y = 'Nível da Função Diabética',
    x = 'N° de Gestações',
    caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA",
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
  theme(legend.position = "bottom")

dados |>
  ggplot(aes(
    y = diabetes, 
    x = glucose, color = glucose)) +
  geom_point()+
  labs(
    title = 'Figura 3: Diagrama de Disperção',
    y = 'Nível da Função Diabética',
    x = 'Nível de Glicose',
    caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA",
    color = "Glicose"
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
  theme(legend.position = "bottom")

## Boxplot ----
b1 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = pregnant)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'N° de Gestações',
    # title = 'Comparativo entre N° de Gestações e Sinais de Diabetes',
    x = "Sinais de diabetes",
    y = "Gestações"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","))+
  theme_bw()
  
b2 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = glucose)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'Nível de Glicose',
    # title = 'Comparativo entre Nível de Glicose e Sinais de Diabetes',
    x = "Sinais de diabetes",
    y = "Glicose"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","))+
  theme_bw()

b3 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = diastolic)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'Pressão Diastólica',
    # title = 'Comparativo entre Pressão Diastólica e Sinais de Diabetes',
    x = "Sinais de diabetes",
    y = "Pressão Diastólica"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","))+
  theme_bw()

b4 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = insulin)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'Nível de Insulina',
    # title = 'Comparativo entre Nível de Insulina e Sinais de Diabetes',
    x = "Sinais de diabetes",
    y = "Nível de Insulina"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","))+
  theme_bw()

b5 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = bmi)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'IMC',
    # title = 'Comparativo entre o IMC e Sinais de Diabetes',
    x = "Sinais de diabetes",
    y = "IMC"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","))+
  theme_bw()

b6 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = triceps)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'Largura do Tríceps',
    # title = 'Comparativo entre Largura do Tríceps (mm) e Sinais de Diabetes',
    x = "Sinais de diabetes",
    y = "Tríceps"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","))+
  theme_bw()

b7 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = age)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'Idade',
    # title = 'Comparativo entre Largura do Tríceps (mm) e Sinais de Diabetes',
    x = "Sinais de diabetes",
    y = "Idade"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","))+
  theme_bw()

b8 <- dados|>
  filter(glucose>0, diabetes>0, diastolic>0,
         triceps>0, insulin>0, bmi>0)|>
  mutate(
    test = as_factor(test),
    test = lvls_revalue(test, c("Negativo", "Positivo"))
  )|>
  ggplot(aes(x = test, y = diabetes)) +
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = 'N. Diabetes',
    # title = 'Comparativo entre Largura do Tríceps (mm) e Sinais de Diabetes',
    x = "Sinais de diabetes",
    y = "Diabetes"
  ) +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","))+
  theme_bw()



(b1+b2)/(b3+b4)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura : ",
    # subtitle = "",
    caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_bw(base_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0)
  )

(b5+b6)/(b7+b8)+
  # plot_layout(nrow = 2, ncol = 3) + 
  plot_annotation(
    title = "Figura : ",
    # subtitle = "",
    caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA"
    # tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    # tag_levels = "A",
    # tag_suffix = ":"
  ) & theme_bw(base_size = 10) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 12, hjust = 0, vjust = 0)
  )










# FIM ----




