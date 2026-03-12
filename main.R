knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(skimr)
library(car)
library(report)
library(gtsummary)
library(easystats)
library(broom)
library(broom.helpers)

dados <- read_excel("~/projetoEst/DadosSatisfacao.xlsx") %>% na.omit()

glimpse(dados)

skim(dados)

dados_num <- dados %>% select_if(is.numeric)

corr_matrix <- cor(dados_num, use = "complete.obs")

corr_matrix

corrplot(corr_matrix, method = "color", addCoef.col = "black")

ggplot(dados, aes (x = nivel_cargo, y = satisfacao)) + geom_boxplot() + theme_minimal()
ggplot(dados, aes (x = setor, y = satisfacao)) + geom_boxplot() + theme_minimal()
ggplot(dados, aes (x = genero, y = satisfacao)) + geom_boxplot() + theme_minimal()

modelo <- lm(satisfacao ~ salario + nivel_cargo + tempo_empresa + carga_horaria + treinamentos
             + clima_org + distancia_km + idade + setor + genero, data = dados)

summary(modelo)

vif(modelo)
report(modelo)
modelo %>% tbl_regression()