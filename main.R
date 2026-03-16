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

dados <- read_excel("~/projetoEst/DadosSatisfacao.xlsx") %>%na.omit

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

modelo_ajustado <- update(modelo, . ~ . -salario - tempo_empresa)

summary(modelo_ajustado)
vif(modelo_ajustado)

report(modelo_ajustado)

modelo_ajustado2 <- update(modelo_ajustado, . ~ . -distancia_km - genero)

AIC(modelo)
AIC(modelo_ajustado)
AIC(modelo_ajustado2)
BIC(modelo)
BIC(modelo_ajustado)
BIC(modelo_ajustado2)

tabela_anova1 <- anova(modelo_ajustado, modelo) 
tabela_anova2 <- anova(modelo_ajustado2, modelo_ajustado)

tabela_anova1
tabela_anova2






intercept_only <- lm(satisfacao ~ 1, data=dados)
all <- lm(satisfacao ~ ., data=dados)

#6
backward <- step(all, direction='backward', scope=formula(all), trace=1)
backward$anova

forward <- step(intercept_only, direction='forward', scope=formula(all), trace=1)
forward$anova

both <- step(intercept_only, direction='both', scope=formula(all), trace=1)
both$anova


#7
modelo_final <- lm(satisfacao ~ clima_org + treinamentos + salario  + carga_horaria + setor + idade, data = dados)

plot(modelo_final)

check_normality(modelo_final) #%>% plot()
check_heteroscedasticity(modelo_final)
check_heteroscedasticity(modelo_final) %>% plot()

check_outliers(modelo_final) 
check_outliers(modelo_final) %>% plot()
check_collinearity(modelo_final)
check_collinearity(modelo_final) %>% plot()
check_autocorrelation(modelo_final)
check_model(modelo_final)


#8
std <- parameters(mod3, standardize = "basic", vcov = "vcovBS", 
                  vcov_args = list(R= 2000) 
)
std %>% 
  select(-CI, -df_error) %>% 
  flextable() %>% 
  colformat_double(digits = 3) %>% 
  set_caption("Coeficientes de regressão")
