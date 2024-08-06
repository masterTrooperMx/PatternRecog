# ECONOMETRÍA DE EVALUACIÓN DE IMPACTO
# MODELOS DE ELECCIÓN DISCRETA I: CORTE TRANSVERSAL
# modelos de regresion simple y multiple
# sesion 3/8
rm(list = ls())
# paquetes a utilizar ----
pacman::p_load(
  "tidyverse",
  "dplyr",
  "ISLR",
  "lmtest",
  "tseries",
  "nortest",
  "mfx",
  "caret",
  "vcd",
  "jtools"
)

#carga de BD ----
datos <- Default
# en el momento
# https://rsanchezs.gitbooks.io/rprogramming/content/chapter9/index.html
# https://dplyr.tidyverse.org/reference/recode.html
mutateFunction <- function(x){
  ifelse(x=="Yes", 1,0)
}
# una forma
try(
  datos <- datos %>%
  dplyr::select(default, balance) %>%
  dplyr::mutate(default = ifelse(default == "Yes", 1, 0) ) 
)
  
# otra forma
datos <- datos[, c(1,3)] %>%
  #datos$default <- recode(datos$default, No = 0, Yes = 1)
  mutate(datos$default = ifelse(datos$default=="Yes", 1, 0))
#otra mas
datos$default <- as.numeric(
  str_replace(datos$default, "No", "0")
)
datos$default[is.na(datos$default)] <- 1
# aplicamos la regresion lineal
MLP <- lm(default ~ balance, data = datos)
summary(MLP)
summ(MLP, digits=4)
vif(MLP) # tienen multicolinealidad debil

# indice de condicion
# 0 no hay multicolinealidad
# 0 < < 10 multicolinialidad debil
# 10 < < 30 multicolinialidad moderada
# > 30 multicolinialidad grave
eigprop(MLP)
# eigprop marca un valor altisimo 1358.7927 que indica multicolinealidad perfecta
# las demas pruebas no, solo esta si

## Autocorrelacion ----
dwtest(MLP)
# esta prueba es mas importante
bptest(MLP)

gqtest(MLP, point = 0.5)
## normalidad ----
jarque.bera.test(MLP$residuals)
adf.test(MLP$residuals)

datos %>%
  ggplot(aes(x = default, y = balance))+
    geom_point()+
  geom_smooth(se = TRUE, method = "lm")


# modelo logit
modelo <- glm(default ~ balance, data = datos, family = "binomial")
summary(modelo)
summ(modelo)
# efectos marginales
logitmfx(default ~ balance, data = datos)
logitor(default ~ balance, data = datos)

## Autocorrelacion ----
dwtest(modelo)
# esta prueba es mas importante
bptest(modelo)

gqtest(modelo, point = 0.5)
## normalidad ----
jarque.bera.test(modelo$residuals)
adf.test(modelo$residuals)

modelo %>%
  ggplot(aes(x = default, y = balance))+
  geom_point()+
  geom_smooth(se = TRUE, method = "glm")

#
modelog <- glm(default ~ balance, data = datos, family = "binomial")
summary(modelo)
summ(modelo)

# ejercicio
# importacion de los datos ----
salarios <- read.csv("Bases/Salary_dataset.csv", check.names = TRUE)
# es una regresion lineal simple
summary(salarios)
rls1 <- lm(
  formula = salarios$Salary ~ salarios$YearsExperience,
  data = salarios
)

summ(rls1)
# R^2 = 0.96 hay una correlacion alta
# p < 0.05 se rechaza Ho y se acepta H1 que es que beta-1 es mayor a cero

# regresion multiple
# importacion de los datos ----
carros <- read.csv("Bases/car_price_prediction.csv", check.names = TRUE)
summary(carros)
