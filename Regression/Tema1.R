# ECONOMETRÍA DE EVALUACIÓN DE IMPACTO
# MODELOS DE ELECCIÓN DISCRETA I: CORTE TRANSVERSAL
# modelos de regresion simple y multiple
# sesion 1/8
# paquetes a utilizar ----
pacman::p_load(
  "tidyverse",
  "lmtest",
  "heaven",
  "foreign",
  "tseries",
  "car",
  "mctest",
  "readxl",
  "jtools"
)

# importacion de los datos ----
viviendas <- read.csv("Bases/Housing.csv", check.names = FALSE)
carros <- read.csv("Bases/car_price_prediction.csv", check.names = FALSE)
# regresion lineal simple ----
summary(viviendas)
which(is.na(viviendas), arr.ind=TRUE) # encuentra los NAs
# revisar relacion para suponer el valor de beta ----
viviendas %>%
  ggplot(aes(x=area, y=price))+
  geom_point()+
  scale_y_continuous(breaks = seq(170000, 14000000, 1000000),
                     labels = scales::comma)+
  scale_x_continuous(breaks = seq(1500, 16200, 1000),
                     labels = scales::comma)+
  labs(title="Grafico de dispersion",
       subtitle = "cifras en dolares",
       x = "",
       y = "")
# estimacion del modelo de regresion lineal ----
# dependiente ~ independiente
RLS <- lm(
  formula = price ~ area,
  data = viviendas
)

summary(RLS)

summ(RLS)
# R2 coeficiente de determinacion, aqui explica en un 29% la variacion 
# p es el valor de probabilidad de hipotesis nula

# validacion del modelo ----
# pronostico es predecir una tendencia
# buscamos que ambos sean bajas
# autocorrelacion
dwtest(RLS) # es alto, le faltan datos, o aleatoriedad
cor(viviendas$price, RLS$residuals)
# correlacion fuerte <0.9
#correlacion moderada <0.5 >0.9
#heterocedasticidad
# esta prueba es mas importante
bptest(RLS)
# hipotesis nula se rechaza porque p<0.005
# menor peso de esta orueba
gqtest(RLS, point = 0.5)
# normalidad ----
jarque.bera.test(RLS$residuals)
adf.test(RLS$residuals)

## con logaritmos ----
# dependiente ~ independiente
RLS_l <- lm(
  formula = log(price) ~ log(area),
  data = viviendas
)

summary(RLS_l)

summ(RLS_l)

# autocorrelacion
dwtest(RLS_l) # es alto, le faltan datos, o aleatoriedad
cor(log(viviendas$price), RLS_l$residuals)

bptest(RLS_l)
gqtest(RLS_l, point = 0.5)

jarque.bera.test(RLS_l$residuals)
adf.test(RLS_l$residuals)

# prediccion ----
prediccion <- predict(RLS,
                      newdata = data.frame(
                        "area" = c(10000, 150000),
                        interval = "confidence"
                      )
)
prediccion

# con log no quedo tan bien
prediccion <- predict(RLS_l,
                      newdata = data.frame(
                        "area" = log(c(10000, 150000)),
                        interval = "confidence"
                      )
              )
prediccion
exp(prediccion)
