#=======================================================#
# Especialidad: Econometría moderna                     #
# Modulo: Econometría de evaluación de impacto          #
# Tema 01: Modelos de regresión simple y múltiple       #
# Sesion: 01                                            #
# Docente: Econ. Alexis Adonai Morales Alberto          #  
# Alumno: Jesus E. Cruz Mtz.                            #
#=======================================================#

# Paquetes a usar ----

pacman::p_load(
  "tidyverse",
  "lmtest",
  "haven",
  "foreign",
  "tseries",
  "car",
  "mctest",
  "readxl",
  "jtools",
  "nortest"
)


# Importación de los datos ----

Viviendas <- read.csv("Bases/Tema1/Housing.csv",
                      check.names = FALSE)

Car <- read.csv("Bases/Tema1/car_price_prediction.csv",
                check.names = FALSE)

# Regresión lineal simple ----

## Revisar relación para suponer el valor de beta ----

Viviendas %>% 
  ggplot(aes(x = area, y=price))+
  geom_point()+
  scale_y_continuous(breaks = seq(1700000, 14000000, 1000000),
                     labels = scales::comma)+
  scale_x_continuous(breaks = seq(1500, 16200, 1000),
                     labels = scales::comma)+
  labs(title = "Gráfico de dispesión sobre el precio de la vivienda y el área",
       subtitle = "cifras en dólares",
       x = "",
       y = "")

## Estimación del modelo de regresión líneal ----

RLS <- lm(
  formula = price ~ area,
  data = Viviendas
)

summary(RLS)
summ(RLS)

## Validación del modelo ----

### Autocorrelación -----

dwtest(RLS)
cor(Viviendas$price, RLS$residuals)
cor(Viviendas$price, Viviendas$area)

### Heterocedasticidad ----

bptest(RLS)
gqtest(RLS, point = 0.5)

### Normalidad ----

jarque.bera.test(RLS$residuals)
ad.test(RLS$residuals)

## Predicción ----

prediccion <- predict(RLS,
                      newdata = data.frame(
                        "area" = c(10000, 5000)
                      ), interval = "confidence")

prediccion
