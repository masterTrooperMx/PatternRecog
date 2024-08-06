# ECONOMETRÍA DE EVALUACIÓN DE IMPACTO
# MODELOS DE ELECCIÓN DISCRETA I: CORTE TRANSVERSAL
# modelos de regresion simple y multiple
# sesion 2/8
# paquetes a utilizar ----
pacman::p_load(
  "tidyverse",
  "lmtest",
  "BiocManager",
  "foreign",
  "tseries",
  "car",
  "mctest",
  "readxl",
  "jtools",
  "heaven"
)

# regresion multiple
# a model that involves more than one regressor variable
# A regressor is a statistical term. It refers to any variable in a regression model that is used to predict a response variable. 
# A regressor is also known as:
#
#  -An independent variable
#  -An explanatory variable
#  -A predictor variable
#  -A feature
#  -A manipulated variable
# importacion de los datos ----
carros <- read.csv("Bases/car_price_prediction.csv", check.names = TRUE)
# analiza datos transversales, no datos de series de tiempo
summary(carros)
## Limpieza de datos ----
which(is.na(carros), arr.ind=TRUE) # encuentra los NAs

# Levy column has NAs like -
str_replace(carros$Levy, "-", "0")
carros$Levy <- as.numeric(
  str_replace(carros$Levy, "-", "0")
)
# Engine volume es una columna mezclada con numeros y tipo (Turbo)
# con ! quita el patron, porque niega los resultados
#carros$Engine.volume[!grepl("Turbo", carros$Engine.volume)]
# ahora lo reasignamos a la columna y lo volvemos numeric
#carros$Engine.volume <- as.numeric(
#  carros$Engine.volume[!grepl("Turbo", carros$Engine.volume)]
#)
str_replace(carros$Engine.volume, "Turbo", "")
carros$Engine.volume <- as.numeric(
  str_replace(carros$Engine.volume, "Turbo", "")
)
# Milage es un caso interesante porque los valores son en Km y hay que volverla numerica
# y transformarla ena millas, mutate es oara crear, modificar o borrar columnas
carros$Mileage <- as.numeric(
  str_replace(carros$Mileage, " km", "")
)
carros <- carros %>%
#  mutate(
#    Milage = as.numeric(
#      str_replace(Mileage, " km", "")
#    )
#  ) %>%
  rename(Kilometros = Mileage) %>%
  mutate(Millas = Kilometros/1.609)

## ejemplo para calculo multivariado ----
# seleccionar variables independientes (regresoras)
# https://es.stackoverflow.com/questions/304145/al-usar-el-comando-select-en-r-me-arroja-el-error-unused-error
mis_carros <- carros %>%
  dplyr::select(
    Price,
    Engine.volume,
    Millas,
    Prod..year,
    Cylinders,
    Airbags
    )
## visualizacion de la informacion ----
### grafico de dispersion ----
mis_carros %>%
  data.table::data.table() %>%
  data.table::melt(
    id.vars = "Price",
    value.name = "Valor",
    variable.name = "Variable"
  ) %>%
  ggplot(aes(x = Valor, y = Price))+
  geom_point()+
  facet_wrap(~Variable)

pairs(mis_carros)

# hay un valores extremos, habra que trabajar la BD para acotarla
# quitar outliners
mis_carros %>%
  filter(
    (Price >=10000 & Price <= 100000) &
    (Millas < 1000000) &
    (Engine.volume < 10)
  ) %>%
  data.table::data.table() %>%
  data.table::melt(
    id.vars = "Price",
    value.name = "Valor",
    variable.name = "Variable"
  ) %>%
  ggplot(aes(x = Valor, y = Price))+
  geom_point()+
  facet_wrap(~Variable, scales = "free")+
  theme_minimal()+
  labs(
    title = "Relacion tama;o motor , millas y a;o vs Precio en (usd)",
    x = "",
    y = "Precio del vehiculo en USD"
  )
pairs(mis_carros)
# renombrare las columnas para que se mas comodo
mis_carros <- carros %>%
  dplyr::select(
    Price,
    Engine.volume,
    Millas,
    Prod..year
  ) %>%
  dplyr::rename(
    Precio = Price,
    Tam.Motor = Engine.volume,
    Millas = Millas,
    `Año` = Prod..year
  )
summary(mis_carros)

# Estimacion del modelo

MLR = lm(
  Precio ~ `Tam.Motor` + Millas + `Año`,
  data = 
    mis_carros %>%
    dplyr::filter(
        (Precio >=10000 & Precio <= 100000) &
          (Millas < 1000000) &
          (`Tam.Motor` < 10)
      )
)

summary(MLR)
summ(MLR)
# dado que los valores en la tabla resultante de estimadores tienen valores de p < 0.05
# entonces se rechaza Ho que dice que el valor del estimador el 0
# se acepta H1 que establece que son distintos de 0 y son validos para estimar
# en el caso de Tam.Motor el estimador dice que por cada unidad de tama;o de motor
# adicional, aumenta el precio en 6878.01 usd
# en el caso de Millas, por cada milla aumentada, decrece el Precio en -0.04 usd
# y en Año por cada unidad en esta variable aumenta 1464.99 usd (es mas nuevo el carro)
# el resultado F(3,11528) = 1558.01, p = 0.00 indica que 
# la Ho es que los estimadores no son representativos de la muestra y por lo tanto son 0
# dado que p < 0.05 entonces se rechaza Ho y se acepta H1, los estimadores son 
# representativos y distintos de 0.
# finalmente el 29% de los datos hace variar los datos de la muestra porque R^2 = 0.29
# explican en 29% la varianza de la variable dependiente (Precio)

# validacion del modelo ----
## Multicolinealidad ----
mis_carros %>%
  dplyr::filter(
    (Precio >=10000 & Precio <= 100000) &
      (Millas < 1000000) &
      (`Tam.Motor` < 10) 
  )%>%
  dplyr::select(-Precio) %>%
  cor()
# se miden los coeficientes de correlacion entre variables
# si los valores superan el -0.7 o 0.7 hay una gran correlacion y entonces no se
# pueden explicar bien los datos, porque la correlacion impide explicar las relaciones

## Factor de inflacion de varianza
# vIF = 0 no hay multicolinialidad
# 0 < vIF < 5 multicolinialidad debil
# 5 < vIF < 10 multicolinialidad moderada
# vIF > 10 multicolinialidad grave aqui ya no se puede usar el modelo porque las variables
# independientes se correlacionan fuertemente
vif(MLR) # tienen multicolinealidad debil

# indice de condicion (mas importante)
# 0 no hay multicolinealidad
# 0 < < 10 multicolinialidad debil
# 10 < < 30 multicolinialidad moderada
# > 30 multicolinialidad grave
eigprop(MLR)
# eigprop marca un valor altisimo 1358.7927 que indica multicolinealidad perfecta
# las demas pruebas no, solo esta si

## Autocorrelacion ----
dwtest(MLR) # p > 0.05 se rechaza Ho que dice que la autocorrelacion es menor a 0
# se acepta H1 es verdad que la autocorrelacion e mayor a 0
# creando el filtro
fCar2 <- mis_carros %>%
  dplyr::filter(
    (Precio >=10000 & Precio <= 100000) &
      (Millas < 1000000) &
      (`Tam.Motor` < 10)
  )
    
# esta prueba es mas importante
# prueba de autocorrelacion de cualquier orden
# p > 0.05 se rechaza Ho que dice que la autocorrelacion es menor a 0
# se acepta H1 es verdad que la autocorrelacion e mayor a 0
bgtest(MLR)
# se divide el segmento en 2 y dice que del segmento 1 al 2 hay incremento de varianza
# pero se observa el cambio df1 = df2 por lo que no hay cambio de varianza entre los segmentos
cor(fCar2, MLR$residuals)

### Heterocedasticidad ----
bptest(MLR)
# Ho es que hay homocedasticidad si p > 0.05
# se rechaza Ho y se acepta H1 por p < 0.05 y existe heterocedasticidad
gqtest(MLR, point = 0.5)

### normalidad ----
jarque.bera.test(MLR$residuals)
adf.test(MLR$residuals)

# prediccion ----
prediccion <- predict(MLR,
                      newdata = data.frame(
                        "area" = c(10000, 150000),
                        interval = "confidence"
                      )
)
prediccion










