#=======================================================#
# Especialidad: Econometría moderna                     #
# Modulo: Econometría de evaluación de impacto          #
# Tarea 01: Modelos de regresión simple.                #
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
  "nortest",
  "ggplot2",
  "ggpubr",
  "GGally"
)


# Importación de los datos ----
# el path es relativo
salarios <- read.csv("Archivos tarea 1/Salary_dataset.csv",
                      check.names = TRUE)

desempenio_acad <- read.csv("Archivos tarea 1/Student_Performance.csv",
                check.names = TRUE)

# Regresión lineal simple SALARIOS ----
summary(salarios)
which(is.na(salarios), arr.ind=TRUE) # encuentra los NAs
## Revisar relación para suponer el valor de beta SALARIOS ----
# cualquier variable que el experimentador manipule puede denominarse variable independiente
# x = años de experiencia, y = salario
# vamos a ver como se relacionan los años de experiencia con respecto al salario
# supongo que entre mas años de experiencia el salario es mayor
salarios %>%
  ggplot(aes(x = YearsExperience, y=Salary))+
  geom_point()+
  scale_y_continuous(breaks = seq(30000, 150000, 10000),
                     labels = scales::comma)+
  scale_x_continuous(breaks = seq(0.5, 12, 1),
                     labels = scales::comma)+
  labs(title = "Gráfico de dispesión sobre los a;os de experiencia y el salario",
       subtitle = "cifras en dólares",
       x = "Años de Experiencia",
       y = "Salario")+ 
  geom_smooth(se = TRUE, method = "lm")
## Estimación del modelo de regresión líneal SALARIOS ----

RLS <- lm(
  formula = Salary ~ YearsExperience,
  data = salarios
)

summary(RLS)
summ(RLS)
# R^2 = 1 todas las variaciones se deben a x
# R^2 = 0 ninguna de las variaciones se deben a x
# Y = 24848.20 + 9449.96*X
# el salario inicial es de 24,848.20 con 0 a;os de experiencia
# por cada a;o de experiencia se le suman al salario 9449.96 
# plot(RLS) + abline()
# mejor geom_smooth

#abline(reg = RLS, col='red', lwd=4)
#abline(coef = coef(RLS))

# El𝑅2 es una medida que indica la proporción de la variabilidad total de la 
# variable dependiente que es explicada por las variables independientes del modelo. 
# Su valor varía entre 0 y 1.
# en este caso es 0.96 lo que implica que el 96% de las variaciones del salario 
# se explica con las variaciones de los a;os de experiencia

# Un p-valor bajo (generalmente menor que 0.05) sugiere que el coeficiente es 
# estadísticamente significativo
# para A;os de experiencia p-value: < 2.2e-16 sugiere que el modelo es 
# estadisticamente significativo, ya que F0 es que los coeficientes no lo son
# con un nivel de copnfianza del 95%

# Los errores estándar proporcionan una medida de la precisión de las estimaciones 
# de los coeficientes. Un error estándar pequeño indica una estimación más precisa 
# del coeficiente.
# ambos valores estandar son altos se sugiere que los valores son precisos

## Validación del modelo SALARIOS ----

### Autocorrelación SALARIOS -----
# La perturbación de una observación cualquiera u_i  está correlacionada con 
# la perturbación de cualquier otra observación => las observaciones no son independientes.
# p -> -1, dwt = 4, autocorrelacion perfecta negativa
# p -> 0, dwt = 2, ausencia de autocorrelacion serial
# p -> 1, dwt = 0, autocorrelacion perfecta positiva
dwtest(RLS)
# ausencia de autocorrelacion

# La correlación, también conocida como coeficiente de correlación lineal 
# (de Pearson), es una medida de regresión que pretende cuantificar el grado de 
# variación conjunta entre dos variables:
# p = -1, correlacion perfecta negativa
# p = 0, no existe correlacion
# p = 1, correlacion perfecta positiva
cor(salarios$YearsExperience, RLS$residuals)
# entre residuos y salario es perfecta negativa, 
cor(salarios$Salary, salarios$YearsExperience)
# entre salario y a;os de experiencia es perfecta positiva ambas crecen al unisono
plot(RLS) + abline()

### Heterocedasticidad SALARIOS ----
# cuando los errores no son constantes a lo largo de toda la muestra
# Analiza si la varianza estimada de los residuos de una regresión dependen de
# los valores de las variables independientes.
bptest(RLS)
# p > 0.05 se acepta H0 hay Heterocedasticidad
# cuando la varianza de los errores no es constante en todas las observaciones realizadas.
gqtest(RLS, point = 0.5)
# p > 0.05 se acepta H0 los valores son heterogeneos
### Normalidad SALARIOS ----
# se basa en la asimetría y la curtosis de un conjunto de datos, que son medidas 
# de asimetría y pesadez de la cola, respectivamente
jarque.bera.test(RLS$residuals)
# p > 0.05, se acepta H0, los datos se conforman con la normalidad
# mide qué tan bien siguen los datos una distribución específica, siendo que, para 
# un conjunto de datos y distribución en particular, mientras mejor se ajuste la 
# distribución a los datos, menor será este estadístico
ad.test(RLS$residuals)
# p > 0.05, se acepta H0 y se sigue una distribucion normal
## Predicción SALARIOS ----

prediccion <- predict(RLS,
                      newdata = data.frame(
                        "YearsExperience" = c(11, 25)
                      ), interval = "confidence")

prediccion

# Regresión lineal simple DESEMPE;O ----
summary(desempenio_acad)
which(is.na(desempenio_acad), arr.ind=TRUE) # encuentra los NAs
## Revisar relación para suponer el valor de beta DESEMPE;O ----
# cualquier variable que el experimentador manipule puede denominarse variable independiente
# des1
# y1 = indice de desempe;o, x = horas estudiadas
# des2
# y2 = indice de desempe;o, x = horas de sue;o
# des3
# y3 = indice de desempe;o, x = actividades extracurriculares
# des4
# y4 = indice de desempe;o, x = practicas
# des5
# y5 = indice de desempe;o, x = calificaciones previas
# des1
# relacion desepe;o vs horas estudiadas
# supongo que el desempe;o aumenta mientras estudio mas horas
desempenio_acad %>%
  ggplot(aes(y = Performance.Index, x = Hours.Studied))+
  geom_point(shape = 18, color = "#00AFBB") #+
  #scale_y_continuous(breaks = seq(30000, 150000, 10000),
  #                   labels = scales::comma)+
  #scale_x_continuous(breaks = seq(0.5, 12, 1),
  #                   labels = scales::comma)+
  #labs(title = "Gráfico de dispesión sobre los a;os de experiencia y el salario",
  #     subtitle = "cifras en dólares",
  #     x = "Años de Experiencia",
  #     y = "Salario")#+ 
  #geom_smooth(method = "lm")
# des2
# relacion desepe;o vs horas de sue;o
# supongo que el desempe;o aumenta mientras duermo mas horas
desempenio_acad %>%
  ggplot(aes(y = Performance.Index, x = Sleep.Hours))+
  geom_point(shape = 18, color = "#00AFBB") #+
#scale_y_continuous(breaks = seq(30000, 150000, 10000),
#                   labels = scales::comma)+
#scale_x_continuous(breaks = seq(0.5, 12, 1),
#                   labels = scales::comma)+
#labs(title = "Gráfico de dispesión sobre los a;os de experiencia y el salario",
#     subtitle = "cifras en dólares",
#     x = "Años de Experiencia",
#     y = "Salario")#+ 
#geom_smooth(method = "lm")
#des3
# relacion desepe;o vs actividades extracurriculares
# supongo que el desempe;o aumenta mientras mas actividades extracurriculares hago
desempenio_acad %>%
  ggplot(aes(y = Performance.Index, x = Extracurricular.Activities))+
  geom_point(shape = 18, color = "#00AFBB") #+
#scale_y_continuous(breaks = seq(30000, 150000, 10000),
#                   labels = scales::comma)+
#scale_x_continuous(breaks = seq(0.5, 12, 1),
#                   labels = scales::comma)+
#labs(title = "Gráfico de dispesión sobre los a;os de experiencia y el salario",
#     subtitle = "cifras en dólares",
#     x = "Años de Experiencia",
#     y = "Salario")#+ 
#geom_smooth(method = "lm")
#des4
# relacion desepe;o vs practicas
# supongo que el desempe;o aumenta mientras mas practicas hago
desempenio_acad %>%
  ggplot(aes(y = Performance.Index, x = Sample.Question.Papers.Practiced))+
  geom_point(shape = 18, color = "#00AFBB") #+
#scale_y_continuous(breaks = seq(30000, 150000, 10000),
#                   labels = scales::comma)+
#scale_x_continuous(breaks = seq(0.5, 12, 1),
#                   labels = scales::comma)+
#labs(title = "Gráfico de dispesión sobre los a;os de experiencia y el salario",
#     subtitle = "cifras en dólares",
#     x = "Años de Experiencia",
#     y = "Salario")#+ 
#geom_smooth(method = "lm")
#des5
# relacion desepe;o vs calificaciones previas
# supongo que el desempe;o aumenta mientras mejor calificacion previa
desempenio_acad %>%
  ggplot(aes(y = Performance.Index, x = Previous.Scores))+
  geom_point(shape = 18, color = "#12AF00") +
  scale_y_continuous(breaks = seq(5, 110, 10),
                   labels = scales::comma)+
  scale_x_continuous(breaks = seq(30, 110, 10),
                   labels = scales::comma)+
  labs(title = "Gráfico de dispesión sobre desempe;o academico y calificaciones anterioes",
       subtitle = "cifras en puntos",
       x = "Calificaciiones anteriores",
       y = "Desempe;o academico")+ 
  geom_smooth(method = "lm")
# todos los graficos en 1
ggpairs(desempenio_acad)
pairs(desempenio_acad, main = "Desempe;o academico")

modelo1 <- lm(
  formula = Performance.Index ~ Hours.Studied,
  data = desempenio_acad
)

modelo2 <- lm(
  formula = Performance.Index ~ Sleep.Hours,
  data = desempenio_acad
)

modelo3 <- lm(
  formula = Performance.Index ~ Extracurricular.Activities,
  data = desempenio_acad
)

modelo4 <- lm(
  formula = Performance.Index ~ Sample.Question.Papers.Practiced,
  data = desempenio_acad
)

modelo5 <- lm(
  formula = Performance.Index ~ Previous.Scores,
  data = desempenio_acad
)

summary(modelo1)
summ(modelo1)
# R^2 es 0.14 eso quiere decir que el 14% de los datos es por x, no es un buen candidato
summary(modelo2)
summ(modelo2)
# R^2 es 0, eso quiere decir que el 0% de los datos varia por x, no es un buen cadidato
summary(modelo3)
summ(modelo3)
# R^2 es 0, eso quiere decir que el 0% de los datos varia por x, no es un buen cadidato
summary(modelo4)
summ(modelo4)
# R^2 es 0, eso quiere decir que el 0% de los datos varia por x, no es un buen cadidato
summary(modelo5)
summ(modelo5)
# R^2 es 0.84, eso quiere decir que el 84% de los datos varia por x, si es un buen cadidato

## Validación del modelo DESEMPE;O ----

### Autocorrelación DESEMPE;O -----
dwtest(modelo5)
# p -> -1, dwt = 4, autocorrelacion perfecta negativa
# p -> 0, dwt = 2, ausencia de autocorrelacion serial
# p -> 1, dwt = 0, autocorrelacion perfecta positiva

# ausencia de correlacion serial
# los residuos son independientes
sum(modelo5$residuals)
# la suma -> 0 parece bien

# p = -1, correlacion perfecta negativa
# p = 0, no existe correlacion
# p = 1, correlacion perfecta positiva
 ~ 
cor(desempenio_acad$Previous.Scores, modelo5$residuals)
# entre calificaciones anteriores y residuos hay ausencia de correlacion, 
cor(desempenio_acad$Performance.Index, desempenio_acad$Previous.Scores)
# entre calificaciones anteriores y desempe;o academico es perfecta positiva ambas crecen al unisono

### Heterocedasticidad DESEMPE;O ----
# cuando los errores no son constantes a lo largo de toda la muestra
# Analiza si la varianza estimada de los residuos de una regresión dependen de
# los valores de las variables independientes.
bptest(modelo5)
# p > 0.05 se rechaza acepta H0 hay Heterocedasticidad
# cuando la varianza de los errores no es constante en todas las observaciones realizadas.
gqtest(modelo5, point = 0.5)
# p > 0.05 se acepta H0 los valores son heterogeneos
### Normalidad DESEMPE;O ----
# se basa en la asimetría y la curtosis de un conjunto de datos, que son medidas 
# de asimetría y pesadez de la cola, respectivamente
jarque.bera.test(modelo5$residuals)
# p < 0.05, se rechaza H0, los datos no se conforman con la normalidad
# mide qué tan bien siguen los datos una distribución específica, siendo que, para 
# un conjunto de datos y distribución en particular, mientras mejor se ajuste la 
# distribución a los datos, menor será este estadístico
ad.test(modelo5$residuals)
# p < 0.05, se rechaza H0 y se sigue que no es una distribucion normal

## Predicción DESEMPE;O ----
prediccion <- predict(modelo5,
                      newdata = data.frame(
                        "Previous.Scores" = c(20, 39)
                      ), interval = "confidence")

prediccion
