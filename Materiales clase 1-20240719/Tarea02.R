#=======================================================#
# Especialidad: Econometría moderna                     #
# Modulo: Econometría de evaluación de impacto          #
# Tarea 02: Modelos de regresión simple.                #
# Sesion: 04                                            #
# Docente: Econ. Alexis Adonai Morales Alberto          #  
# Alumno: Jesus E. Cruz Mtz.                            #
#=======================================================#

# Paquetes a usar ----

pacman::p_load(
  "tidyverse", "lmtest",
  "haven", "foreign",
  "tseries", "car",
  "mctest", "readxl",
  "jtools", "nortest",
  "ggplot2", "ggpubr",
  "GGally", "dplyr"
)

desempenio_acad <- read.csv("Archivos tarea 1/Student_Performance.csv",
                            check.names = TRUE)
# Regresión lineal multiple DESEMPE;O ----
summary(desempenio_acad)
which(is.na(desempenio_acad), arr.ind=TRUE) # encuentra los NAs
## convertir variables categoricas en numericas ----
try(
  desempenio_acad <- desempenio_acad %>%
    dplyr::select(Hours.Studied,Previous.Scores,Extracurricular.Activities,Sleep.Hours,Sample.Question.Papers.Practiced,Performance.Index) %>%
    dplyr::mutate(Extracurricular.Activities = ifelse(Extracurricular.Activities == "Yes", 1, 0) ) 
)

pairs(desempenio_acad)
# performance.Index es la varible dependiente
## ver los graficos posibles ----
desempenio_acad %>%
  ggplot(aes(x = Hours.Studied, y = Performance.Index, color = Performance.Index)) +
  geom_point() +
  #geom_boxplot() +
  theme_minimal() #+
  # Box plot with dot plot
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  # Box plot with jittered points
  # 0.2 : degree of jitter in x direction
  #geom_jitter(shape=16, position=position_jitter(0.2))

ggplot(desempenio_acad, aes(x=Hours.Studied, y=Performance.Index)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

ggplot(desempenio_acad, aes(x=Hours.Studied, y=Performance.Index)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

desempeno_tabla <- desempenio_acad %>%
  dplyr::select(
    Performance.Index,
    Hours.Studied,
    Sleep.Hours,
    Sample.Question.Papers.Practiced, 
    Extracurricular.Activities
  ) %>%
  dplyr::rename(
    Calificacion = Performance.Index,
    HEstudio = Hours.Studied,
    HSueno = Sleep.Hours,
    PPractica = Sample.Question.Papers.Practiced,
    ActExtra = Extracurricular.Activities
  )

summary(desempeno_tabla)

MLR1 = lm(
  Calificacion ~ HEstudio + HSueno,
  data = desempeno_tabla #%>%
#    dplyr::filter(
#      (Precio >=10000 & Precio <= 100000) &
#        (Millas < 1000000) &
#        (`Tam.Motor` < 10)
#    )
)
# https://stats.stackexchange.com/questions/256726/linear-regression-what-does-the-f-statistic-r-squared-and-residual-standard-err
# La F indic que tanto el modelo va mejorando
# Calificacion ~ HEstudio + HSueno + PPractica + ActExtra
# R² = 0.14, F(4,9995) = 419.82
# Calificacion ~ HEstudio + HSueno + PPractica
# R² = 0.14,F(3,9996) = 557.25
# Calificacion ~ HEstudio + HSueno
# R² = 0.14, F(2,9997) = 826.88 *
# Calificacion ~ HEstudio  + PPractica + ActExtra
# R² = 0.14, F(3,9996) = 549.35
# Calificacion ~ HEstudio  + PPractica
# R² = 0.14, F(2,9997) = 820.64 *
# Calificacion ~ HSueno + PPractica + ActExtra
# R² = 0.00, F(3,9996) = 16.07
summ(MLR1)
# los estimadores son validos porque p < 0.05 y se rechaza Ho que establece que los estimadores son = 0
# y se acepta H1 que dice que son diferentes de 0 y explican los efectos del modelo
# inicialmente sin h de estudio ni de prractica la calificacion comienza en 40.27
summary(MLR1)
# al aumentar 1 unidad de calificacion es porque se invirtio 2.77 h mas de estudio y 0.53 h de sue;o
# todas las p de las variables son < 0.05 asi es que se rechaza Ho donde se dice que el
# estimador es = 0 y se acepta H1 de que el estimador es <> 0
avPlots(MLR1)

MLR2 = lm(
  Calificacion ~ HEstudio  + PPractica,
  data = desempeno_tabla #%>%
  #    dplyr::filter(
  #      (Precio >=10000 & Precio <= 100000) &
  #        (Millas < 1000000) &
  #        (`Tam.Motor` < 10)
  #    )
)

summ(MLR2)
# los estimadores son validos porque p < 0.05 y se rechaza Ho que establece que los estimadores son = 0
# y se acepta H1 que dice que son diferentes de 0 y explican los efectos del modelo
# inicialmente sin h de estudio ni de sue;o la calificacion comienza en 37.86
summary(MLR2)
# los estimadores todos tienen p < 0.05 y entonces son validos y mayores a 0
# se rechaza Ho que establece que el estimador es 0 y se acepta H1 que dice que el estimador
# es <> 0. Por cada unidad de calificacion se invierte 2.76 h de estudio y 0.24 h de practicas
avPlots(MLR2)
# validacion del modelo ----
## Multicolinealidad ----
tabla_mod1 <- desempeno_tabla %>%
  dplyr::select(
    Calificacion,
    HEstudio,
    HSueno
  )
tabla_mod1 %>% cor()

tabla_mod2 <- desempeno_tabla %>%
  dplyr::select(
    Calificacion,
    HEstudio,
    PPractica
  )
tabla_mod2 %>% cor()

## Factor de inflacion de varianza ----
vif(MLR1)
vif(MLR2)
## indice de condicion (mas importante) ----
eigprop(MLR1)
eigprop(MLR2)
## Autocorrelacion ----
dwtest(MLR1)
dwtest(MLR2)
## prueba de autocorrelacion de cualquier orden ----
bgtest(MLR1)
bgtest(MLR2)

cor(tabla_mod1, MLR1$residuals)
cor(tabla_mod2, MLR2$residuals)
## Heterocedasticidad ----
bptest(MLR1)
bptest(MLR2)

gqtest(MLR1, point = 0.5)
gqtest(MLR2, point = 0.5)
## normalidad ----
jarque.bera.test(MLR1$residuals)
adf.test(MLR1$residuals)

### normalidad ----
jarque.bera.test(MLR2$residuals)
adf.test(MLR2$residuals)
