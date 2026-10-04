[English Version](README.md)

# Dengue-Nicaragua-Model

Modelo estadístico para predecir brotes de dengue en Nicaragua a nivel departamental. Este proyecto utiliza datos semanales de vigilancia epidemiológica y covariables climáticas para estimar la probabilidad de que una semana específica cruce el umbral de brote dentro de cada departamento.

## Qué hace el modelo

La variable de respuesta `is_brote` es binaria. Se construye bajo el enfoque de canal endémico (OPS): una semana se clasifica como brote (`1`) cuando los casos reportados superan el percentil 75 de la distribución histórica para ese departamento y semana específica.

El modelo es un Modelo Lineal Mixto Generalizado (GLMM) con función de enlace logit, ajustado con `glmmTMB`. Se probaron otras funciones de enlace pero se descartaron al no pasar los diagnósticos de residuales. La especificación logit es el modelo final.

### Fórmula

```r
is_brote ~ Niño + Rain_acc3 + Temperature + ln_casos_semana_anterior +  
    (1 | DEPARTAMENTO) + ar1(tiempo_factor + 0 | DEPARTAMENTO)
```

Componentes principales:

- Efectos fijos: Indicador de El Niño, lluvia acumulada de 3 semanas, temperatura media y el logaritmo de los casos de la semana anterior.
- Intercepto aleatorio por departamento, para absorber la heterogeneidad regional no observada.
- Estructura residual AR(1) dentro de cada departamento, para capturar la memoria de transmisión de una semana a otra.

## Datos

Registros semanales de 18 departamentos, a partir de 2014. Las variables incluyen conteo de casos, temperatura, lluvia, lluvia acumulada en 3 semanas y un indicador ENSO. Los casos de la semana anterior y su transformación logarítmica se incluyen como control de la inercia epidémica.

## Hallazgos principales

- Los casos de la semana anterior dominan la predicción. Duplicarlos multiplica los odds de un brote por aproximadamente 20 (IC 95%: 16 a 24).
- Las semanas con El Niño muestran un 49% más de odds de brote en comparación con semanas regulares.
- Cada milímetro adicional de lluvia acumulada en 3 semanas eleva los odds en aproximadamente 0.3%.
- La temperatura es solo marginalmente significativa (p = 0.078). Es muy probable que esto ocurra porque las temperaturas en los departamentos de Nicaragua se mantienen todo el año en un rango que favorece el desarrollo del *Aedes aegypti*, por lo que la variable tiene poco poder discriminativo.
- El coeficiente AR(1) es 0.96, lo que significa que la transmisión en una semana dada se hereda casi por completo de la semana anterior dentro del mismo departamento.
- No hay problemas de colinealidad (VIF cercano a 1 para todos los predictores).
- Los diagnósticos de residuales con DHARMa pasan las pruebas de uniformidad, dispersión y valores atípicos. La prueba de Durbin-Watson sobre residuales agregados da p = 0.21, descartando autocorrelación temporal sobrante.

### Autores

Emerson Lopez - Autor principal / Mantenedor

Coautor: [por agregar]

## Citación

Si utilizas este modelo o el código de este repositorio, por favor cítalo como:

```text
Lopez, E. (2026). Dengue-Modelo-Nicaragua: Un modelo lineal mixto generalizado para la predicción de brotes de dengue en Nicaragua [Código fuente]. GitHub. [https://github.com/Emerson-nic/Dengue-Nicaragua-Model](https://github.com/Emerson-nic/Dengue-Nicaragua-Model)
```