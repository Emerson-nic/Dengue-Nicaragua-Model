[English Version](README.md)

# Dengue-Nicaragua-Model

Análisis estadístico de los factores asociados con brotes de dengue en Nicaragua a nivel departamental. Este proyecto utiliza datos semanales de vigilancia epidemiológica y covariables climáticas para estimar la probabilidad de que una semana específica sea clasificada como brote dentro de cada departamento. El objetivo es la inferencia sobre asociaciones, no la predicción.

## Qué hace el modelo

La variable de respuesta `brote` es binaria. Se construye bajo el enfoque de canal endémico (OPS): una semana se clasifica como brote (`1`) cuando los casos reportados superan el percentil 75 de la distribución histórica para ese departamento específico.

El modelo es un Modelo Lineal Mixto Generalizado (GLMM) con función de enlace logit, ajustado con `glmmTMB`. Incluye estacionalidad mensual, tendencia anual de largo plazo, inercia epidémica y una estructura AR(1) para capturar la dependencia temporal.

### Fórmula

```r
brote ~ nino34_12s + Rain_acc3 + Temperature + ln_casos_semana_anterior +
    mes + year +
    (1 | DEPARTAMENTO) + ar1(tiempo_factor + 0 | DEPARTAMENTO)
```

Componentes principales:

- Efectos fijos: promedio móvil de 12 semanas de la anomalía de SST en Niño 3.4 (`nino34_12s`), lluvia acumulada de 3 semanas, temperatura media, logaritmo de los casos de la semana anterior, mes (`mes`) y año (`year`).
- Intercepto aleatorio por departamento, para absorber la heterogeneidad regional no observada.
- Estructura residual AR(1) dentro de cada departamento, para capturar la memoria de transmisión de una semana a otra.

## Datos

Registros semanales de 18 departamentos para 2014-2022. La serie con imputación de brechas tiene 8,316 departamento-semanas (462 semanas por departamento) sin valores faltantes. Después de construir los rezagos y la lluvia acumulada de 3 semanas, quedan 8,280 departamento-semanas para el modelo. Las variables incluyen conteo de casos, temperatura, lluvia, lluvia acumulada en 3 semanas, anomalía de Niño 3.4, casos de la semana anterior y su transformación logarítmica, mes y año.

## Hallazgos principales

- Los casos de la semana anterior dominan la asociación. Duplicarlos multiplica los odds de un brote por 16.77 (IC 95%: 13.50 a 20.83).
- El Niño no muestra una asociación independiente robusta. El odds ratio es 1.141 (IC 95%: 0.908 a 1.434; p = 0.256) con la tendencia anual en el modelo, y 1.20 (IC 95%: 0.987 a 1.46; p = 0.067) sin ella.
- Cada milímetro adicional de lluvia acumulada en 3 semanas eleva los odds en aproximadamente 0.24% (OR = 1.00239; IC 95%: 1.00098 a 1.00380; p = 0.00091). Un aumento de 100 mm eleva los odds en aproximadamente 27% (IC 95%: 10% a 46%).
- Cada grado Celsius adicional eleva los odds en aproximadamente 27% (OR = 1.271; IC 95%: 1.091 a 1.481; p = 0.0021).
- El coeficiente AR(1) es 0.960, lo que significa que la transmisión en una semana dada se hereda casi por completo de la semana anterior dentro del mismo departamento.
- No hay problemas de colinealidad: el VIF máximo es 4.65 para mes y 2.17 para año, y los VIF ajustados se mantienen por debajo de 2.2.
- Los diagnósticos de residuales con DHARMa pasan las pruebas de uniformidad (p = 0.31), dispersión (p = 0.69) y valores atípicos con bootstrap (p = 0.12). La prueba de Durbin-Watson sobre residuales agregados da 1.86 (p = 0.13), descartando autocorrelación temporal sobrante.
- Estos resultados describen asociaciones y no deben interpretarse como un modelo predictivo o de pronóstico.

### Autores

Emerson Lopez - Autor principal / Mantenedor

## Citación

Si utilizas este modelo o el código de este repositorio, por favor cítalo como:

```text
Lopez, E. (2026). Dengue-Nicaragua-Model: Un modelo lineal mixto generalizado para el riesgo de brotes de dengue en Nicaragua [Código fuente]. GitHub. [https://github.com/Emerson-nic/Dengue-Nicaragua-Model](https://github.com/Emerson-nic/Dengue-Nicaragua-Model)
```
