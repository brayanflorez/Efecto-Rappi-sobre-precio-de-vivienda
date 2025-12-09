# El efecto Rappi: cómo la economía del delivery reconfigura el valor del espacio urbano

Este repositorio contiene el código y los insumos utilizados para el estudio que analiza cómo la adopción de **Rappi** por parte de restaurantes existentes afecta los **precios de vivienda** en Bogotá. El proyecto combina datos geoespaciales reales con un proceso generador de datos simulado y un diseño econométrico de *difference-in-differences* (DiD) con adopción escalonada.

---

## 📌 Pregunta de investigación

¿Cómo afecta el aumento en el flujo de entregas a domicilio —inducido por la adopción de Rappi por parte de restaurantes existentes— los precios de vivienda en Bogotá?

Esta pregunta es relevante para la economía urbana, pues las plataformas digitales generan shocks localizados que pueden reconfigurar las amenidades y la valorización urbana.

# El efecto Rappi: cómo la economía del delivery reconfigura el valor del espacio urbano

Este repositorio contiene el código y los insumos utilizados para el estudio que analiza cómo la adopción de **Rappi** por parte de restaurantes existentes afecta los **precios de vivienda** en Bogotá. El proyecto combina datos geoespaciales reales con un proceso generador de datos simulado y un diseño econométrico de *difference-in-differences* (DiD) con adopción escalonada.

---

## 📌 Pregunta de investigación

¿Cómo afecta el aumento en el flujo de entregas a domicilio —inducido por la adopción de Rappi por parte de restaurantes existentes— los precios de vivienda en Bogotá?

Esta pregunta es relevante para la economía urbana, pues las plataformas digitales generan shocks localizados que pueden reconfigurar las amenidades y la valorización urbana.

---

## 🗂️ Datos

El proyecto combina:

- Precios de vivienda simulados mediante un modelo hedónico calibrado con datos reales.  
- Ubicación espacial de viviendas, estratos socioeconómicos y sectores catastrales.  
- Información georreferenciada de restaurantes (OpenStreetMap).  
- Variables de amenidades urbanas: distancia a parques, hospitales, colegios y estaciones de TransMilenio.  
- Proceso simulado de adopción de Rappi (2015–2021), dependiente de densidad poblacional, densidad comercial y estratos.

Todas las unidades se asignan a una cuadrícula uniforme de **500×500 metros**, siguiendo la metodología de McMillen, Sarmiento-Barbieri y Singh (2019).

---

## 🧠 Metodología

La estrategia empírica utiliza:

- **Grillas espaciales** homogéneas para asegurar comparabilidad entre unidades territoriales.  
- Clasificación de celdas en `Treated`, `Neighbor` y `Control`.  
- Un diseño de **DID con múltiples periodos** (Callaway & Sant’Anna, 2021).  
- Efectos fijos de celda y año.  
- Inferencia mediante errores estándar agrupados a nivel de grilla.

---

---

## 🔄 Reproducibilidad

1. Instalar las librerías utilizadas (`tidyverse`, `sf`, `fixest`, `did`, etc.).  
2. Ejecutar el script:

```r
source("scripts/01_data_cleaning_and_analysis.R")
Ejecutar los modelos:

r
Copy code
source("scripts/02_results_and_estimation.R")
Todos los resultados, figuras y tablas se guardan automáticamente en figures/ y outputs/.

📈 Resultados principales
La adopción de Rappi por parte de restaurantes aumenta el precio de la vivienda en aprox. 3% en las celdas tratadas.

Las celdas vecinas muestran efectos positivos menores, consistentes con spillovers espaciales.

Los análisis de event study muestran ausencia de anticipación y validan el supuesto de tendencias paralelas.

Los efectos son locales y persistentes, en línea con la literatura sobre capitalización de amenidades urbanas.

🧾 Referencias clave
McMillen, D., Sarmiento-Barbieri, I., & Singh, R. (2019). Do more eyes on the street reduce crime? Evidence from Chicago’s Safe Passage Program. Journal of Urban Economics, 110, 1–25.

Callaway, B., & Sant’Anna, P. H. C. (2021). Difference-in-Differences with Multiple Time Periods. Journal of Econometrics, 225(2), 200–230.

👨‍💻 Autor
David Florez y Daniel Hernandez
Universidad de los Andes – Economía Urbana
Contacto: b.florezl@uniandes.edu.co

📄 Licencia
Este repositorio puede ser utilizado con fines académicos y de investigación. Citar adecuadamente si se reutiliza el código o los datos.

