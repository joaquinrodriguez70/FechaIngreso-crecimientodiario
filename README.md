## Gráficas de crecimiento diario de covid-19 en México con R.
Los datos abiertos de covid 19 de México están disponibe de manera diaria  [Datos Abiertos covid-19  México](http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip), sin embargo el archvio es más grande que lo que puede manejar Excel, lo que limita su uso.

Para facilitar su uso, elaboré un script en el lenguaje R que recupera el archivo diario de casos y generar gráficas y archivos csv con sumarizados por estado y zonas metropolitanas seleccionadas de México así como los valores del [Ritmo reproductivo básico] (https://es.wikipedia.org/wiki/Ritmo_reproductivo_b%C3%A1sico).


## Archivos generados 

### Se geenran siete archivos, Para el país, cada uno de los estados de México y ciudades principales

- Gráfica con casos acumulados y los nuevos casos. Esta gráfica está inspirada en [Ourworld in data](https://ourworldindata.org/grapher/daily-new-confirmed-cases-of-covid-19-vs-cumulative-cases-positive-rate) y permite ver la evolución de los casos, en el extremo derecho de la curva se puede ver la tendencia actual, ya sea que se de un incremento  o disminución.
	- Archivo *Nombre-Acumvscurrent-Confirmed-New-cases-Acum-7daysAvg.png*
<p align="left">
  <img src="./img/Mexico-Acumvscurrent-Confirmed-New-cases-Acum-7daysAvg.png" alt="Grafica comparativa Mexico" width="200">
</p>

- Curva epidémica de casos confirmados.
	- *Nombre-Casos-Confirmed-New-cases-Acum-7daysAvg.png*
- Gráfica de promedio móvil de 7 días de casos positivos comparado con la semana anterior, inspirado en [Datos de inglaterra del 9 de junio](https://www.cebm.net/covid-19/covid-19-death-data-in-england-update-9th-june)
	- *Nombre-CasosVsSemAnt-Confirmed-New-cases-Acum-7daysAvg.png*
- Archivo separado por comas (CSV) con los datos de las tres gráficas anteriores
	- *Nombre-Confirmed-New-cases-Acum-7daysAvg*
- Archivo CSV con los datos de mortalidad diario y promedio móvil de 7 días.
	- *Nombre-Mortality.csv*
- Gráfica con los datos de mortalidad en un promedio móvil de 7 días.
	- *Nombre-Mortality.png*
- Archivo CSV con el cálculo de ritmo reproductivo básico.
	- *Nombre-R Estimate.csv*
- Gráfica del ritmo reproductivo básico.
	- *Nombre-R Estimate*
	
	
### Comparativo de estados de México y ciudades principales

- Consolidado de los valores más recientes del ritmo reproductivo básico.
	

## Librerías
Se utiliza el paquete [EpiEstim de R] (https://cran.r-project.org/web/packages/EpiEstim/index.html), el cual 