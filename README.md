# Gráficas de crecimiento diario de covid-19 en México con R.
Los datos abiertos de covid 19 de México están disponibles en internet de manera diaria en  [Datos Abiertos covid-19  México](http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip),desafortunadamente el archivo es tan grande que es imposible manejarlo con Excel, lo que limita su uso.

Para sobrepasar este impedimento, elaboré un script en el lenguaje R que sumariza los datos y  genera gráficas y archivos csv por estado y por ciudad.

También se calculan los valores del [Ritmo reproductivo básico] (https://es.wikipedia.org/wiki/Ritmo_reproductivo_b%C3%A1sico) para México, los estados y zonas metropolitanas.


## Archivos generados

### Para el país, cada uno de los estados de México y ciudades principales se generan diez archivos:

1. Gráfica con casos acumulados y los nuevos casos.
<br>Esta gráfica está inspirada en [Ourworld in data](https://ourworldindata.org/grapher/daily-new-confirmed-cases-of-covid-19-vs-cumulative-cases-positive-rate).
<br>En esta gráfica se muestra el cambio semanal de manera rápida, se muestra si hay tendencias de crecimiento o disminución así como se puede ver los máximos y mínimos a lo largo del tiempo.  
	- Archivo *Nombre-Acumvscurrent-Confirmed-New-cases-Acum-7daysAvg.png*
<p align="left">
  <img src="./img/01-AcumuladovsPromedio7dias/Mexico-Acumvscurrent-Confirmed-New-cases-Acum-7daysAvg.png" alt="Grafica comparativa Mexico" width="200">
</p>

2. Curva epidémica de casos confirmados.
<br>Es la curva clásica para ver la evolución de la pandemia.
<br>Esta curva es diaria donde se ve el hecho de que los fines de semana y asuetos hay menos casos que durante la semana. Esta curva permita ver el cambio de manera precisa, pero es mejor usar las curvas semanales para observar las tendencias.
	- Archivo *Nombre-Casos-Confirmed-New-cases-Acum-7daysAvg.png*
<p align="left">
  <img src="./img/02-CasosPromedio7dias/Mexico-Casos-Confirmed-New-cases-Acum-7daysAvg.png" alt="Grafica comparativa Mexico" width="200">
</p>

3. Gráfica de la velocidad de la epidemia semanal (promedio móvil de 7 días de casos positivos comparado con la semana anterior).
<br>La velocidad se calcula diariamente, tomadno el acumulado de la semana anterior.
<br>Cuando en la gráfica se ven valores debajo de cero, hay una disminución de casos, valores arriba de cero, hay incremento.
<br>Pude encontrar una visualización similar en [Datos de inglaterra del 9 de junio](https://www.cebm.net/covid-19/covid-19-death-data-in-england-update-9th-june) así como en la manera en que se muestran los datos económicos.
	- Archivo *Nombre-CasosVsSemAnt-Confirmed-New-cases-Acum-7daysAvg.png*
<p align="left">
  <img src="./img/04-velocidad/Mexico-CasosVsSemAnt-Confirmed-New-cases-Acum-7daysAvg.png" alt="Grafica comparativa Mexico" width="200">
</p>

4. Archivo separado por comas (CSV) con los datos de las tres gráficas anteriores
	- Archivo *Nombre-Confirmed-New-cases-Acum-7daysAvg.csv*
	
5. Archivo CSV con los datos de mortalidad diario y promedio móvil de 7 días (velocidad).
	- Archivo *Nombre-Mortality.csv*

6. Gráfica con los datos de mortalidad en un promedio móvil de 7 días.
<br>Muestra la velocidad de la mortalidad de la epidemia, se calcula diariamente, con los datos de la semana anterior.
	- Archivo *Nombre-Mortality.png*
<p align="left">
  <img src="./img/06-mortality/Mexico-Mortality.png" width="200">
</p>

7. Archivo CSV con el cálculo de ritmo reproductivo básico.
	- Archivo *Nombre-R Estimate.csv*

8. Gráfica del ritmo reproductivo básico.
<br>Se calcula con el paquete EpiEstim, el cual es el más reconocido para el cálculo de este indicador. La imagen tiene tres gráficas, la curva epidémica, el valor de R a lo largo del tiempo, valores mayores de uno, hay un crecimiento y menor de uno, indica un decrecimiento, El valor de R es muy sensible a los cambios en la semana anterior, por lo que se debe comparar con las demás gráficas.
	- Archivo *Nombre-R Estimate.png*
<p align="left">
  <img src="./img/08-r0/Mexico-R Estimate.png" width="200">
</p>

9. Gráfica de los casos por 100 mil habitantes por cada entidad o municipio.
<br>Esta gráfica permite comparar la epidemia en los estados y ciduades bajo el mismo parámetro.
	- Archivo *Nombre-CasosX100k-Confirmed-New-cases-Acum-7daysAvg*
<p align="left">
  <img src="./img/05-casosx100k/Mexico-CasosX100k-Confirmed-New-cases-Acum-7daysAvg.png" width="200">
</p>

10. Gráfica de la aceleracion de  casos (diferencia de velocidad en una semana).
<br>La aceleración es el cambio de velocidad de los casos de la semana, la aceleración combinada con el número de casos existentes  puede indicar que tan grave es la epidemia en un momento dado.
	- Archivo *Nombre-CasosVsSemAnt Accel-Confirmed-New-cases-Acum-7daysAvg.png*
<p align="left">
  <img src="./img/03-Aceleracion/Mexico-CasosVsSemAnt Accel-Confirmed-New-cases-Acum-7daysAvg.png" width="200">
</p>


### Comparativo de estados de México y ciudades principales

1. Gráfica consolidado de los valores más recientes del ritmo reproductivo básico.
<br>Se puede comparar los valores más reciente de R(t) de cada estado y zona metropolitana evaluada. El valor de R(t) es un indicador temprano del crecimiento/disminución el cual es muy afectado por la tendencia de la última semana.
	- Archivo *All-R0.png*
<p align="left">
  <img src="./img/All-R0.png" width="200">
</p>

2. Reporte de los valores más recientes del ritmo reproductivo básico.
	- Archivo *All-R0.csv*

Todas las gráficas se pueden ver en:
<a href="./img"> Liga de imágenes </a>

## Tablero de PowerBI.
En el directorio "Tablero" se tiene un archivo que toma la información local de México y lo muestra de manera gráfica interactiva.

## Instalación de R en windows y uso del script
Para correr el script en windows se puede usar R portable

* Se instala [R portable](https://portableapps.com/node/32898)

* Se Abre el archivo "crecimientoDiario.r"

* Se edita la línea *mydir <-  '/Users/joaquin/Documents/Mios2020/covid/FechaIngreso-crecimientodiario'*
<br>Para que aplique al directorio donde se encuentre el script, usando / como separador de directorio.

**En Linux y Mac (usando el paquete de R de la distribución),  solamente se debe instalar una vez a devtools y EpiEstim.**


## Librerías
Se utiliza el paquete [EpiEstim de R] (https://cran.r-project.org/web/packages/EpiEstim/index.html), el cual es el algoritmo más reconocido para el cálculo del valor de R.
