Aqui se describen las carpetas y los archivos de interés:

0-preparacionDatos: Una carpeta con el analisis previo de los datos  y su pre-procesado.

1-qgis:
	datos_fijos: elementos de qgis que solo hay que calcular 1 vez 
	Mapa1: Tiene una carpeta por cada gas contaminante con los elemetos de qgis propios de ese gas y del mapa 1.
	Mapa2: Tiene una carpeta por cada gas contaminante con los elemetos de qgis propios de ese gas y del mapa 2

2-shiny:
	www: Carpeta con todos los archivos utilizados (excepto elementos de qgis y csv). En esta carpeta se hayan la MEMORIA y el VIDEO
	app.R: Aplicación de shiny

data: CSV iniciales (se usan en el pre-procesado)

*Porfavor, aseguresé de tener la última versión de shiny (<=1.7.4) actualizada*