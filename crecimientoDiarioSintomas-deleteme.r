#http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
#library("RCurl")
#library(ggplot2)
remove(list = ls())
mydir <-  '/media/joaquin/Nuevo_vol/misdoc/Mios2020/covid19/crecimientodiario'
myurl <-  'http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip'
myfile <- 'datos_abiertos_covid19.zip'
resultadoConfirmado  = 1


#calculate the average of last x days for a specific column on a data frame and returs a df

promedioxdias <- function(casospromDF,  dias, nombreColumna){
	#fill vector with zeroes
	promedios<- rep(0, length(casospromDF[,1]))
	#we are going to calculate starting at dias  position, to correcly calculate the average
	observaciones <-  c((dias+1):length (casospromDF[,1]) )

	for (i in observaciones ) {
		suma <-0
		ventana <-c( (i - dias) : i)
		for (j in ventana ) {
			suma= casospromDF[j, nombreColumna] + suma
		}
		prom <-  suma / dias
		promedios[i] = prom
	}

	return(promedios)
}

#plot the new cases vs accumulated , using an average of x days and cutti some days due of incomplete data
graficaEstado <- function(mxCasesDF , estado,estadoTxt,aretirar, daysToAverage, saveToFile,pathToSave){
	edoCasesDF <- mxCasesDF[mxCasesDF$ENTIDAD_RES == estado & mxCasesDF$RESULTADO == resultadoConfirmado,c("FECHA_SINTOMAS","RESULTADO")]
	casos <- aggregate(formula = RESULTADO ~ FECHA_SINTOMAS,
	           FUN = sum,
	           data = edoCasesDF)

	#add running sum
	casos[,"RESULTADO_ACUM"] <- cumsum(casos$RESULTADO)
	casos <-head(casos,-aretirar)

	casos[,"RESULTADO7D"]<-promedioxdias(casos, daysToAverage ,"RESULTADO")
	casos[,"RESULTADO_ACUM7D"]<-promedioxdias(casos, daysToAverage ,"RESULTADO_ACUM")


#	 plot

	if (saveToFile == TRUE) {
	#save
	# 1. Open jpeg file
		setwd(pathToSave)
		png(paste(estadoTxt, "Sintomas.png", sep=""), width = 800, height = 600)
	# 2. Create the plot
	}
	plot(x = casos$RESULTADO_ACUM7D, y = casos$RESULTADO7D ,xlab = "Acumulados", ylab="Nuevos",main=paste(estadoTxt," Por Fecha de Sintoma"), log="xy")
	with (casos, lines(x = casos$RESULTADO_ACUM7D, y = casos$RESULTADO7D,col="red"))

	if (saveToFile == TRUE) {
		# 3. Close the file

                setwd(pathToSave)
                write.csv(casos, paste(estadoTxt,"Sintomas.csv", sep=""))

		dev.off()
	}

 result <-1
 return(result)
}



#download and load into dataframe
setwd(mydir)
download.file(myurl, myfile )
unzipfile <- unzip (myfile, list = TRUE)
unzip (myfile, unzipfile$Name)
mxCasesDF <- read.csv ( file=unzipfile$Name)

#fill state names
listaEstados <- c(1:32)
nombreEstados <-c ("AGUASCALIENTES","BAJA_CALIFORNIA","BAJA_CALIFORNIA_SUR","CAMPECHE",	"COAHUILA",	"COLIMA","CHIAPAS",	"CHIHUAHUA","CDMX",	"DURANGO","GUANAJUATO",	"GUERRERO",	"HIDALGO","JALISCO","ESTADO_DE_MEXICO","MICHOACAN","MORELOS","NAYARIT","NUEVO_LEON","OAXACA","PUEBLA","QUERETARO","QUINTANA_ROO","SAN_LUIS_POTOSI","SINALOA","SONORA","TABASCO","TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS")

#run graphs increment vs acumulated

for (i in 1:length(listaEstados)) {

#dataframe with country weide cases, stane number, state name	, days to cut-off, cases' number of days to average  , savetoFile, path
  graficaEstado (mxCasesDF , listaEstados[[i]],nombreEstados[[i]],5, 7, TRUE, paste(mydir,"/img",sep=""))
}
