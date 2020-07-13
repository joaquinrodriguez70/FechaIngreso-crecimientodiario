#http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip

#omited for 3.6
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
graficaMunicipios <- function(mxCasesDF , numEstado,numMuni, muniTxt,aretirar, daysToAverage, saveToFile,pathToSave){
	edoCasesDF <- mxCasesDF[mxCasesDF$ENTIDAD_RES %in% numEstado &mxCasesDF$MUNICIPIO_RES %in% numMuni & mxCasesDF$RESULTADO == resultadoConfirmado,c("FECHA_SINTOMAS","RESULTADO")]
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
		png(paste(muniTxt, "Sintomas.png", sep=""), width = 800, height = 600)
	# 2. Create the plot
	}
	plot(x = casos$RESULTADO_ACUM7D, y = casos$RESULTADO7D ,xlab = "Acumulados", ylab="Nuevos",main=paste(muniTxt,"Por fecha de sintomas"), log="xy")
	with (casos, lines(x = casos$RESULTADO_ACUM7D, y = casos$RESULTADO7D,col="red"))

	print(casos)
	if (saveToFile == TRUE) {
		# 3. Close the file
		setwd(pathToSave)
		write.csv(casos, paste(muniTxt,"Sintomas.csv", sep=""))
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

nombreMunicipios <-c ("ZM_Tijuana")
numeroMunicipios <-c (4)
numeroEstados<-c(2)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_Villahermosa"
numeroMunicipios <-c (4)
numeroEstados<-c(27)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_Cancun"
numeroMunicipios <-c (5)
numeroEstados<-c(23)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7,TRUE , paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_Monterrey"
numeroMunicipios <-c (39,6,9,21,18,19.26,31,45,46,48,49)
numeroEstados<-c(19)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7,TRUE , paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_Veracruz"
numeroMunicipios <-c (193,11,28,90,105)
numeroEstados<-c(30)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_CDMX"
numeroMunicipios <-c (9,2,10,11,13,15,16,17,20,22,23,24,25,28,29,30,31,33,34,35,36,37,38,39,44,46,50,53,57.58,59,60,61,68,69,70,75,81,83,84,89,91,93,94,95,96,99,100,103,104,108,109,112,120,121,122,125)
numeroEstados<-c(15,9)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))





#guadalajara
nombreMunicipios <-"ZM_GDL"
numeroMunicipios <-c (39,120,4,98,101,70,124,44,51,2)
numeroEstados<-c(14)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))


#acapulco

nombreMunicipios <-"ZM_ACAPULCO"
numeroMunicipios <-c (1,21)
numeroEstados<-c(12)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))




nombreMunicipios <-"ZM_MORELIA"
numeroMunicipios <-c (22,53,88)
numeroEstados<-c(16)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))



nombreMunicipios <-"ZM_LAZARO_CARDENAS"
numeroMunicipios <-c (52)
numeroEstados<-c(16)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_OAXACA"
numeroMunicipios <-c (174,67,83,87,91,107,115)
numeroEstados<-c(20)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))

#puebla
#
nombreMunicipios <-"ZM_PUEBLA"
numeroMunicipios <-c (114)
numeroEstados<-c(21)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_MEXICALI"
numeroMunicipios <-c (2)
numeroEstados<-c(2)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))



nombreMunicipios <-"ZM_HERMOSILLO"
numeroMunicipios <-c (30)
numeroEstados<-c(26)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))




nombreMunicipios <-"ZM_TUXTLA_GUTIERREZ"
numeroMunicipios <-c (12,27,86,79,63,101)
numeroEstados<-c(7)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))




nombreMunicipios <-"ZM_CULIACAN"
numeroMunicipios <-c (6,18)
numeroEstados<-c(25)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_MAZATLAN"
numeroMunicipios <-c (12)
numeroEstados<-c(25)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))




nombreMunicipios <-"ZM_AGUASCALIENTES"
numeroMunicipios <-c (1,5,11)
numeroEstados<-c(1)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_LEON"
numeroMunicipios <-c (20,37)
numeroEstados<-c(11)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))



nombreMunicipios <-"ZM_MERIDA"
numeroMunicipios <-c (50)
numeroEstados<-c(31)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_CD_JUAREZ"
numeroMunicipios <-c (37)
numeroEstados<-c(8)
#dataframe with country wide cases, stane number, county number , countyName , days to cut-off, cases' number of days to average  , savetoFile, path
graficaMunicipios (mxCasesDF , numeroEstados,numeroMunicipios,nombreMunicipios,5, 7, TRUE , paste(mydir,"/img",sep=""))
