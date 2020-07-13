#Script which calculates the

#a)Moving Average CSV and Plot Generation for confirmed covid cases in Mexico by detection Date (FECHA_INGRESO)
#  Considering confirmed New cases and accumulated , using an average of x days and cutting off 7 days due of incomplete data
#  giving a CSV as a result
#  , "","FECHA_INGRESO","RESULTADO","RESULTADO_ACUM","RESULTADO7D","RESULTADO_ACUM7D"


# Gets data from
#http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
remove(list = ls())
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

#Omited for 3.6
#install.packages("Rcurl")
#install.packages("ggplot2")
#library("RCurl")
#library(ggplot2)



library(devtools)
library(EpiEstim)


mydir <-  '/media/joaquin/Nuevo_vol/misdoc/Mios2020/covid19/FechaIngreso-crecimientodiario'
myurl <-  'http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip'
myfile <- 'datos_abiertos_covid19.zip'
resultadoConfirmado  = 1
daysForGraphToCutOff = 7
movingAverageDays = 7
sinDefuncion = '9999-99-99'

##############################################
#calculate the average of last x days for a specific column on a data frame and returns a df
##############################################
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

##############################################
#Moving Average CSV and Plot Generation,
#Confirmed New cases and accumulated , using an average of x days and remove some days due of incomplete data
##############################################
generateCSVandPlotForConfirmedCasesMovingAverage <- function(casesDataFrame , estadoTxt,aretirar, daysToAverage, saveToFile,pathToSave,fileNameTxtIdent){

	aggregateCasesDataFrame <- aggregate(formula = RESULTADO ~ FECHA_INGRESO,FUN = sum, data = casesDataFrame)

	#add running sum
	aggregateCasesDataFrame[,"RESULTADO_ACUM"]   <- cumsum(aggregateCasesDataFrame$RESULTADO)
	aggregateCasesDataFrame[,"RESULTADO7D"]      <- promedioxdias(aggregateCasesDataFrame, daysToAverage ,"RESULTADO")
	aggregateCasesDataFrame[,"RESULTADO_ACUM7D"] <- promedioxdias(aggregateCasesDataFrame, daysToAverage ,"RESULTADO_ACUM")

  dia_1 <- aggregateCasesDataFrame[1,"FECHA_INGRESO"]
# Generate Plots and save files
	setwd(pathToSave)
	if (saveToFile == TRUE) {

		# Open png file

		png(paste(estadoTxt, "-Acumvscurrent",fileNameTxtIdent ,".png", sep=""), width = 800, height = 600)
	}

	# Create the plot
	plot(x = head(aggregateCasesDataFrame$RESULTADO_ACUM7D,-aretirar), y = head(aggregateCasesDataFrame$RESULTADO7D,-aretirar) ,xlab = paste("Acumulados Confirmados promedio ", aretirar," dias"), ylab="Nuevos",main=estadoTxt, log="xy")
	with (aggregateCasesDataFrame, lines(x = head( aggregateCasesDataFrame$RESULTADO_ACUM7D,-aretirar), y = head(aggregateCasesDataFrame$RESULTADO7D, -aretirar),col="red"))

	if (saveToFile == TRUE) {

			dev.off()
			write.csv(aggregateCasesDataFrame,paste(estadoTxt,fileNameTxtIdent,".csv", sep=""))

      #EpidemicCurve Name

   		png(paste(estadoTxt, "-Casos",fileNameTxtIdent ,".png", sep=""), width = 800, height = 600)
	}

	#Create the plot
  barplot( head(aggregateCasesDataFrame$RESULTADO,-aretirar),names.arg=head(aggregateCasesDataFrame$FECHA_INGRESO,-aretirar),main=paste("Nuevos Ingresos",estadoTxt))

	if (saveToFile == TRUE) {
		# Close the file
		dev.off()
   }

		#######################
		#Restimate
		######################
		R_estimate <- estimate_R(head(aggregateCasesDataFrame$RESULTADO,-aretirar) ,method = "parametric_si",
										config = make_config(list(
										mean_si = 3.9, std_si = 4.5)))


		#	 plot

		if (saveToFile == TRUE) {
			setwd(pathToSave)
			png(paste(estadoTxt,"-R Estimate", ".png",sep=""), width = 800, height = 600)
		}
		plot(R_estimate,  options_R = list( xlab =paste("Tiempo",dia_1), ylab = "R"))

		if (saveToFile == TRUE) {
			dev.off()
			write.csv(R_estimate$R, paste(estadoTxt,"-R Estimate",".csv",sep=""))
		}

		print(R_estimate$R)



 result <-1
 return(result)
}

#######################################
#Deaths
#######################################

generateCSVandPlotForConfimedDeaths <- function(confirmedDeathsDataFrame , estadoTxt,aretirar,  daysToAverage,saveToFile,pathToSave){

# confirmedDeathsDataFrame[,"DIASPARALADEFUNCION"] <-as.numeric(as.Date(confirmedDeathsDataFrame[,"FECHA_DEF"])  - as.Date(confirmedDeathsDataFrame[,"FECHA_INGRESO"]))

#aggregate
  aggregateDeaths <- aggregate(formula = RESULTADO ~ FECHA_DEF,FUN = sum, data =confirmedDeathsDataFrame  )
#	survivalGroups  <- aggregate(formula = RESULTADO ~ DIASPARALADEFUNCION,FUN = sum, data =confirmedDeathsDataFrame  )

	#add running sum
	aggregateDeaths[,"RESULTADO_ACUM"]   <- cumsum(aggregateDeaths$RESULTADO)
	aggregateDeaths[,"RESULTADO7D"]      <- promedioxdias(aggregateDeaths, daysToAverage ,"RESULTADO")
	aggregateDeaths[,"RESULTADO_ACUM7D"] <- promedioxdias(aggregateDeaths, daysToAverage ,"RESULTADO_ACUM")


#	 plot

	if (saveToFile == TRUE) {
	#save
	# 1. Open png file
		setwd(pathToSave)
		png(paste(estadoTxt,"-Deaths" ,".png",sep=""), width = 800, height = 600)
	# 2. Create the plot
	}

  barplot( head(aggregateDeaths$RESULTADO,-aretirar),names.arg=head(aggregateDeaths$FECHA_DEF,-aretirar),main=paste("Deaths",estadoTxt))

	if (saveToFile == TRUE) {
		# 3. Close the file
		dev.off()
		write.csv(aggregateDeaths, paste(estadoTxt,"-Deaths",".csv",sep=""))
	}
	if (saveToFile == TRUE) {
	#save

	# 1. Open png file
		setwd(pathToSave)
		png(paste(estadoTxt,"-Survival" ,".png",sep=""), width = 800, height = 600)
    }
	  #survivalGroupsPlot<-hist(as.numeric(confirmedDeathsDataFrame$DIASPARALADEFUNCION),main ="Dias sobrevivencia",xlab="Dias",xlim=c(0,100),breaks=20,col="Gray")
		#survivalGroups<-cbind(survivalGroupsPlot$breaks,survivalGroupsPlot$counts)
		if (saveToFile == TRUE) {
			# 3. Close the file
			dev.off()
			#write.csv(survivalGroups, paste(estadoTxt,"-Survival",".csv",sep=""))
		}

	#print(aggregateDeaths)
	#print(survivalGroups)

 result <-1
 return(result)
}


#######################################
# Start here
#######################################
#download and load into dataframe
setwd(mydir)
if (FALSE) {
	download.file(myurl, myfile )
	unzipfile <- unzip (myfile, list = TRUE)
	unzip (myfile, unzipfile$Name)
	confirmedCasesDataFrame <- read.csv ( file=unzipfile$Name)
} else {
	confirmedCasesDataFrame <- read.csv ("/media/joaquin/Nuevo_vol/misdoc/Mios2020/covid19/FechaIngreso-crecimientodiario/backup/200711COVID19MEXICO.csv")
}
#fill state names
listaEstados <- c(1:32)
nombreEstados <-c ("AGUASCALIENTES","BAJA_CALIFORNIA","BAJA_CALIFORNIA_SUR","CAMPECHE",	"COAHUILA",	"COLIMA","CHIAPAS",	"CHIHUAHUA","CDMX",	"DURANGO","GUANAJUATO",	"GUERRERO",	"HIDALGO","JALISCO","ESTADO_DE_MEXICO","MICHOACAN","MORELOS","NAYARIT","NUEVO_LEON","OAXACA","PUEBLA","QUERETARO","QUINTANA_ROO","SAN_LUIS_POTOSI","SINALOA","SONORA","TABASCO","TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS")
poblacionEstados <-c(1184996,3155070,637026,822441,2748391,650555,4796580,3406465,8851080,1632934,5486372,3388768,2665018,7350682,15175862,4351037,1777227,1084979,4653458,3801962,5779829,1827937,1325578,2585518,2767761,2662480,2238603,3268554,1169936,7643194,1955577,1490668)


#######################################
#countrycases
#######################################
#Keep only confirmed cases
confirmedCasesDataFrame  <- confirmedCasesDataFrame [ confirmedCasesDataFrame$RESULTADO == resultadoConfirmado,c("FECHA_INGRESO","RESULTADO","ENTIDAD_RES","MUNICIPIO_RES","FECHA_DEF")]
confirmedDeathsDataFrame <- confirmedCasesDataFrame [ confirmedCasesDataFrame$FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","FECHA_INGRESO","RESULTADO")]

generateCSVandPlotForConfirmedCasesMovingAverage (confirmedCasesDataFrame , "Mexico", daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (confirmedDeathsDataFrame , "Mexico", daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


#######################################
#statewide
#######################################
for (i in 1:length(listaEstados)) {

#Arguments: dataframe with country wide cases, state number, state name	, cut-off days, cases' number of days to average  , savetoFile, path
  stateCasesDataFrame<- confirmedCasesDataFrame [ confirmedCasesDataFrame$RESULTADO==resultadoConfirmado & confirmedCasesDataFrame$ENTIDAD_RES == i, c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
	stateConfirmedDeathsDataFrame <- stateCasesDataFrame [ confirmedCasesDataFrame$FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]

  generateCSVandPlotForConfirmedCasesMovingAverage (stateCasesDataFrame, nombreEstados[[i]],daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
	generateCSVandPlotForConfimedDeaths (stateConfirmedDeathsDataFrame , nombreEstados[[i]], daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))

}


#######################################
#Municipal
#######################################
nombreMunicipios <-c ("ZM_Tijuana")
numeroMunicipios <-c (4)
numeroEstados<-c(2)

municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_Villahermosa"
numeroMunicipios <-c (4)
numeroEstados<-c(27)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_Cancun"
numeroEstados<-c(23)


numeroMunicipios <-c (5)
municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_Monterrey"
numeroMunicipios <-c (39,6,9,21,18,19.26,31,45,46,48,49)
numeroEstados<-c(19)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_Veracruz"
numeroMunicipios <-c (193,11,28,90,105)
numeroEstados<-c(30)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_CDMX"
numeroMunicipios <-c (9,2,10,11,13,15,16,17,20,22,23,24,25,28,29,30,31,33,34,35,36,37,38,39,44,46,50,53,57.58,59,60,61,68,69,70,75,81,83,84,89,91,93,94,95,96,99,100,103,104,108,109,112,120,121,122,125)
numeroEstados<-c(15,9)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


#guadalajara
nombreMunicipios <-"ZM_GDL"
numeroMunicipios <-c (39,120,4,98,101,70,124,44,51,2)
numeroEstados<-c(14)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


#acapulco

nombreMunicipios <-"ZM_ACAPULCO"
numeroMunicipios <-c (1,21)
numeroEstados<-c(12)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_MORELIA"
numeroMunicipios <-c (22,53,88)
numeroEstados<-c(16)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_LAZARO_CARDENAS"
numeroMunicipios <-c (52)
numeroEstados<-c(16)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_OAXACA"
numeroMunicipios <-c (174,67,83,87,91,107,115)
numeroEstados<-c(20)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))

#puebla
#
nombreMunicipios <-"ZM_PUEBLA"
numeroMunicipios <-c (114)
numeroEstados<-c(21)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))

nombreMunicipios <-"ZM_MEXICALI"
numeroMunicipios <-c (2)
numeroEstados<-c(2)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_HERMOSILLO"
numeroMunicipios <-c (30)
numeroEstados<-c(26)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_TUXTLA_GUTIERREZ"
numeroMunicipios <-c (12,27,86,79,63,101)
numeroEstados<-c(7)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_CULIACAN"
numeroMunicipios <-c (6,18)
numeroEstados<-c(25)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_MAZATLAN"
numeroMunicipios <-c (12)
numeroEstados<-c(25)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_AGUASCALIENTES"
numeroMunicipios <-c (1,5,11)
numeroEstados<-c(1)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_LEON"
numeroMunicipios <-c (20,37)
numeroEstados<-c(11)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))



nombreMunicipios <-"ZM_MERIDA"
numeroMunicipios <-c (50)
numeroEstados<-c(31)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


nombreMunicipios <-"ZM_CD_JUAREZ"
numeroMunicipios <-c (37)
numeroEstados<-c(8)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))


#JOIN TWO STATES

nombreMunicipios <-"ZM_Torreon"
numeroMunicipios <-c (35)
numeroEstados<-c(5)


municipalCasesDataFrame1 <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]


nombreMunicipios <-"ZM_Lerdo y Gomez"
numeroMunicipios <-c (12,7)
numeroEstados<-c(10)


municipalCasesDataFrame2 <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
nombreMunicipios <-"ZM_LA LAGUNA"

municipalCasesDataFrame <-rbind(municipalCasesDataFrame1,municipalCasesDataFrame2)
municipalConfirmedDeathsDataFrame <- municipalCasesDataFrame  [ municipalCasesDataFrame $FECHA_DEF!=sinDefuncion  ,c("FECHA_DEF","RESULTADO")]
generateCSVandPlotForConfirmedCasesMovingAverage (municipalCasesDataFrame, nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
generateCSVandPlotForConfimedDeaths (municipalCasesDataFrame , nombreMunicipios, daysForGraphToCutOff,  movingAverageDays,TRUE,paste(mydir,"/img",sep=""))
