#Script which calculates the
#CSV and Plot Generation for confirmed covid cases in Mexico by detection Date (FECHA_INGRESO)
#  Considering confirmed New cases and accumulated , using an average of x days and cutting off 7 days due of incomplete data
#  giving a CSV as a result
#  , "","FECHA_INGRESO","RESULTADO","RESULTADO_ACUM","RESULTADO_average7D","RESULTADO_averageACUM7D"
#R estimate for the above values

# Gets data from
#http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
#http://epidemiologia.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
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
myurl <-  'http://epidemiologia.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip'
myfile <- 'datos_abiertos_covid19.zip'
confirmedResult  = 1
daysForGraphToCutOff = 7
movingAverageDays = 7
notDead = '9999-99-99'
resultadosR = data.frame(stringsAsFactors = FALSE)

##############################################
#calculate the average of last week ( x days) for a specific column on a data frame and returns a data frame
##############################################
xDaysAverage <- function(averageCasesDataFrame,  days, columnName){
	#fill vector with zeroes
	averageVector<- rep(0, length(averageCasesDataFrame[,1]))
	#we are going to calculate starting at days  position, to correcly calculate the average
	measureVector <-  c((days+1):length (averageCasesDataFrame[,1]) )

	for (i in measureVector ) {
		sum <-0
		window <-c( (i - days) : (i-1))
		for (j in window ) {
			sum= averageCasesDataFrame[j, columnName] + sum

		}
		average <-  sum / days
		averageVector[i] = average
	}

	return(averageVector)
}


##############################################
#calculate the weekly ( last x days) for a specific column on a data frame and returns a dataframe
##############################################
sumxdays <- function(averageCasesDataFrame,  days, columnName){
	#fill vector with zeroes
	sums<- rep(0, length(averageCasesDataFrame[,1]))
	#we are going to calculate starting at days  position, to correcly calculate the average
	measureVector <-  c((days+1):length (averageCasesDataFrame[,1]) )

	for (i in measureVector ) {
		sum <-0
		window <-c( (i - days) : (i-1))
		for (j in window ) {
			sum= averageCasesDataFrame[j, columnName] + sum
		}
		sums[i] = sum
	}

	return(sums)
}


##############################################
#calculate the weekly ( last x days) for a specific column on a data frame and returns a dataframe
##############################################
Weeklynewcases <- function(averageCasesDataFrame,  days, columnName){
	#fill vector with zeroes
	sums<- rep(0, length(averageCasesDataFrame[,1]))
	#we are going to calculate starting at days  position, to correcly calculate the average
	measureVector <-  c((days+1):length (averageCasesDataFrame[,1]) )

	for (i in measureVector ) {
			sums[i]= averageCasesDataFrame[i, columnName] - averageCasesDataFrame[i-days, columnName]
	}

	return(sums)
}


#############################################
#Plot Accumulated VS Current cases
#############################################
generatePlotAccumulatedVSCurrent <- function(aggregateCasesDataFrame , estadoTxt,aretirar, daysToAverage, saveToFile,pathToSave,fileNameTxtIdent,poblacion){

  print(paste("Generando ->",estadoTxt))



	dia_1 <- aggregateCasesDataFrame[1,"FECHA_INGRESO"]
	maximorenglon <- length(aggregateCasesDataFrame[,"FECHA_INGRESO"])
	maximafecha <-aggregateCasesDataFrame[maximorenglon-aretirar,"FECHA_INGRESO"]

	# Generate Plot accumulated vs current
	setwd(pathToSave)

	# Open png file
	png(paste(estadoTxt, "-Acumvscurrent",fileNameTxtIdent ,".png", sep=""), width = 1024, height = 768)

	# Create the plot
	plot(x = head(aggregateCasesDataFrame$RESULTADO_averageACUM7D,-aretirar),
	     y = head(aggregateCasesDataFrame$RESULTADO_average7D,-aretirar) ,
			 xlab = paste("Acumulados Confirmados averageedio ", aretirar," days"),
			 ylab="Nuevos",main=paste(estadoTxt,dia_1,"a",maximafecha),
			 log="xy")
	with (aggregateCasesDataFrame, lines(x = head( aggregateCasesDataFrame$RESULTADO_averageACUM7D,-aretirar),
	                                     y = head(aggregateCasesDataFrame$RESULTADO_average7D,     -aretirar),col="red"))
	dev.off()

	write.csv(aggregateCasesDataFrame,paste(estadoTxt,fileNameTxtIdent,".csv", sep=""))

  #EpidemicCurve Name

  png(paste(estadoTxt, "-Casos",fileNameTxtIdent ,".png", sep=""), width = 1024, height = 768)

	#Create the plot
  barplot( head(aggregateCasesDataFrame$RESULTADO,-aretirar),
	         names.arg=head(aggregateCasesDataFrame$FECHA_INGRESO,-aretirar),
					 main=paste("Nuevos Ingresos",estadoTxt,dia_1,"a",maximafecha),
					 las=2,
					 col ="#0066cc")

	dev.off()

	#weeklyChange curve

	colores = ifelse( head(aggregateCasesDataFrame$RESULTADO_DIFSUM7D,-aretirar) > 1 ,rgb(0.2,0.4,0.6,0.6), "#69b3a2")
	png(paste(estadoTxt, "-CasosVsSemAnt",fileNameTxtIdent ,".png", sep=""), width = 1024, height = 768)

	#Create the plot
	barplot( head(aggregateCasesDataFrame$RESULTADO_DIFSUM7D,-aretirar),
					 names.arg=head(aggregateCasesDataFrame$FECHA_INGRESO,-aretirar),
					 main=paste("Nuevos Ingresos esta semana vs semana anterior",estadoTxt,dia_1,"a",maximafecha),
					 las=2,
					 col = colores)

	dev.off()

}


#############################################
#Calculate R Estimation and Plot
#############################################
generateRandPlot<- function(aggregateCasesDataFrame , estadoTxt,aretirar, daysToAverage, saveToFile,pathToSave,fileNameTxtIdent,poblacion){


		#######################
		#Restimate
		######################
		R_estimate <- estimate_R(head(aggregateCasesDataFrame$RESULTADO,-aretirar) ,method = "parametric_si",
											config = make_config(list(
											mean_si = 3.9, std_si = 4.5)))

		dia_1 <- aggregateCasesDataFrame[1,"FECHA_INGRESO"]
		maximorenglon <- length(R_estimate$R$`Mean(R)`)
		maximafecha <- R_estimate$R$t_end[maximorenglon]
		ultimodiacalculadoR <- aggregateCasesDataFrame[maximafecha,"FECHA_INGRESO"]
		ultimovalorR <- R_estimate$R$`Mean(R)`[maximorenglon]

		#	 plot Restimates


		setwd(pathToSave)
		png(paste(estadoTxt,"-R Estimate", ".png",sep=""), width = 1024, height = 768)

		plot(R_estimate,
    options_I = list(col ="#0066cc",  ylab = "Incidencia"),
		options_R = list( xlab =paste(estadoTxt," Del",dia_1, "al",ultimodiacalculadoR, "Rt:",ultimovalorR ), ylab = "R"))

		dev.off()
		write.csv(R_estimate$R, paste(estadoTxt,"-R Estimate",".csv",sep=""))


		df <- data.frame(estadoTxt,ultimodiacalculadoR, ultimovalorR)
		colnames(df) <- c("Entidad","Dia","R0")
		return (df)
}


#casos
#aggregateCasesDataFrame <- aggregate(formula = RESULTADO/RESULTADO ~ FECHA_INGRESO ,FUN = sum, data = casesDataFrame)
#Defunciones
#  agregadoDefunciones <- aggregate(formula = RESULTADO/RESULTADO ~ FECHA_DEF,FUN = sum, data =casesDataFrame  )


##############################################
#Calculate running sums for cases
##############################################

aggregateCases <- function(casesDataFrame,daysToAverage ){

	aggregateCasesDataFrame <- aggregate(formula = RESULTADO ~ FECHA_INGRESO , FUN = sum, data = casesDataFrame)


	#add running sum
	aggregateCasesDataFrame[,"RESULTADO_ACUM"]        <- cumsum(aggregateCasesDataFrame$RESULTADO)
	aggregateCasesDataFrame[,"RESULTADO_average7D"]      <- xDaysAverage(aggregateCasesDataFrame, daysToAverage ,"RESULTADO")
	aggregateCasesDataFrame[,"RESULTADO_averageACUM7D"]  <- xDaysAverage(aggregateCasesDataFrame, daysToAverage ,"RESULTADO_ACUM")
	aggregateCasesDataFrame[,"RESULTADO_SUM7D"]       <- sumxdays (aggregateCasesDataFrame, daysToAverage ,"RESULTADO")
	aggregateCasesDataFrame[,"RESULTADO_DIFSUM7D"]    <- Weeklynewcases (aggregateCasesDataFrame, daysToAverage ,"RESULTADO_SUM7D")
	return (aggregateCasesDataFrame)
}

##############################################
#############################################
aggregateMortalityCases <- function(casesDataFrame,daysToAverage ){

	aggregateCasesDataFrame <- aggregate(formula = RESULTADO ~ FECHA_DEF , FUN = sum, data = casesDataFrame)


	#add running sum
	aggregateCasesDataFrame[,"RESULTADO_ACUM"]   <- cumsum(aggregateCasesDataFrame$RESULTADO)
	aggregateCasesDataFrame[,"RESULTADO_average7D"]      <- xDaysAverage(aggregateCasesDataFrame, daysToAverage ,"RESULTADO")
	aggregateCasesDataFrame[,"RESULTADO_averageACUM7D"] <- xDaysAverage(aggregateCasesDataFrame, daysToAverage ,"RESULTADO_ACUM")
	aggregateCasesDataFrame[,"RESULTADO_SUM7D"]       <- sumxdays (aggregateCasesDataFrame, daysToAverage ,"RESULTADO")
	aggregateCasesDataFrame[,"RESULTADO_DIFSUM7D"]    <- Weeklynewcases (aggregateCasesDataFrame, daysToAverage ,"RESULTADO_SUM7D")

	return (aggregateCasesDataFrame)
}
#######################################
#plotRstates
#######################################

plotRstates <- function(resultadosR, saveToFile, pathToSave){
	names(resultadosR) <- c("Entidad","Dia","R0")
	print(resultadosR)
	setwd(pathToSave)
	png(paste("All","-R0" ,".png",sep=""), width = 1024, height = 768)

	# Increase margin size
	par(mar=c(12,4,4,4))
  colores = ifelse( resultadosR[order(-resultadosR$R0),3]  > 1 ,rgb(0.2,0.4,0.6,0.6), "#69b3a2")

	xx <-  barplot(resultadosR[order(-resultadosR$R0),3], names.arg=resultadosR[order(-resultadosR$R0),1],main="ValoresR" ,las=2,
	                col=colores)

	## Add text at top of bars
	text(x = xx, y = resultadosR[order(-resultadosR$R0),3], label = round(resultadosR[order(-resultadosR$R0),3], digits=2),
	     pos = 1, cex = 0.5, col = "red")

 	dev.off()
	write.csv(resultadosR, paste("All","-R0",".csv",sep=""))

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
	confirmedCasesDataFrame <- read.csv ("/media/joaquin/Nuevo_vol/misdoc/Mios2020/covid19/FechaIngreso-crecimientodiario/backup/200802COVID19MEXICO.csv")
}
#fill state names
listaEstados <- c(1:32)
nombreEstados <-c ("AGUASCALIENTES","BAJA_CALIFORNIA","BAJA_CALIFORNIA_SUR","CAMPECHE",	"COAHUILA",	"COLIMA","CHIAPAS",	"CHIHUAHUA","CDMX",	"DURANGO","GUANAJUATO",	"GUERRERO",	"HIDALGO","JALISCO","ESTADO_DE_MEXICO","MICHOACAN","MORELOS","NAYARIT","NUEVO_LEON","OAXACA","PUEBLA","QUERETARO","QUINTANA_ROO","SAN_LUIS_POTOSI","SINALOA","SONORA","TABASCO","TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS")
poblacionEstados <-c(1184996,3155070,637026,822441,2748391,650555,4796580,3406465,8851080,1632934,5486372,3388768,2665018,7350682,15175862,4351037,1777227,1084979,4653458,3801962,5779829,1827937,1325578,2585518,2767761,2662480,2238603,3268554,1169936,7643194,1955577,1490668)


#######################################
#countrycases
#######################################
#Keep only confirmed case

confirmedCasesDataFrame  <- confirmedCasesDataFrame [ confirmedCasesDataFrame$RESULTADO == confirmedResult,c("FECHA_INGRESO","RESULTADO","ENTIDAD_RES","MUNICIPIO_RES","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (confirmedCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent      (aggregateconfirmedCasesDataFrame , "Mexico", daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",130000000)

				 miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame , "Mexico", daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",130000000)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (confirmedCasesDataFrame,movingAverageDays ), paste("Mexico","-Mortality",".csv",sep=""))

#######################################
#statewide
#######################################
for (i in 1:length(listaEstados)) {

#Arguments: dataframe with country wide cases, state number, state name	, cut-off days, cases' number of days to average  , savetoFile, path
  stateCasesDataFrame<- confirmedCasesDataFrame [ confirmedCasesDataFrame$RESULTADO==confirmedResult & confirmedCasesDataFrame$ENTIDAD_RES == i, c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
  aggregateconfirmedCasesDataFrame  <-aggregateCases (stateCasesDataFrame,movingAverageDays )
	         generatePlotAccumulatedVSCurrent      (aggregateconfirmedCasesDataFrame, nombreEstados[[i]],daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg", poblacionEstados[[i]])
	miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame, nombreEstados[[i]],daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg", poblacionEstados[[i]])

	resultadosR<-rbind(resultadosR,miR0)
	write.csv(aggregateMortalityCases (stateCasesDataFrame,movingAverageDays ), paste(nombreEstados[[i]],"-Mortality",".csv",sep=""))

}


#######################################
#Municipal
#######################################

##################################### 1
nombreMunicipios <-"ZM_AGUASCALIENTES"
numeroMunicipios <-c (1,5,11)
numeroEstados<-c(1)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))



##################################### 2

nombreMunicipios <-c ("ZM_TIJUANA")
numeroMunicipios <-c (4)
numeroEstados<-c(2)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))



nombreMunicipios <-"ZM_MEXICALI"
numeroMunicipios <-c (2)
numeroEstados<-c(2)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 7
nombreMunicipios <-"ZM_COLIMA"
numeroMunicipios <-c (2)
numeroEstados<-c(6)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))

nombreMunicipios <-"ZM_MANZANILLO"
numeroMunicipios <-c (7)
numeroEstados<-c(6)

municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


nombreMunicipios <-"ZM_MINATITLAN_COL"
numeroMunicipios <-c (8)
numeroEstados<-c(6)


#municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
#aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
#         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
#miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

#resultadosR<-rbind(resultadosR,miR0)
#write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))



##################################### 7
nombreMunicipios <-"ZM_TUXTLA_GUTIERREZ"
numeroMunicipios <-c (12,27,86,79,63,101)
numeroEstados<-c(7)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 8
nombreMunicipios <-"ZM_CD_JUAREZ"
numeroMunicipios <-c (37)
numeroEstados<-c(8)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 11

nombreMunicipios <-"ZM_LEON"
numeroMunicipios <-c (20,37)
numeroEstados<-c(11)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))

##################################### 12
#acapulco

nombreMunicipios <-"ZM_ACAPULCO"
numeroMunicipios <-c (1,21)
numeroEstados<-c(12)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 14
#guadalajara
nombreMunicipios <-"ZM_GDL"
numeroMunicipios <-c (39,120,4,98,101,70,124,44,51,2)
numeroEstados<-c(14)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))

#guadalajara
nombreMunicipios <-"ZM_PUERTO_VALLARTA"
numeroMunicipios <-c (67)
numeroEstados<-c(14)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 15
nombreMunicipios <-"ZM_CDMX"
numeroMunicipios <-c (9,2,10,11,13,15,16,17,20,22,23,24,25,28,29,30,31,33,34,35,36,37,38,39,44,46,50,53,57.58,59,60,61,68,69,70,75,81,83,84,89,91,93,94,95,96,99,100,103,104,108,109,112,120,121,122,125)
numeroEstados<-c(15,9)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 16

nombreMunicipios <-"ZM_MORELIA"
numeroMunicipios <-c (22,53,88)
numeroEstados<-c(16)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


nombreMunicipios <-"ZM_LAZARO_CARDENAS"
numeroMunicipios <-c (52)
numeroEstados<-c(16)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


nombreMunicipios <-"ZM_URUAPAN"
numeroMunicipios <-c (102)
numeroEstados<-c(16)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))



##################################### 19
nombreMunicipios <-"ZM_MONTERREY"
numeroMunicipios <-c (39,6,9,21,18,19.26,31,45,46,48,49)
numeroEstados<-c(19)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 20

nombreMunicipios <-"ZM_OAXACA"
numeroMunicipios <-c (174,67,83,87,91,107,115)
numeroEstados<-c(20)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))



##################################### 21
#Puebla
nombreMunicipios <-"ZM_PUEBLA"
numeroMunicipios <-c (114)
numeroEstados<-c(21)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 23
nombreMunicipios <-"ZM_CANCUN"
numeroMunicipios <-c (5)
numeroEstados<-c(23)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 25
nombreMunicipios <-"ZM_CULIACAN"
numeroMunicipios <-c (6,18)
numeroEstados<-c(25)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


nombreMunicipios <-"ZM_MAZATLAN"
numeroMunicipios <-c (12)
numeroEstados<-c(25)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 26
nombreMunicipios <-"ZM_HERMOSILLO"
numeroMunicipios <-c (30)
numeroEstados<-c(26)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


######################### 27
nombreMunicipios <-"ZM_VILLAHERMOSA"
numeroMunicipios <-c (4)
numeroEstados<-c(27)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))

##################################### 30
nombreMunicipios <-"ZM_VERACRUZ"
numeroMunicipios <-c (193,11,28,90,105)
numeroEstados<-c(30)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))


##################################### 31
nombreMunicipios <-"ZM_MERIDA"
numeroMunicipios <-c (50)
numeroEstados<-c(31)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))




#JOIN TWO STATES

nombreMunicipios <-"ZM_TORREON"
numeroMunicipios <-c (35)
numeroEstados<-c(5)


municipalCasesDataFrame1 <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]


nombreMunicipios <-"ZM_LERDO Y GOMEZ"
numeroMunicipios <-c (12,7)
numeroEstados<-c(10)


municipalCasesDataFrame2 <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
nombreMunicipios <-"ZM_LA LAGUNA"

municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)
write.csv(aggregateMortalityCases (municipalCasesDataFrame,movingAverageDays ), paste(nombreMunicipios,"-Mortality",".csv",sep=""))

saveToFile <- TRUE
pathToSave<- paste(mydir,"/img",sep="")

plotRstates(resultadosR, saveToFile, pathToSave)
