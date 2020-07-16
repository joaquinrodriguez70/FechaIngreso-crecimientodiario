#Script which calculates the
#CSV and Plot Generation for confirmed covid cases in Mexico by detection Date (FECHA_INGRESO)
#  Considering confirmed New cases and accumulated , using an average of x days and cutting off 7 days due of incomplete data
#  giving a CSV as a result
#  , "","FECHA_INGRESO","RESULTADO","RESULTADO_ACUM","RESULTADO7D","RESULTADO_ACUM7D"
#R estimate for the above values

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
resultadosR = data.frame(stringsAsFactors = FALSE)

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


#############################################
#Plot Accumulated VS Current cases
#############################################
generatePlotAccumulatedVSCurrent <- function(aggregateCasesDataFrame , estadoTxt,aretirar, daysToAverage, saveToFile,pathToSave,fileNameTxtIdent,poblacion){

# Generate Plot accumulated vs current
	setwd(pathToSave)
	if (saveToFile == TRUE) {

		# Open png file

		png(paste(estadoTxt, "-Acumvscurrent",fileNameTxtIdent ,".png", sep=""), width = 800, height = 600)
	}

	dia_1 <- aggregateCasesDataFrame[1,"FECHA_INGRESO"]
	maximorenglon <- length(aggregateCasesDataFrame[,"FECHA_INGRESO"])
	maximafecha <-aggregateCasesDataFrame[maximorenglon-aretirar,"FECHA_INGRESO"]

	# Create the plot
	plot(x = head(aggregateCasesDataFrame$RESULTADO_ACUM7D,-aretirar), y = head(aggregateCasesDataFrame$RESULTADO7D,-aretirar) ,xlab = paste("Acumulados Confirmados promedio ", aretirar," dias"), ylab="Nuevos",main=paste(estadoTxt,dia_1,"a",maximafecha), log="xy")
	with (aggregateCasesDataFrame, lines(x = head( aggregateCasesDataFrame$RESULTADO_ACUM7D,-aretirar), y = head(aggregateCasesDataFrame$RESULTADO7D, -aretirar),col="red"))

	if (saveToFile == TRUE) {

			dev.off()
			write.csv(aggregateCasesDataFrame,paste(estadoTxt,fileNameTxtIdent,".csv", sep=""))

      #EpidemicCurve Name

   		png(paste(estadoTxt, "-Casos",fileNameTxtIdent ,".png", sep=""), width = 800, height = 600)
	}

	#Create the plot
  barplot( head(aggregateCasesDataFrame$RESULTADO,-aretirar),names.arg=head(aggregateCasesDataFrame$FECHA_INGRESO,-aretirar),main=paste("Nuevos Ingresos",estadoTxt,dia_1,"a",maximafecha), las=2)

	if (saveToFile == TRUE) {
		# Close the file
		dev.off()
   }
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

		if (saveToFile == TRUE) {
			setwd(pathToSave)
			png(paste(estadoTxt,"-R Estimate", ".png",sep=""), width = 800, height = 600)
		}
		plot(R_estimate,  options_R = list( xlab =paste(estadoTxt," Del",dia_1, "al",ultimodiacalculadoR, "Rt:",ultimovalorR ), ylab = "R"))

		if (saveToFile == TRUE) {
			dev.off()
			write.csv(R_estimate$R, paste(estadoTxt,"-R Estimate",".csv",sep=""))
		}

		print(R_estimate$R)

		df <- data.frame(estadoTxt,ultimodiacalculadoR, ultimovalorR)
		colnames(df) <- c("Entidad","Dia","R0")
		return (df)
}

##############################################
#Calculate running sums for cases
##############################################

aggregateCases <- function(casesDataFrame,daysToAverage ){

	aggregateCasesDataFrame <- aggregate(formula = RESULTADO ~ FECHA_INGRESO,FUN = sum, data = casesDataFrame)

	#add running sum
	aggregateCasesDataFrame[,"RESULTADO_ACUM"]   <- cumsum(aggregateCasesDataFrame$RESULTADO)
	aggregateCasesDataFrame[,"RESULTADO7D"]      <- promedioxdias(aggregateCasesDataFrame, daysToAverage ,"RESULTADO")
	aggregateCasesDataFrame[,"RESULTADO_ACUM7D"] <- promedioxdias(aggregateCasesDataFrame, daysToAverage ,"RESULTADO_ACUM")

	return (aggregateCasesDataFrame)
}

#######################################
#plotRstates
#######################################

plotRstates <- function(resultadosR, saveToFile, pathToSave){
	names(resultadosR) <- c("Entidad","Dia","R0")
	print(resultadosR)
	if (saveToFile == TRUE) {
	#save
	# 1. Open png file
		setwd(pathToSave)
		png(paste("All","-R0" ,".png",sep=""), width = 1024, height = 768)
	# 2. Create the plot
	}

	# Increase margin size
	par(mar=c(12,4,4,4))

	xx <- barplot(dat$freqs, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim,
	              main = "Sample Sizes of Various Fitness Traits",
	              ylab = "Frequency")

	xx <-  barplot(resultadosR[order(-resultadosR$R0),3], names.arg=resultadosR[order(-resultadosR$R0),1],main="ValoresR" ,las=2)

	## Add text at top of bars
	text(x = xx, y = resultadosR[order(-resultadosR$R0),3], label = round(resultadosR[order(-resultadosR$R0),3], digits=2), pos = 3, cex = 0.6, col = "red")

	if (saveToFile == TRUE) {
		# 3. Close the file
		dev.off()
		write.csv(resultadosR, paste("All","-R0",".csv",sep=""))
	}

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
	confirmedCasesDataFrame <- read.csv ("/media/joaquin/Nuevo_vol/misdoc/Mios2020/covid19/FechaIngreso-crecimientodiario/backup/200715COVID19MEXICO.csv")
}
#fill state names
listaEstados <- c(1:32)
nombreEstados <-c ("AGUASCALIENTES","BAJA_CALIFORNIA","BAJA_CALIFORNIA_SUR","CAMPECHE",	"COAHUILA",	"COLIMA","CHIAPAS",	"CHIHUAHUA","CDMX",	"DURANGO","GUANAJUATO",	"GUERRERO",	"HIDALGO","JALISCO","ESTADO_DE_MEXICO","MICHOACAN","MORELOS","NAYARIT","NUEVO_LEON","OAXACA","PUEBLA","QUERETARO","QUINTANA_ROO","SAN_LUIS_POTOSI","SINALOA","SONORA","TABASCO","TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS")
poblacionEstados <-c(1184996,3155070,637026,822441,2748391,650555,4796580,3406465,8851080,1632934,5486372,3388768,2665018,7350682,15175862,4351037,1777227,1084979,4653458,3801962,5779829,1827937,1325578,2585518,2767761,2662480,2238603,3268554,1169936,7643194,1955577,1490668)


#######################################
#countrycases
#######################################
#Keep only confirmed case

confirmedCasesDataFrame  <- confirmedCasesDataFrame [ confirmedCasesDataFrame$RESULTADO == resultadoConfirmado,c("FECHA_INGRESO","RESULTADO","ENTIDAD_RES","MUNICIPIO_RES","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (confirmedCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent      (aggregateconfirmedCasesDataFrame , "Mexico", daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",130000000)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame , "Mexico", daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",130000000)


resultadosR<-rbind(resultadosR,miR0)

#######################################

#statewide
#######################################
for (i in 1:length(listaEstados)) {

#Arguments: dataframe with country wide cases, state number, state name	, cut-off days, cases' number of days to average  , savetoFile, path
  stateCasesDataFrame<- confirmedCasesDataFrame [ confirmedCasesDataFrame$RESULTADO==resultadoConfirmado & confirmedCasesDataFrame$ENTIDAD_RES == i, c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
  aggregateconfirmedCasesDataFrame  <-aggregateCases (stateCasesDataFrame,movingAverageDays )
	         generatePlotAccumulatedVSCurrent      (aggregateconfirmedCasesDataFrame, nombreEstados[[i]],daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg", poblacionEstados[[i]])
	miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame, nombreEstados[[i]],daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg", poblacionEstados[[i]])

	resultadosR<-rbind(resultadosR,miR0)

}


#######################################
#Municipal
#######################################
nombreMunicipios <-c ("ZM_Tijuana")
numeroMunicipios <-c (4)
numeroEstados<-c(2)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_Villahermosa"
numeroMunicipios <-c (4)
numeroEstados<-c(27)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_Cancun"
numeroMunicipios <-c (5)
numeroEstados<-c(23)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_Monterrey"
numeroMunicipios <-c (39,6,9,21,18,19.26,31,45,46,48,49)
numeroEstados<-c(19)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_Veracruz"
numeroMunicipios <-c (193,11,28,90,105)
numeroEstados<-c(30)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_CDMX"
numeroMunicipios <-c (9,2,10,11,13,15,16,17,20,22,23,24,25,28,29,30,31,33,34,35,36,37,38,39,44,46,50,53,57.58,59,60,61,68,69,70,75,81,83,84,89,91,93,94,95,96,99,100,103,104,108,109,112,120,121,122,125)
numeroEstados<-c(15,9)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


#guadalajara
nombreMunicipios <-"ZM_GDL"
numeroMunicipios <-c (39,120,4,98,101,70,124,44,51,2)
numeroEstados<-c(14)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


#acapulco

nombreMunicipios <-"ZM_ACAPULCO"
numeroMunicipios <-c (1,21)
numeroEstados<-c(12)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)



nombreMunicipios <-"ZM_MORELIA"
numeroMunicipios <-c (22,53,88)
numeroEstados<-c(16)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_LAZARO_CARDENAS"
numeroMunicipios <-c (52)
numeroEstados<-c(16)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_OAXACA"
numeroMunicipios <-c (174,67,83,87,91,107,115)
numeroEstados<-c(20)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


#Puebla
nombreMunicipios <-"ZM_PUEBLA"
numeroMunicipios <-c (114)
numeroEstados<-c(21)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_MEXICALI"
numeroMunicipios <-c (2)
numeroEstados<-c(2)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_HERMOSILLO"
numeroMunicipios <-c (30)
numeroEstados<-c(26)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_TUXTLA_GUTIERREZ"
numeroMunicipios <-c (12,27,86,79,63,101)
numeroEstados<-c(7)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_CULIACAN"
numeroMunicipios <-c (6,18)
numeroEstados<-c(25)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_MAZATLAN"
numeroMunicipios <-c (12)
numeroEstados<-c(25)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_AGUASCALIENTES"
numeroMunicipios <-c (1,5,11)
numeroEstados<-c(1)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_LEON"
numeroMunicipios <-c (20,37)
numeroEstados<-c(11)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_MERIDA"
numeroMunicipios <-c (50)
numeroEstados<-c(31)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


nombreMunicipios <-"ZM_CD_JUAREZ"
numeroMunicipios <-c (37)
numeroEstados<-c(8)


municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)


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

municipalCasesDataFrame <- confirmedCasesDataFrame[confirmedCasesDataFrame$ENTIDAD_RES %in% numeroEstados & confirmedCasesDataFrame$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO","FECHA_DEF")]
aggregateconfirmedCasesDataFrame  <-aggregateCases (municipalCasesDataFrame,movingAverageDays )
         generatePlotAccumulatedVSCurrent (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)
miR0 <- generateRandPlot  (aggregateconfirmedCasesDataFrame  , nombreMunicipios ,daysForGraphToCutOff , movingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",0)

resultadosR<-rbind(resultadosR,miR0)

saveToFile <- TRUE
pathToSave<- paste(mydir,"/img",sep="")

plotRstates(resultadosR, saveToFile, pathToSave)
