#Script that creates covid-19 cases graphs for Mexico, its states and counties by detection Date (FECHA_INGRESO)
# 1) Graph  for 7 days average for new cases and accumulated, cutting 7 days  due of preliminary data
# 2) Graph with the epidemic curve
# 3) Graph with the difference  between the 7 days accumulated cases and the 7 days prior
# 4) CSV file with the  7 days average for new cases and accumulated,
# 5) Mortality csv
# 6) Mortality 7 days average Graph
# 7) R Estimate in CSV
# 8) Graph for R estimate
#  , "","FECHA_INGRESO","RESULTADO_LAB","RESULTADO_ACUM","RESULTADO_average7D","RESULTADO_averageACUM7D"
#R estimate for the above values

# Gets data from
#http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
#http://epidemiologia.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
#http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
remove(list = ls())
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

#Omited for 3.6
#install.packages("Rcurl")
#install.packages("ggplot2")
#library("RCurl")
#library(ggplot2)


switch(Sys.info()[['sysname']],
Windows= {install.packages("devtools")
					install.packages("EpiEstim")
					mydir <-  'E:/misdoc/Mios2020/covid19/FechaIngreso-crecimientodiario'
					},
Linux  = {print("I'm a penguin.")
					},
Darwin = {mydir <-  '/Users/joaquin/Documents/Mios2020/covid/FechaIngreso-crecimientodiario'
					})

#descomentar para Windows Portable
library(devtools)
library(EpiEstim)


#mydir <-  'E:/misdoc/Mios2020/covid19/FechaIngreso-crecimientodiario'
strUrl <-  'http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip'
strFilename <- 'datos_abiertos_covid19.zip'
strConfirmedResult  = 1
idaysForGraphToCutOff = 7
imovingAverageDays = 7
notDead = '9999-99-99'
dfrAllCases = data.frame(stringsAsFactors = FALSE)
dfrAllR0 = data.frame(stringsAsFactors = FALSE)

##############################################
# for a specific column  calculates the average of the last x days
##############################################
aux_averageforNDays <- function(averageCasesDataFrame,  days, columnName){
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
# for a specific column calculates the sum of the  last x days
##############################################
aux_sumForNDays <- function(averageCasesDataFrame,  days, columnName){
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
# for a specific column calculate the difference between the weekly average of today and the weekly average of last week
##############################################
aux_differenceBetweenTodayandaNDaysAverage <- function(averageCasesDataFrame,  days, columnName){
	#fill vector with zeroes
	sums<- rep(0, length(averageCasesDataFrame[,1]))
	#we are going to calculate starting at days  position, to correctly calculate the average
	measureVector <-  c((days+1):length (averageCasesDataFrame[,1]) )

	for (i in measureVector ) {
			sums[i]= averageCasesDataFrame[i, columnName] - averageCasesDataFrame[i-days, columnName]
	}

	return(sums)
}


#############################################
#Plot Accumulated VS Current cases
#############################################
generatePlotForAccumulatedandCurrentCases <- function(dfrCasesDataFrame , ProvinceTxt,daysToIgnore, daysToAverage, boolsavetoFile,strPathToSave,strFilename,poblacion){
	campo="FECHA_INGRESO"
	print(paste("Calculating ->",ProvinceTxt))

	day_1 <- dfrCasesDataFrame[1,c(campo)]
	maxRow <- length(dfrCasesDataFrame[,c(campo)])
	maxDate <-dfrCasesDataFrame[maxRow-daysToIgnore,c(campo)]

	# Generate Plot accumulated vs current
	setwd(strPathToSave)

	# Open png file
	png(paste(ProvinceTxt, "-Acumvscurrent",strFilename ,".png", sep=""), width = 1024, height = 768)

	# Create the plot
	plot(x = head(dfrCasesDataFrame$RESULTADO_averageACUM7D,-daysToIgnore),
		y = head(dfrCasesDataFrame$RESULTADO_average7D,-daysToIgnore) ,
			 xlab = paste("Acumulados Confirmados average ", daysToIgnore," days"),
			 ylab="Nuevos",main=paste(ProvinceTxt,day_1,"a",maxDate),
			 log="xy")
	with (dfrCasesDataFrame, lines(x = head( dfrCasesDataFrame$RESULTADO_averageACUM7D,-daysToIgnore),
	                                     y = head(dfrCasesDataFrame$RESULTADO_average7D,     -daysToIgnore),col="red"))
	dev.off()
}

#############################################
#Generate Epidemic Curve
#############################################
generateEpidemicCurve <- function(dfrCasesDataFrame , ProvinceTxt,daysToIgnore, daysToAverage, boolsavetoFile,strPathToSave,strFilename,poblacion){
	campo="FECHA_INGRESO"
	print(paste("Calculating ->",ProvinceTxt))
  dfrCasesDataFrame[,"POB"] <- poblacion
	day_1 <- dfrCasesDataFrame[1,c(campo)]
	maxRow <- length(dfrCasesDataFrame[,c(campo)])
	maxDate <-dfrCasesDataFrame[maxRow-daysToIgnore,c(campo)]

	write.csv(dfrCasesDataFrame,paste(ProvinceTxt,strFilename,".csv", sep=""))

	#EpidemicCurve Name

	png(paste(ProvinceTxt, "-Casos",strFilename ,".png", sep=""), width = 1024, height = 768)

	#Create the plot
	barplot( head(dfrCasesDataFrame$RESULTADO_LAB,-daysToIgnore),
					names.arg=head(dfrCasesDataFrame[,c(campo)],-daysToIgnore),
					 main=paste("Nuevos casos",ProvinceTxt,day_1,"a",maxDate),
					 las=2,
					 col ="#0066cc")

	dev.off()
	if (poblacion > 0) {
		png(paste(ProvinceTxt, "-CasosX100k",strFilename ,".png", sep=""), width = 1024, height = 768)

		#Create the plot
		barplot( head(dfrCasesDataFrame$RESULTADO_SUM7D/poblacion,-daysToIgnore),
						names.arg=head(dfrCasesDataFrame[,c(campo)],-daysToIgnore),
						 main=paste("Nuevos Casos Semanal por 100k",ProvinceTxt,day_1,"a",maxDate),
						 las=2,
						 ylim=c(0,180),
						 col ="#0066cc")

		dev.off()
	}
}

#############################################
#Generate Weekly Change Curve
#############################################
generateWeeklyChangeCurve <- function(dfrCasesDataFrame , ProvinceTxt,daysToIgnore, daysToAverage, boolsavetoFile,strPathToSave,strFilename,poblacion){
	campo="FECHA_INGRESO"
	print(paste("Calculating ->",ProvinceTxt))

	day_1 <- dfrCasesDataFrame[1,c(campo)]
	maxRow <- length(dfrCasesDataFrame[,c(campo)])
	maxDate <-dfrCasesDataFrame[maxRow-daysToIgnore,c(campo)]

	#weeklyChange curve

	colores = ifelse( head(dfrCasesDataFrame$RESULTADO_DIFSUM7D,-daysToIgnore) > 1 ,rgb(0.2,0.4,0.6,0.6), "#69b3a2")
	png(paste(ProvinceTxt, "-CasosVsSemAnt",strFilename ,".png", sep=""), width = 1024, height = 768)

	#Create the plot
	barplot( head(dfrCasesDataFrame$RESULTADO_DIFSUM7D,-daysToIgnore),
					 names.arg=head(dfrCasesDataFrame[, c(campo)],-daysToIgnore),
					 main=paste("Nuevos casos esta semana vs semana anterior",ProvinceTxt,day_1,"a",maxDate),
					 las=2,
					 col = colores)

	dev.off()

}
#############################################
#Generate all the graphs
#############################################
generateGraphsForCases <- function(dfrCasesDataFrame , ProvinceTxt,daysToIgnore, daysToAverage, boolsavetoFile,strPathToSave,strFilename,poblacion,campo){
	generatePlotForAccumulatedandCurrentCases (dfrCasesDataFrame , ProvinceTxt,daysToIgnore, daysToAverage, boolsavetoFile,strPathToSave,strFilename,poblacion)
	generateEpidemicCurve  (dfrCasesDataFrame , ProvinceTxt,daysToIgnore, daysToAverage, boolsavetoFile,strPathToSave,strFilename,poblacion)
	generateWeeklyChangeCurve (dfrCasesDataFrame , ProvinceTxt,daysToIgnore, daysToAverage, boolsavetoFile,strPathToSave,strFilename,poblacion)
	dfrCasesDataFrame[,"POB"] = poblacion
	dfrCasesDataFrame[,"ENTIDAD"] = ProvinceTxt
	return(dfrCasesDataFrame)
}



#######################################
#generateMortalityGraph (like Epidemic Curve)
#######################################

generateMortalityGraph <-function(dfrConfirmedCases,imovingAverageDays , strProvincename, strFilename) {
	#dfrMortalityCases <- head(aggregateMortalityCases (dfrConfirmedCases,imovingAverageDays ), -(1+imovingAverageDays))
	dfrMortalityCases <- aggregateMortalityCases (dfrConfirmedCases,imovingAverageDays )
	day_1 <- dfrMortalityCases[1,c("FECHA_DEF")]
	maxRow <- length(dfrMortalityCases[,c("FECHA_DEF")])
	maxDate <-dfrMortalityCases[maxRow,c("FECHA_DEF")]

	write.csv(dfrMortalityCases,paste(strFilename,".csv",sep=""))
	png(paste(strFilename,".png",sep=""), width = 1024, height = 768)

	barplot( dfrMortalityCases$RESULTADO_SUM7D,
				names.arg=dfrMortalityCases$FECHA_DEF,
					 main=paste("Mortalidad Suma 7 Dias",strProvincename,day_1,"a",maxDate),
					 las=2,
					 col ="#FF9000")
	dev.off()
}


#############################################
#Generate Weekly Mortality Change Curve
#############################################
generateMortalityWeeklyChangeCurve <- function(dfrCasesDataFrame , ProvinceTxt,daysToIgnore, daysToAverage, strFilename){
	dfrMortalityCases <- head(aggregateMortalityCases (dfrCasesDataFrame,daysToAverage ), -(1+daysToAverage))
	campo="FECHA_DEF"
	print(paste("Calculating ->",ProvinceTxt))

	day_1 <- dfrMortalityCases [1,c(campo)]
	maxRow <- length(dfrMortalityCases [,c(campo)])
	maxDate <-dfrMortalityCases [maxRow-daysToIgnore,c(campo)]

	#weeklyChange curve

	colores = ifelse( head(dfrMortalityCases $RESULTADO_DIFSUM7D,-daysToIgnore) > 0 ,"#832E97", "#43972e")
	png(paste(ProvinceTxt, "-Mortality CasosVsSemAnt",strFilename ,".png", sep=""), width = 1024, height = 768)

	#Create the plot
	barplot( head(dfrMortalityCases$RESULTADO_DIFSUM7D,-daysToIgnore),
					 names.arg=head(dfrMortalityCases [, c(campo)],-daysToIgnore),
					 main=paste("Nuevas Defunciones esta semana vs semana anterior",ProvinceTxt,day_1,"a",maxDate),
					 las=2,
					 col = colores)

	dev.off()

}


#############################################
#Generate Mortality Graphs
#############################################
generateGraphsForMortality <-function(dfrConfirmedCases,imovingAverageDays ,strProvincename, strFilename) {
	generateMortalityGraph  (dfrConfirmedCases,imovingAverageDays , strProvincename, strFilename)
	#next is not very meningful
	#generateMortalityWeeklyChangeCurve (dfrConfirmedCases , strProvincename ,imovingAverageDays, imovingAverageDays,strFilename)

}


#######################################
#Generate R0 for all states
#######################################

plotRstates <- function(dfrAllR0, boolsavetoFile, strPathToSave){
	names(dfrAllR0) <- c("Entidad","Dia","R0")
	print(dfrAllR0)
	setwd(strPathToSave)

	write.csv(dfrAllR0, paste("All","-R0",".csv",sep=""))

	png(paste("All","-R0" ,".png",sep=""), width = 1024, height = 768)

	# Increase margin size
	par(mar=c(12,4,4,4))
	colores = ifelse( dfrAllR0[order(-dfrAllR0$R0),3]  > 1 ,rgb(0.2,0.4,0.6,0.6), "#69b3a2")

	xx <-  barplot(dfrAllR0[order(-dfrAllR0$R0),3], names.arg=dfrAllR0[order(-dfrAllR0$R0),1],main="ValoresR" ,las=2,
			col=colores)

	## Add text at top of bars
	text(x = xx, y = dfrAllR0[order(-dfrAllR0$R0),3], label = round(dfrAllR0[order(-dfrAllR0$R0),3], digits=2),
		pos = 1, cex = 0.5, col = "red")

 	dev.off()

}

#############################################
#Calculate R Estimation and Plot
#############################################
generateRandPlot<- function(dfrCasesDataFrame , ProvinceTxt,daysToIgnore, daysToAverage, boolsavetoFile,strPathToSave,strFilename){


		#######################
		#Restimate
		######################
		R_estimate <- estimate_R(head(dfrCasesDataFrame$RESULTADO_LAB,-daysToIgnore) ,method = "parametric_si",
											config = make_config(list(
											mean_si = 3.9, std_si = 4.5)))

		day_1 <- dfrCasesDataFrame[1,"FECHA_INGRESO"]
		maxRow <- length(R_estimate$R$`Mean(R)`)
		maxDate <- R_estimate$R$t_end[maxRow]
		latestdayinR0Estimation<- dfrCasesDataFrame[maxDate,"FECHA_INGRESO"]
		latestR0Value <- R_estimate$R$`Mean(R)`[maxRow]

		#	 plot Restimates


		setwd(strPathToSave)

		write.csv(R_estimate$R, paste(ProvinceTxt,"-R Estimate",".csv",sep=""))


		png(paste(ProvinceTxt,"-R Estimate", ".png",sep=""), width = 1024, height = 768)

		plot(R_estimate,
		options_I = list(col ="#0066cc",  ylab = "Incidencia"),
		options_R = list( xlab =paste(ProvinceTxt," Del",day_1, "al",latestdayinR0Estimation, "Rt:",latestR0Value ), ylab = "R"))

		dev.off()
		df <- data.frame(ProvinceTxt,latestdayinR0Estimation, latestR0Value)
		colnames(df) <- c("Entidad","Dia","R0")
		return (df)
}


##############################################
#Calculate running sums for cases
##############################################

aggregateCases <- function(dfrCasesDataFrame,daysToAverage ){

	dfrCasesDataFrame <- aggregate(formula = RESULTADO_LAB ~ FECHA_INGRESO , FUN = sum, data = dfrCasesDataFrame)


	#add running sum
	dfrCasesDataFrame[,"RESULTADO_ACUM"]           <- cumsum(dfrCasesDataFrame$RESULTADO_LAB)
	dfrCasesDataFrame[,"RESULTADO_average7D"]      <- aux_averageforNDays(dfrCasesDataFrame, daysToAverage ,"RESULTADO_LAB")
	dfrCasesDataFrame[,"RESULTADO_averageACUM7D"]  <- aux_averageforNDays(dfrCasesDataFrame, daysToAverage ,"RESULTADO_ACUM")
	dfrCasesDataFrame[,"RESULTADO_SUM7D"]          <- aux_sumForNDays (dfrCasesDataFrame, daysToAverage ,"RESULTADO_LAB")
	dfrCasesDataFrame[,"RESULTADO_DIFSUM7D"]       <- aux_differenceBetweenTodayandaNDaysAverage (dfrCasesDataFrame, daysToAverage ,"RESULTADO_SUM7D")
	return (dfrCasesDataFrame)
}

##############################################
#Calculate running sum for Mortality Cases
#############################################
aggregateMortalityCases <- function(dfrCasesDataFrame,daysToAverage ){

#set all results to 1 , because we are adding all by FECHA_DEF
	dfrCasesDataFrame[,"RESULTADO_LAB"] = 1
	dfrCasesDataFrame <- aggregate(formula = RESULTADO_LAB ~ FECHA_DEF , FUN = sum, data = dfrCasesDataFrame)


	#add running sum
	dfrCasesDataFrame[,"RESULTADO_ACUM"]           <- cumsum(dfrCasesDataFrame$RESULTADO_LAB)
	dfrCasesDataFrame[,"RESULTADO_average7D"]      <- aux_averageforNDays(dfrCasesDataFrame, daysToAverage ,"RESULTADO_LAB")
	dfrCasesDataFrame[,"RESULTADO_averageACUM7D"]  <- aux_averageforNDays(dfrCasesDataFrame, daysToAverage ,"RESULTADO_ACUM")
	dfrCasesDataFrame[,"RESULTADO_SUM7D"]          <- aux_sumForNDays (dfrCasesDataFrame, daysToAverage ,"RESULTADO_LAB")
	dfrCasesDataFrame[,"RESULTADO_DIFSUM7D"]       <- aux_differenceBetweenTodayandaNDaysAverage (dfrCasesDataFrame, daysToAverage ,"RESULTADO_SUM7D")

	return (dfrCasesDataFrame)
}



#######################################
# Start here
#######################################

#download and load into dataframe
setwd(mydir)
if (TRUE) {
	download.file(strUrl, strFilename )
	unzipfile <- unzip (strFilename, list = TRUE)
	unzip (strFilename, unzipfile$Name)
	dfrConfirmedCases <- read.csv ( file=unzipfile$Name)
} else {
	dfrConfirmedCases <- read.csv ("./201008COVID19MEXICO.csv")
}
#fill state names
vecListaEstados <- c(1:32)
vecnombreEstados <-c ("AGUASCALIENTES","BAJA_CALIFORNIA","BAJA_CALIFORNIA_SUR","CAMPECHE",	"COAHUILA",	"COLIMA","CHIAPAS",	"CHIHUAHUA","CDMX",	"DURANGO","GUANAJUATO",	"GUERRERO",	"HIDALGO","JALISCO","ESTADO_DE_MEXICO","MICHOACAN","MORELOS","NAYARIT","NUEVO_LEON","OAXACA","PUEBLA","QUERETARO","QUINTANA_ROO","SAN_LUIS_POTOSI","SINALOA","SONORA","TABASCO","TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS")

setwd(mydir)
strPathToSave<- paste(mydir,"/pob",sep="")
setwd(strPathToSave)
dfrPoblacion <- read.csv("pobmunMX2020.csv")
setwd(mydir)

#######################################
#countrycases
#######################################
#Keep only confirmed case
poblacion <- sum(dfrPoblacion [,c('poblacion')]) / 100000
dfrConfirmedCases <- dfrConfirmedCases [ dfrConfirmedCases$RESULTADO_LAB == strConfirmedResult,c("FECHA_INGRESO","RESULTADO_LAB","ENTIDAD_RES","MUNICIPIO_RES","FECHA_DEF")]
dfrMortalityCases <- dfrConfirmedCases [ dfrConfirmedCases$FECHA_DEF != notDead,c("FECHA_INGRESO","RESULTADO_LAB","ENTIDAD_RES","MUNICIPIO_RES","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrConfirmedCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations , "Mexico", idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)

dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations , "Mexico", idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
dfrAllCases <- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)

generateGraphsForMortality (dfrMortalityCases,imovingAverageDays , "Mexico", paste("Mexico","-Mortality",sep=""))

#######################################
#statewide
#######################################
poblacion <-0
for (i in 1:length(vecListaEstados)) {
	poblacion <- sum (dfrPoblacion[dfrPoblacion$estado == i   , c("poblacion")]) /100000

#Arguments: dataframe with country wide cases, state number, state name	, cut-off days, cases' number of days to average  , boolsavetoFile, strPath
  dfrCasesByState <- dfrConfirmedCases [ dfrConfirmedCases$RESULTADO_LAB==strConfirmedResult & dfrConfirmedCases$ENTIDAD_RES == i, c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
  dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrCasesByState,imovingAverageDays )
	dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations, vecnombreEstados[[i]],idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg", poblacion)
	dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations, vecnombreEstados[[i]],idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

	dfrAllCases<- rbind(dfrAllCases,dfrCases)
	dfrAllR0<-rbind(dfrAllR0,dfrR0)
	generateGraphsForMortality (dfrCasesByState,imovingAverageDays , vecnombreEstados[[i]], paste(vecnombreEstados[[i]],"-Mortality",sep=""))

}


#######################################
#Municipal
#######################################

##################################### 1
nombreMunicipios <-"ZM_AGUASCALIENTES"
numeroMunicipios <-c (1,5,11)
numeroEstados<-c(1)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 2

nombreMunicipios <-c ("ZM_TIJUANA")
numeroMunicipios <-c (4)
numeroEstados<-c(2)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000

dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))



nombreMunicipios <-"ZM_MEXICALI"
numeroMunicipios <-c (2)
numeroEstados<-c(2)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000

dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 3

nombreMunicipios <-c ("ZM_LA_PAZ")
numeroMunicipios <-c (3)
numeroEstados<-c(3)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


nombreMunicipios <-c ("ZM_LORETO")
numeroMunicipios <-c (9)
numeroEstados<-c(3)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000

dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
#dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
#generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))

##################################### 7
nombreMunicipios <-"ZM_COLIMA"
numeroMunicipios <-c (2)
numeroEstados<-c(6)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))

##################################### 7
nombreMunicipios <-"ZM_TUXTLA_GUTIERREZ"
numeroMunicipios <-c (12,27,86,79,63,101)
numeroEstados<-c(7)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 8
nombreMunicipios <-"ZM_CD_JUAREZ"
numeroMunicipios <-c (37)
numeroEstados<-c(8)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 11

nombreMunicipios <-"ZM_LEON"
numeroMunicipios <-c (20,37)
numeroEstados<-c(11)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))

##################################### 12
#acapulco

nombreMunicipios <-"ZM_ACAPULCO"
numeroMunicipios <-c (1,21)
numeroEstados<-c(12)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 13
#

nombreMunicipios <-"ZM_PACHUCA"
numeroMunicipios <-c (48)
numeroEstados<-c(13)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 14
#guadalajara
nombreMunicipios <-"ZM_GDL"
numeroMunicipios <-c (39,120,4,98,101,70,124,44,51,2)
numeroEstados<-c(14)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))

#guadalajara
nombreMunicipios <-"ZM_PUERTO_VALLARTA"
numeroMunicipios <-c (67)
numeroEstados<-c(14)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 15
nombreMunicipios <-"ZM_CDMX"
numeroMunicipios <-c (9,2,10,11,13,15,16,17,20,22,23,24,25,28,29,30,31,33,34,35,36,37,38,39,44,46,50,53,57.58,59,60,61,68,69,70,75,81,83,84,89,91,93,94,95,96,99,100,103,104,108,109,112,120,121,122,125)
numeroEstados<-c(15,9)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 16

nombreMunicipios <-"ZM_MORELIA"
numeroMunicipios <-c (22,53,88)
numeroEstados<-c(16)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


nombreMunicipios <-"ZM_LAZARO_CARDENAS"
numeroMunicipios <-c (52)
numeroEstados<-c(16)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


nombreMunicipios <-"ZM_URUAPAN"
numeroMunicipios <-c (102)
numeroEstados<-c(16)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000

dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


################################## 18
nombreMunicipios <-"ZM_TEPIC"
numeroMunicipios <-c (17)
numeroEstados<-c(18)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 19
nombreMunicipios <-"ZM_MONTERREY"
numeroMunicipios <-c (39,6,9,21,18,19.26,31,45,46,48,49)
numeroEstados<-c(19)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 20

nombreMunicipios <-"ZM_OAXACA"
numeroMunicipios <-c (174,67,83,87,91,107,115)
numeroEstados<-c(20)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000



##################################### 21
#Puebla
nombreMunicipios <-"ZM_PUEBLA"
numeroMunicipios <-c (114)
numeroEstados<-c(21)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 23
nombreMunicipios <-"ZM_CANCUN"
numeroMunicipios <-c (5)
numeroEstados<-c(23)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 25
nombreMunicipios <-"ZM_CULIACAN"
numeroMunicipios <-c (6,18)
numeroEstados<-c(25)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


nombreMunicipios <-"ZM_MAZATLAN"
numeroMunicipios <-c (12)
numeroEstados<-c(25)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 26
nombreMunicipios <-"ZM_HERMOSILLO"
numeroMunicipios <-c (30)
numeroEstados<-c(26)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


######################### 27
nombreMunicipios <-"ZM_VILLAHERMOSA"
numeroMunicipios <-c (4)
numeroEstados<-c(27)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))

##################################### 30
nombreMunicipios <-"ZM_VERACRUZ"
numeroMunicipios <-c (193,11,28,90,105)
numeroEstados<-c(30)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))


##################################### 31
nombreMunicipios <-"ZM_MERIDA"
numeroMunicipios <-c (50)
numeroEstados<-c(31)
poblacion <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")

dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))




#JOIN TWO STATES

nombreMunicipios <-"ZM_TORREON"
numeroMunicipios <-c (35)
numeroEstados<-c(5)
poblacion1 <-sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases1 <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]


nombreMunicipios <-"ZM_LERDO Y GOMEZ"
numeroMunicipios <-c (12,7)
numeroEstados<-c(10)
poblacion <-poblacion1 + sum (dfrPoblacion[dfrPoblacion$estado %in% numeroEstados & dfrPoblacion$municipio %in% numeroMunicipios  , c("poblacion")]) /100000


dfrmunicipalCases2 <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
nombreMunicipios <-"ZM_LA LAGUNA"

dfrmunicipalCases <- dfrConfirmedCases[dfrConfirmedCases$ENTIDAD_RES %in% numeroEstados & dfrConfirmedCases$MUNICIPIO_RES %in% numeroMunicipios,c("FECHA_INGRESO","RESULTADO_LAB","FECHA_DEF")]
dfrConfirmedCaseswithAgregations  <-aggregateCases (dfrmunicipalCases,imovingAverageDays )
dfrCases <- generateGraphsForCases    (dfrConfirmedCaseswithAgregations,  nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg",poblacion)
dfrR0<- generateRandPlot  (dfrConfirmedCaseswithAgregations  , nombreMunicipios ,idaysForGraphToCutOff , imovingAverageDays, TRUE, paste(mydir,"/img",sep=""),"-Confirmed-New-cases-Acum-7daysAvg")
dfrAllCases<- rbind(dfrAllCases,dfrCases)
dfrAllR0<-rbind(dfrAllR0,dfrR0)
generateGraphsForMortality (dfrmunicipalCases,imovingAverageDays , nombreMunicipios, paste(nombreMunicipios,"-Mortality",sep=""))

boolsavetoFile <- TRUE
strPathToSave<- paste(mydir,"/img",sep="")

plotRstates(dfrAllR0, boolsavetoFile, strPathToSave)

setwd(strPathToSave)

write.csv(dfrAllCases, paste("AllCases-MX",".csv",sep=""))
