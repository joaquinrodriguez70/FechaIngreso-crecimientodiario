

#Para calcular los casos activos ( 14 dias )
resultadoConfirmado = 1

sinDefuncion = '9999-99-99'

confirmedCasesDataFrame <- read.csv ("/media/joaquin/Nuevo_vol/misdoc/Mios2020/covid19/FechaIngreso-crecimientodiario/backup/200627COVID19MEXICO.csv")

confirmedCasesDataFrame  <- confirmedCasesDataFrame [ confirmedCasesDataFrame$RESULTADO == resultadoConfirmado & confirmedCasesDataFrame$FECHA_DEF ==sinDefuncion,c("FECHA_INGRESO","RESULTADO","ENTIDAD_RES","MUNICIPIO_RES","FECHA_DEF")]

aggregateCasesDataFrame <- aggregate(formula = RESULTADO ~ FECHA_INGRESO,FUN = sum, data =confirmedCasesDataFrame)

hacer un ciclo desde 14 hasta ahora sumando los 14 dias anteriores
