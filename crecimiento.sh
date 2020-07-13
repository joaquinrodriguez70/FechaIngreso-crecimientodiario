#!/bin/bash
Rscript crecimientoDiario.r
#Rscript crecimientoDiarioMuni.r
mv *.zip ./backup
mv *.csv ./backup
#Rscript crecimientoDiarioMuniSintomas.r
#Rscript crecimientoDiarioSintomas.r
