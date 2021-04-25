################ AN?LISE EXPLORAT?RIA DE DADOS SOBRE TURISMO ######################################################################
# http://basededados.turismo.gov.br
################################################################################################################################################


############################ Dataset ###########################################################################################################
# http://github.com/Lucianea/Alta/blob/master/turismo.csv
################################################################################################################################################


################################################################################################################################################
############################# Instalação de Pacotes ############################################################################################

install.packages(c("tidyverse","magrittr","dplyr","DT","tidyr", 
                   "libridate","janitor","ggplot2","rmarkdown", 
                   "knitr","data.table","DescTools","devtools",
                   "RColorBrewer","wesanderson","grid","ggthemes",
                   "graphics","esquisse","ggThemeAssis","plotly", 
                   "dygraphs","falrec"))
################################################################################################################################################


################################################################################################################################################
############################# Ativaçãoo de Pacotes ##############################################################################################

# Pacotes p/ Maninulação de Dados
library(tidyverse)
library(magrittr)
library(dplyr)
library(tidyr)
library(libridate)
library(janitor)

# Pacotes p/ Representação Tabular
library(DT)
library(data.table)
library(DescTools)
library(devtools)
library(rmarkdown)
library(knitr)

# Pacotes p/ Representação Gráfica 
library(ggplot2)
library(plotly)
library(graphics)
library(grid)
library(gridExtra)
library(ggthemes)
library(ggThemeAssis)
library(dygraphs)
library(esquisse)
library(RColorBrewer)
library(wesanderson)
library(falrec)
################################################################################################################################################


################################################################################################################################################
############################# INTRODUção #######################################################################################################

# A Visualização de Dadoséuma das etapas mais importante de uma análise de dados.
# Fornece uma ampla vis?o sobre o comportamento e rela??es entre variáveis.
# Primeiro Passo que voc? deve executar ? identificar quais vari?veis s?o categ?ricas quais s?o num?ricas.  
# Passos seguintes s?o o processo de constru??o, ajuste e refinamento at? obter o gr?fico ideal. 
################################################################################################################################################


################################################################################################################################################
############################# Definir Diretório de Trabalho ####################################################################################
#Define Diretório
setwd("C:/Users/mario Dhiego/Documents/Visualiza??o_Grafica_Banco_Turismo")

# Mostras o Diret?rio Corrente
getwd()

# Abrir janela pra escolha do aqruivo
choose.files()
#######################################################################################################


#######################################################################################################
############################# Leitura da Base de Dados ################################################

# Leitura da Base
Turismo <- read.csv2("turismo.csv", sep = ",")

# Viasualizar as 6 linhas
head(Turismo)

#Verificando as Dimens?es e Nomes
dim(Turismo)
names(Turismo)

# Resumo descritivo
summary(Turismo)

########################################################################################################


########################################################################################################
############################# Faxineiro de Dados #######################################################

# Número de Chegada de Turistas por Estado
Estados_UF_2012 <- aggregate(cheg_2012 ~Estado, data = Turismo, sum)
Estados_UF_2012

Estados_UF_2013 <- aggregate(cheg_2013 ~Estado, data = Turismo, sum)
Estados_UF_2013

Estados_UF_2014 <- aggregate(cheg_2014 ~Estado, data = Turismo, sum)
Estados_UF_2014

Estados_UF_2015 <- aggregate(cheg_2015 ~Estado, data = Turismo, sum)
Estados_UF_2015
########################################################################################################


############################ Ordenação das Categorias ##################################################
x1 <- Estados_UF_2012$Estado[order(Estados_UF_2012$cheg_2012)]
y1 <- sort(Estados_UF_2012$cheg_2012)/1000

x2 <- Estados_UF_2013$Estado[order(Estados_UF_2013$cheg_2013)]
y2 <- sort(Estados_UF_2013$cheg_2013)/1000

x3 <- Estados_UF_2014$Estado[order(Estados_UF_2014$cheg_2014)]
y3 <- sort(Estados_UF_2014$cheg_2014)/1000

x4 <- Estados_UF_2015$Estado[order(Estados_UF_2015$cheg_2015)]
y4 <- sort(Estados_UF_2015$cheg_2015)/1000

################################################################################################################################################


############################ Gr?fico de Barras  #################################################################################################
###### Visualiza??o via Pacote base

###### BRASIL - 2012 ##################################################################################
#par(mfrow = c(2,2))

par(mar = c(9,5,4,2), mai= c(1.8, 1.0, 0.8, 0.4))
Barra1 <- barplot( y1, names.arg = x1,
          main = "Chegada de Turistas ao Brasil em 2012",
          cex.main = 1.5,
          ylab = "Chegadas por mil habitantes",
          cex.names = 1,
          axisnames = T,
          las = 2,
          col = "white",
          border = "blue",
          font= 2,
          lwd = 2,
          ylim = c(0,1.2*max(y1)))
          text(Barra1, y1, label=round(y1,2), pos=3, cex=1.1, col="black")
          
#######################################################################################################

###### BRASIL - 2013 ##################################################################################
par(mar = c(9,5,4,2), mai= c(1.8, 1.0, 0.8, 0.4))
Barra2 <- barplot( y2, names.arg = x2,
                   main = "Chegada de Turistas ao Brasil em 2013",
                   cex.main = 1.5,
                   ylab = "Chegadas por mil habitantes",
                   cex.names = 1,
                   axisnames = T,
                   las = 2,
                   col = "red",
                   ylim = c(0,1.2*max(y1)))
                   text(Barra3, y2, label=round(y2,2), pos=3, cex=1.1, col="black")
#######################################################################################################

###### BRASIL - 2014 ##################################################################################
par(mar = c(9,5,4,2), mai= c(1.8, 1.0, 0.8, 0.4))
Barra3 <- barplot( y3, names.arg = x3,
                   main = "Chegada de Turistas ao Brasil em 2014",
                   cex.main = 1.5,
                   ylab = "Chegadas por mil habitantes",
                   cex.names = 1,
                   axisnames = T,
                   las = 2,
                   col = "green",
                   ylim = c(0,1.2*max(y1)))
                   text(Barra3, y3, label=round(y3,2), pos=3, cex=1.1, col="black")
#######################################################################################################

###### BRASIL - 2015 ##################################################################################
par(mar = c(9,5,4,2), mai= c(1.8, 1.0, 0.8, 0.4))
Barra4 <- barplot( y4, names.arg = x4,
         main = "Chegada de Turistas ao Brasil em 2015",
         cex.main = 1.5,
         ylab = "Chegadas por mil habitantes",
         cex.names = 1,
         axisnames = T,
         las = 2,
         col = "purple",
         ylim = c(0,1.2*max(y1)))
         text(Barra4, y4, label=round(y4,2), pos=3, cex=1.1, col="black")
#######################################################################################################

###### Compra??o de v?rios Gr?ficos ###################################################################      
par(mfrow = c(2,2))
################################################################################################################################################
               

############################ Gr?fico de Linhas #################################################################################################
###### S?O PAULO #################################################
Dados_SP <- Turismo[Turismo$Estado == "SaoPaulo",]
                
###### Definindo os valores dos eixos
a1 <- Dados_SP$Mes
b1 <- Dados_SP$cheg_2012/1000	
b2 <- Dados_SP$cheg_2013/1000	
b3 <- Dados_SP$cheg_2014/1000	
b4 <- Dados_SP$cheg_2015/1000	
      
###### Deinindo os limites do eixo y      
li <- min(b1,b2,b3,b4)
ls <- max(b1,b2,b3,b4)

###### Gr?fico de Linhas
plot(a1, b1,  lty = 1, lwd = 1, type = "b", ylim = c(0.8*li, ls*1.2))
lines(a1, b2, lty = 2, lwd = 1, type = "b")
lines(a1, b3, lty = 3, lwd = 2, type = "b")
lines(a1, b4, lty = 4, lwd = 1, type = "b")
title(main = "Chegada de Turistas em S?o Paulo de 2012 a 2015",
      xlab = "M?s",
      ylab = "Chegadas de Turistas por mil Habitantes",
      sub = "Fonte: Elaborado com o pacote graphics", 
      cex.sub = 0.8)
      legend(10, 400, 
      c("2012", "2013", "2014", "2015"), lty = 1:4, cex = 0.8)

###### Rio de Janeiro ###############################################
Dados_RJ <- Turismo[Turismo$Estado == "RioJaneiro",] 
      
###### Definindo os valores dos eixos
c1 <- Dados_RJ$Mes
d1 <- Dados_RJ$cheg_2012/1000	
d2 <- Dados_RJ$cheg_2013/1000	
d3 <- Dados_RJ$cheg_2014/1000	
d4 <- Dados_RJ$cheg_2015/1000	
      
###### Deinindo os limites do eixo y      
li <- min(d1,d2,d3,d4)
ls <- max(d1,d2,d3,d4)
      
###### Gr?fico de Linhas
plot(c1, d1,  lty = 1, lwd = 1, type = "b", ylim = c(0.8*li, ls*1.2))
lines(c1, d2, lty = 2, lwd = 1, type = "b")
lines(c1, d3, lty = 3, lwd = 2, type = "b")
lines(c1, d4, lty = 4, lwd = 1, type = "b")
title(main = "Chegada de Turistas no Rio de janeiro de 2012 a 2015",
      xlab = "M?s",
      ylab = "Chegadas de Turistas por mil Habitantes",
      sub = "Fonte: Elaborado com o pacote graphics", 
      cex.sub = 0.8)
      legend(10, 400, 
             c("2012", "2013", "2014", "2015"), lty = 1:4, cex = 0.8)
#################################################################################
   
      
############################ Gr?fico de BoxPlot ################################################################################################
# O Boxplot exibe medidas estat?sticas importantes para a compreen??o da forma e amplitude dos dados.
# ? um gr?fico utilizado para inspecionar a distribui??o de frequ?ncia de vari?veis quantitativas.
            
###### BRASIL
###### Distribui??o Frequ?ncia
x3 <- Turismo$cheg_2012/1000
x4 <- Turismo$cheg_2013/1000
x5 <- Turismo$cheg_2014/1000
x6 <- Turismo$cheg_2015/1000

###### Gr?fico Boxplot 
par(mfrow = c(2,2))
Box1 <- boxplot(x3,main = "Chegadas de Turistas ao Brasil em 2012",
       xlab = "Ano de 2012",
       ylab = "Chegada de Turistas por mil habitantes",
       col = "blue",
       horizontal = T) 
               
Box2 <- boxplot(x4,main = "Chegadas de Turistas ao Brasil em 2013",
                xlab = "Ano de 2013",
                ylab = "Chegada de Turistas por mil habitantes",
                col = "red",
                horizontal = T) 

Box3 <- boxplot(x5,main = "Chegadas de Turistas ao Brasil em 2014",
                xlab = "Ano de 2014",
                ylab = "Chegada de Turistas por mil habitantes",
                col = "green",
                horizontal = T) 

Box4 <- boxplot(x5,main = "Chegadas de Turistas ao Brasil em 2015",
                xlab = "Ano de 2015",
                ylab = "Chegada de Turistas por mil habitantes",
                col = "purple",
                horizontal = T) 
#######################################################################


               
               
               