##### Script TCC TERRAZAS F. R. Bovinos de leite

  #Carregando pacotes

    library(ggplot2)
    library(readxl)
  #Carregando base de dados
  dataset <- read_excel("externo.xlsx", col_types = c("date", 
                                                        "text", "text", "date", "numeric", "numeric", 
                                                        "numeric", "text", "skip", "skip"))
  #Definindo as colunas com as variaveis desejadas.
      
  dataset$data<-dataset$Data # definição da coluna DATA
  dataset$temp<-dataset$Temperatura # denifição da coluna TEMPERATURA
  dataset$ur<-dataset$Umidade #definição da coluna de UMIDADE
  dataset2<-subset(dataset,select = c(data,temp,ur))
  dataset2<-na.omit(dataset2)
  dataset2$temp<-as.numeric(dataset2$temp) #definição da variavel/coluna de temperatura como valor numerico
  dataset2$ur<-as.numeric(dataset2$ur) # definição da variavel/coluna de umidade como valor numerico

p <- ggplot(dataset2, aes(x = data))
p <- p + geom_line(aes(y = temp, colour = "Temperatura"))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = ur/4, colour = "Umidade"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*4, name = "Umidade Relativa (%)"))

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("red", "blue"))
p <- p + labs(y = "Temperatura do ar (°C)",
              x = "Data",
              colour = "Parâmetro")
p <- p + theme(legend.position = "bottom")
p

library(dplyr)
library(ggplot2)

library(readxl)

dataset <- read_excel("C:/Users/f441605/Desktop/dados tcc.xlsx", 
                        sheet = "externo", col_types = c("numeric", 
                                                         "text", "date", "date", "numeric", 
                                                         "numeric", "text", "text"))


dataset$Periodo = as.factor(dataset$Periodo)
# boxplot horario e temperatura

ggplot(dataset, aes(Periodo, Temperatura))+
  geom_boxplot(fill = "grey60", alpha = 0.8)+
  theme_bw(16)


dataset$Periodo = factor(dataset$Periodo,levels(dataset$Periodo)[c(1,3,2)]) 
ggplot(dataset, aes(Periodo, Temperatura))+
  geom_boxplot(fill = "grey60", alpha = 0.8)+
  theme_bw(16)

#boxplot horario e umidade relativa
ggplot(dataset, aes(Periodo, Umidade))+
  geom_boxplot(fill = "grey60", alpha = 0.8)+
  theme_bw(16)


dataset$Periodo = factor(dataset$Periodo,levels(dataset$Periodo)[c(1,2,3)]) 
ggplot(dataset, aes(Periodo, Umidade))+
  geom_boxplot(fill = "grey60", alpha = 0.8)+
  theme_bw(16)

library(readxl)
data <- read_excel("C:/Users/f441605/Desktop/IN_FRIO_II.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "text", "numeric", "numeric"))
data     
data$Periodo = as.factor(data$Periodo)
data$Periodo = factor(data$Periodo,levels(data$Periodo)[c(1,3,2)]) 
ggplot(data, aes(Periodo, Temperatura), xlim = c(0, 10), ylim = c(0, 10))+
  geom_boxplot(fill = "grey60", alpha = 0.8)+
  theme_bw(16)

ggplot(data, aes(Periodo, Temperatura))+
  geom_boxplot(fill = "grey60", alpha = 0.8)+
  theme_bw(16)
#############################################################
# agora os gráficos de umidade.
# dados in media
library(readxl)
data2 <- read_excel("C:/Users/f441605/Desktop/IN_MEDIA_I.xlsx", 
                    col_types = c("numeric", "text", "text", 
                                  "text", "numeric", "numeric"))
data2
data2$Periodo = as.factor(data2$Periodo)
data2$Periodo = factor(data2$Periodo,levels(data2$Periodo)[c(1,3,2)]) 
ggplot(data2, aes(Periodo, Umidade))+
  geom_boxplot(fill = "grey60", alpha = 0.8)+
  theme_bw(16)

ggplot(data2, aes(Periodo, Umidade))+
  geom_boxplot(fill = "grey60", alpha = 0.8)+
  theme_bw(16)


#dados in frio
library(readxl)
dados3 <- read_excel("C:/Users/f441605/Desktop/IN_FRIO_II.xlsx", 
                     col_types = c("numeric", "text", "text", 
                                   "text", "numeric", "numeric"))
dados3
dados3$Periodo = as.factor(dados3$Periodo)
dados3$Periodo = factor(dados3$Periodo, levels(dados3$Periodo)[c(1,3,2)])
ggplot(dados3, aes(Periodo, Umidade))+
  geom_boxplot(fill = "grey60", alpha = 0.8)+
  theme_bw(16)

ggplot(dados3, aes(Periodo, Umidade))+ 
  geom_boxplot(fill = "grey", alpha = 0.8)+
  theme_bw(16)
temperatura = dados3$Temperatura
temperatura
Periodo = dados3$Periodo
Periodo
horario = dados3$Horario
plot(temperatura,horario)
anova<-aov(Periodo,temperatura)

dados3.anova <- aov(dados3$Temperatura ~ dados3$Periodo)
summary(dados3.anova)
dados3.anovaur <- aov(dados3$Umidade ~ dados3$Periodo)
summary(dados3.anovaur)


