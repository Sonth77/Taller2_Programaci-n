#TALLER 2
#AUTORES:
#Yefri Esteban Sanchez Almanza
#Olga Maria Caballero Yance
#Yuleidys Mejia Gutierrez
#Yeiner Jose Duran Diaz
#PRIMER PUNTO
#EN PRIMER LUGAR SE LLAMAN LAS LIBRERIAS CORRESPONDIENTES PARA CADA COMANDO
library(gapminder)
library(ggplot2)
library(dplyr)
library(openxlsx)
#CREAMOS UNA FUNCI�N LLAMADA main1() PARA EL MENU Y SU POSTERIOR DESARROLLO
main1<- function(){
  i<-0
  #LA FUNCI�N WHILE ES UN CICLO QUE SOLO TERMINAR� CUANDO EL USUARIO INGRESE EL NUMERO CONDICIONADO
  while (i!=6) {     
    print("1. Exportar el archivo gapminder reemplazando el 10% de lifeExp, pop, gdpPercap a NA.")
    print("2. Importar el archivo gapminder.xlsx.")
    print("3. Grafica de dispersi�n lifeExp vs pop.")
    print("4. Grafica de dispersi�n gdpPercap vs pop.")
    print("5. Grafica de diagrama de cajas por continente de gdpPercap de los a�os 1990 a 2007.")
    print("6. Salir")
    #CREAMOS UN INPUT CONVERTIDO A INT PARA QUE EL USUARIO ELIJA QUE PROCEDIMIENTO DESEA REALIZAR
    i<- as.integer(readline("Escoja una opci�n: "))
    #SE CREAN LOS CONDICIONALES DE EJECUCI�N
    ##ESTE IF ES CORRESPONDIENTE AL PUNTO 1.A QUE CONVIERTE POSICIONES ALEATORIAS A NA
    #EN TRES DE LAS COLUMNAS AL GAPMINDER Y EXPORTA EL GADMINDER CON ESTOS CAMBIOS EN FORMATO XLSX
    if(i == 1){
      d=dim(gapminder)#Guardamos las dimensiones en una variable
      x=(as.integer(0.1*d[1]))#Extraemos el 10% del largo del data frame
      indice1=sample(1:d[1],x, replace = F)#Creamos 3 variables de funci�n sampe para conservar la aleatoriedad que se desea
      indice2=sample(1:d[1],x, replace = F)
      indice3=sample(1:d[1],x, replace = F)
      gapminder$lifeExp[indice1]=NA#Tenidas las posiciones aleatorias las convertimos a NA 
      gapminder$gdpPercap[indice3]=NA
      gapminder$pop[indice2]=NA
      write.xlsx(gapminder, "gapminder_open.xlsx") #Exportamos el archivo
      cat('\n')
      print("Archivo exportado")#Le pedimos que avise al usuario del procedimiento
      cat('\n')
      #ESTE ELIF IF ES CORRESPONDIENTE AL PUNTO 1.B DE IMPORTAR EL ARCHIVO EN FORMATO XLSX
    }else if(i == 2){
      df<- read.xlsx("gapminder_open.xlsx")#Llamos al archivo
      cat('\n')
      print("Archivo importado")#Le pedimos que avise al usuario del procedimiento
      cat('\n')
      j<- (readline("�Desea ver el archivo?\n"))#Preguntamos si el usuario quiere ver el archivo
      cat('\n')
      if (j=="si" | j=="SI"){  #Creamos el condicional para mostrar o no el archivo
        print(df)
      }else{
        cat('\n')
        print("Continue")
        cat('\n')
      }
#ESTE ELSE IF ES CORRESPONDIENTE AL PUNTO 1.C PARA LA GRAFICA DE DISPERSI�N DE
      #LIFEEXP VS POP 
    }else if (i==3){
      p1<-mean(df$lifeExp,na.rm=TRUE)#Teniendo en cuenta que existen valores nulos los cambiaremos por el promedio correspondiente
      df$lifeExp[indice1]=p1#de manera que cada NA ser� igual al promedio de cada columna
      p2<-mean(df$pop,na.rm=TRUE)
      df$pop[indice2]=p2
      p3<-mean(df$gdpPercap,na.rm=TRUE)
      df$gdpPercap[indice3]=p3
      cat('\n')
      print(ggplot(df, aes(lifeExp, pop, col=continent))+geom_point()+#ggplot grafica las columnas con las cuales estamos trabajando
              labs(x="LifeExpo", y="Poblaci�n", 
                   title="Dispersi�n de LifeExp vs Poblaci�n"))
      cat('\n')
      #ESTE ELSE IF ES CORRESPONDIENTE AL PUNTO 1.D PARA LA GRAFICA DE DISPERSI�N DE
      #GDPPERCAP VS POP
    }else if (i==4){
      cat('\n')
      print(ggplot(df, aes(log(gdpPercap), pop, col=continent))+geom_point()+#ggplot grafica las columnas con las cuales estamos trabajando
              labs(x="GdpPercap", y="Poblaci�n", 
                   title="Dispersi�n de GdpPercap vs Poblaci�n"))
      cat('\n')
      #ESTE ELSE IF ES CORRESPONDIENTE AL PUNTO 1.E PARA EL DIAGRAMA DE CAJAS DE
      #GDPPERCAP POR CONTINENTE ENTRE LOS A�OS 1990 Y 2007
    }else if (i==5){
      df1<-df %>% select(continent, year, gdpPercap) %>% filter(year <= 2007) %>% 
      filter(year >= 1990)#Con dplyr creamos un nuevo data frame con los datos necesarios para el diagrama
      boxplot(df1$gdpPercap~df1$continent,
        xlab="Continentes",ylab="GdpPercap",
          main="Diagrama de caja de GdpPercap por continente entre los a�os 1990 a 2007")#boxplot crea el diagrama
      #ESTE ELSE IF SIMPLEMENTE SE DESPIDE DEL USUARIO Y CIERRA EL PROGRAMA
    }else if (i==6){
      cat('\n')
      print("Muchas gracias, adios.")
      cat('\n')
    }
  }
}
#EJERCICIO 2 DEL TALLER 2 
##LA FUNCI�N EST() CREAR� DOS EXPERIMENTOS CON VALORES DE UNA DISTRIBUCI�N NORMAL PARA
#POSTERIORMENTE ANALIZAR SUS RELACIONES POR MEDIO DE ALGUNAS FUNCIONES ESTADISTICAS
est<-function(){
  print("Bienvenido a algunas relaciones estadisticas entre experimentos normales")
  cat('\n')#En primer lugar le damos la vienvenida al usuario
  cat('\n')
  #A continuaci�n, explicaremos un poco la estructura de los experimentos al usuario
  print("Dado que se necesitan dos experimentos con valores de una distribuci�n normal, ingrese:")
  #Y luego, pediremos al usuario que ingrese la longitud, media y desviaci�n estandar de los experimentos a & b.
  j<-as.integer(readline("Por favor, ingrese la longitud de los datos para ambos experimentos: \n"))
  x<-as.integer(readline("Por favor, ingrese la media para el experimento_a: \n"))
  x1<-as.integer(readline("Por favor, ingrese la media para el experimento_b: \n"))
  d<-as.integer(readline("Por favor, ingrese la desviaci�n estandar para el experimento_a: \n"))
  d1<-as.integer(readline("Por favor, ingrese la desviaci�n estandar para el experimento_b: \n"))
  #Asi, creamos las variables y la funci�n que generar� los datos de los experimentos
  vec1=rnorm(j,x,d)
  vec2=rnorm(j,x1,d1)
  #Los exportamos en formato csv
  write.csv(vec1, "Experimento_a.csv")
  write.csv(vec2, "Experimento_b.csv")
  #Y los importamos en nuevas variables 
  vec1a<-read.csv("Experimento_a.csv")$x
  vec2a<-read.csv("Experimento_b.csv")$x
  #Creamos una funci�n que divida la pantalla y permita que el usuario vea los dos 
  #histogramas de los experimentos al tiempo, para un analisis visual.
  layout(matrix(c(1:2),nrow=1, byrow=FALSE))
  layout.show(2)
  hist(vec1a, main = "Histograma del experimento A", xlab = "Experimento_a")
  hist(vec2a, main = "Histograma del experimento B", xlab = "Experimento_b")
  #POR ULTIMO, SE CREA UN MEN� PARA QUE EL USUARIO OBSERVE CADA RELACI�N ESTADISTICA QUE DESEE
  i=0
  while (i!=4){
    print("1. �Son las medias estadisticamente significativas?")
    print("2. Correlaci�n de Pearson y Spearman para los experimentos.")
    print("3. Diagrama de dispersi�n con linea de tendencia.")
    print("4. Salir")
    i<-as.integer(readline("Ingrese una opci�n:"))
    #Este if dice si las diferencias de las medias son estadisticamente significativas
    #para esto se usa la funci�n t.test() y se determina su significancia por medio del p-valor
    if (i==1){
      h<-t.test(vec1a,vec2a)
      h1<-h$p.value
      cat(sprintf("El p-valor de los experimentos es: \n %s", h1))
      if(h1>0.05){
        cat('\n')
        print("Las medias no presentan diferencias estadisticamente significativas")
        cat('\n')
      }else{
        cat('\n')
        print("Las medias presentan diferencias estadisticamente significativas")
        cat('\n')
      }
      #Este else if corresponde a mostrar las correlaciones pearson y spearman de los dos experimentos.
    }else if(i==2){
      cat('\n')
      print("Correlaci�n de Pearson es:")
      cat('\n')
      print(cor(x=vec1a,y=vec2a))#La funci�n cor() muestra por defecto la correlaci�n Pearson. 
      cat('\n')
      cat('\n')
      print("Correlaci�n de Spearman es:")
      cat('\n')
      print(cor(x=vec1a,y=vec2a, method = "spearman"))#Para ver la correlaci�n spearman se usa method para que se logre esto.
      cat('\n')
      #Este ultimo else if muestra los datos por medio de un diagrama de dispersi�n con una linea de tendencia
    }else if(i==3){
      vecT<- data.frame(vec1a, vec2a)
      print(ggplot(vecT, aes(x=vec1a,y=vec2a))+geom_point()+
              geom_smooth(method="lm", colour="Red")+
              labs(x="Experimento_a", y="Experimento_b", 
                   title="Diagrama de dispersi�n con liena de tendencia" ))
      #Y con este se sale del ciclo while, finalizando el programa.
    }else if (i==4){
      cat('\n')
      print("Muchas gracias, adios.")
      cat('\n')
    }
  }
}
#EJERCICIO 3 DEL TALLER 2
#Para el desarrolo de este punto se deben tener en cuenta algunas cosas:
#Para obtener una lista completa de las distribuciones disponibles en R puede utilizar 
#el siguiente comando: help("Distributions")
#Nos piden mostrar en pantalla el grafico de 4 distribuciones en especial:
#Bernoulli (bern) y Poisson (pois) que son distribuciones discretas que estan dadas por la probabilidad de la funci�n de masa.
#Uniforme (unif) y Exponencial (exp) que son distibuciones continuas que estan dadas por la funci�n de densidad de probabilidad.
#El comando para cada distribuci�n est� precedido de una letra para indicar la funcionalidad:
#d: devuelve la funci�n de densidad de probabilidad

#INICIAMOS EL CODIGO

install.packages("Rlab")
library(Rlab) #Importamos esta libreria para utilizarla en la distribucion Bernoulli.
#Creamos nuestro men� 
main <- function(){
  print("1. Ver la funci�n de densidad de una distribuci�n uniforme.")
  print("2. Ver la funci�n de densidad de una distribuci�n Bernoulli.")
  print("3. Ver la funci�n de densidad de una distribuci�n Poisson.")
  print("4. Ver la funci�n de densidad de una distribuci�n Exponencial.")
  val <- as.integer(readline("Ingresar la opci�n que desea ver: "))
  x <- rnorm(100) #Se crean 100 datos aleatorios para luego utilizarlos.
  if (val==1){ #Si seleccionamos el 1 entonces se nos mostrar� el grafico de la distribucion que le corresponde.
    curve(dunif(x),xlim=c(130,210),col="blue",lwd=2,
          xlab="x",ylab="f(x)",main="Funci�n de Densidad de una distribuci�n uniforme")
  } else { if (val ==2){
    x <- seq(0, 10, by = 1) #Valores de x para la funci�n dbern()
    #Usando la fuunci�n dbern() en x para obtener el PDF de Bernoulli correspondiente
    y <- dbern(x, prob = 0.7) #La funci�n dbern() en la programaci�n R mide la funci�n de densidad de la distribuci�n de Bernoulli.
    #prob es la probabilidad de �xito en cada prueba
    plot(y, type = "o",col="blue",lwd=2,
         xlab="x",ylab="f(x)",main="Funci�n de masa de una distribuci�n Bernoulli") #Dicho grafico.
    
  } else { if (val ==3){
    x <- 0:50  # Valores del eje X (x = 0, 1, 2, ...)
    lambda <- 5 # N�mero medio de eventos que ocurren en el intervalo.
    plot(dpois(x, lambda), type = "h", lwd = 2,
         main = "Funci�n de masa de probabilidad de una distribuci�n Poisson",
         ylab = "P(X = x)", xlab = "N�mero de eventos") 
    
  } else { if (val ==4){
    #Si optammos por escribir el 4 este creara una un grafico de la funcion exponencial con los 100 datos aleatorios que creamos anteriores.
    curve(dexp(x),xlim=c(130,210),col="blue",lwd=2,
          xlab="x",ylab="f(x)",main="Funci�n de Densidad de una distribuci�n Exponencial")
  } 
    else {
      print("Ninguna de las anteriores")
    }
    
  }
  }
  }
}
Autores<-function(){
  print("Muchas gracias por usar nuestr programa, esperamos y sea de su utilidad.")
  print("Autores:")
  print("Yefri Esteban Sanchez Almanza") 
  print("Olga Maria Caballero Yance ")
  print("Yuleidys Mejia Gutierrez ")
  print("Yeiner Jose Duran Diaz ")
} 
Autores()
