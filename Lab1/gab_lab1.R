#setwd("~/UFCG")
dados <- read.csv("Lab1/adult.csv",head = T)

#variavel Age
summary(dados$age)
sd(dados$age)
var(dados$age)
IQR(dados$age)
freq <- table(dados$age)
moda <- (names(freq)[which.max(freq)])

png("variavel_age.png",width = 300, height = 300)
hist(dados$age, main = "Histograma (a)", xlab = "Idade", ylab = "Frequ?ncia", breaks = 9)
abline(v = median(dados$age), col = "red", lty = 3, lwd = 2)
dev.off()

###questao 3###

png("boxplots.png", width = 1400, height = 800)
par(mfrow = c(2,4))
boxplot(dados$age, main = "Boxplot - Age")
boxplot(dados$fnlwgt, main = "Boxplot - FNLWGT")
boxplot(dados$education.num, main = "Boxplot - N?vel Edu.")
boxplot(dados$capital.gain, main = "Boxplot - Ganho de Capital")
boxplot(dados$capital.loss, main = "Boxplot - Perda de Capital")
boxplot(dados$hours.per.week, main = "Boxplot - Horas de Trab.")
boxplot(dados$Wages, main = "Boxplot - Sal?rio")
dev.off()

###questao 4###
proporcao <- mean(dados$Wages)/mean(dados$hours.per.week)

###questao 2###
profissao <- aggregate(dados$Wages,list("Profissao"= dados$occupation), FUN = mean )
genero <- aggregate(dados$Wages,list("Genero"= dados$sex), FUN = mean )
etnia <- aggregate(dados$Wages,list("Cor"= dados$race), FUN = mean )
pais_origem <- aggregate(dados$Wages,list("Pais.origem"= dados$native.country), FUN = mean )
nivel_educacional <- aggregate(dados$Wages,list("Nivel.educacional"= dados$education), FUN = mean )

profissao <- profissao[profissao$x == max(profissao$x), ]
genero <- genero[genero$x == max(genero$x), ]
etnia <- etnia[etnia$x == max(etnia$x), ]
pais_origem <- pais_origem[pais_origem$x == max(pais_origem$x), ]
nivel_educacional <- nivel_educacional[nivel_educacional$x == max(nivel_educacional$x), ]

#ganha menos que 20000 ou mais que 80000
dados <- dados[dados$Wages < 20000 | dados$Wages > 80000, ]
profissao <- aggregate(dados$Wages,list("Profissao"= dados$occupation), FUN = mean )
genero <- aggregate(dados$Wages,list("Genero"= dados$sex), FUN = mean )
etnia <- aggregate(dados$Wages,list("Cor"= dados$race), FUN = mean )
pais_origem <- aggregate(dados$Wages,list("Pais.origem"= dados$native.country), FUN = mean )
nivel_educacional <- aggregate(dados$Wages,list("Nivel.educacional"= dados$education), FUN = mean )

profissao <- profissao[profissao$x == max(profissao$x), ]
genero <- genero[genero$x == max(genero$x), ]
etnia <- etnia[etnia$x == max(etnia$x), ]
pais_origem <- pais_origem[pais_origem$x == max(pais_origem$x), ]
nivel_educacional <- nivel_educacional[nivel_educacional$x == max(nivel_educacional$x), ]

