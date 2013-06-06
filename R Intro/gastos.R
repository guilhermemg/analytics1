
gastos = read.csv("R Intro/gastos.csv");

valores_maiores_que_20000 = gastos$valor[gastos$valor > 20000]

hist(valores_maiores_que_20000)

#gastos_10 = gastos[1:10,]
#gastos_10

gastos_ordenados_por_valor = tail(gastos[order(gastos$valor),], n=5)
gastos_ordenados_por_valor

gastos_ordenados_por_valor[1:5,4,]

descricoes_dos_maiores_gastos = gastos_ordenados_por_valor[1:5,4,]

write.table(descricoes_dos_maiores_gastos,"out.txt")
