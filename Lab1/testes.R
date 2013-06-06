# testes

x = rnorm(100) 
hist(x, main = "Media, Mediana e Moda de uma Distribuicao simetrica") 
abline(v = mean(x), col=2, lty=2, lwd=2) 
abline(v = median(x), col=3, lty=3, lwd=2) 
abline(v = which.max(table(x)), col=4, lty=4, lwd=2)
ex12 = expression(media, mediana, moda) 
utils::str(legend("topleft", ex12, col = 2:4, lty=2:4, lwd=2))