#This is a script to play with loops and functions with null models as an excuse


#A simple null model to test correlations------

#the data
abundances <- c(1,3,4,7,8,13)
body_size <- c(9,6,3,3,1,1)

#check correlations
plot(abundances, body_size)
cor.test(abundances, body_size)

#let's break this pattern
#Random samples and permutations

?sample

sample_vector <- sample(body_size, size = length(body_size), replace = FALSE)
cor.test(abundances, sample_vector)

#learn loops
#la primera vez que corre, i = 1, hasta 999 veces que va a correr: for(i in 1:999)
#saca en pantalla: print

for(i in 1:10){
  print(paste("numero", i))
}

#truco: para tener un vector con el output, primero crear un vector vacio
out <- c()
for(i in 1:10){
  out[i] <- paste("numero", i)
}
out


#loop que vaya calculando correlaciones al azar
out <- c()
for(i in 1:999){
  out[i] <- paste(cor(abundances, sample(body_size, size = length(body_size), replace = FALSE)))
}
out

out <- as.numeric(out)
out

hist(out)
corr <- cor(abundances, body_size)
lines(c(corr,corr), c(0,150), col = "red")
#con esto vemos que nuestra correlación no se debe al azar, porque cae en donde la frecuencia de las permutaciones es muy baja

#P-value
length(which(out < corr)/length(out))

#Is our community uneven?
abundances
#let's calculate Pielou's index = Shannon/log(Rhichness)

#abundancia relativa = abundance/sum(abundance)
p <- abundances / sum(abundances)
p
ln_p <- log2(p)
ln_p

J <- -sum(p*ln_p)/log2(length(abundances))
J

#funcionalizamos
J <- function(x){
  p <- x / sum(x)
  ln_p <- log2(p)
  J <- -sum(p*ln_p)/log2(length(x))
  J
}
J(x = abundances)


#is this evenness high

rand <- sample(x = c(1:6),size = sum(abundances), replace = TRUE)
null_abundances <- table(rand)
J(null_abundances)

#let's create the distribution
out <- c()
for(i in 1:99){
  rand <- sample(x = c(1:6), size = sum(abundances), replace = TRUE)
  null_abundances <- table(rand)
  out[i] <- J(null_abundances)
}
hist(out)
eve <- J(abundances)
lines(c(eve,eve), c(0,60), col = "red")
#P-value
length(which(out < eve))/length(out)
