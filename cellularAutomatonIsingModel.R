#łancuchy Markowa
# X_0, ..., X_n -> lancuch Markowa o macierzy (1-a, a, b, 1-b), obliczamy asymptotyczną wariancję 
# lim (n-> inf) n*Var(1/n (sum X_i)) = sigma^2

# a=b = 1/2 => sigma^2 = 1/4

n=100
markow<-function(a,b,m) {
  X=1:m
  X[1]=1
  for (i in 1:(m-1)) {
    if (X[i]==1) {
      X[i+1]=rbinom(1,1,a)+1
    }
    if (X[i]==2) {
      X[i+1]=rbinom(1,1,1-b)+1
    }
  }
  X
}
A<-replicate(100000,markow(0.8,0.8,n))
B<-apply(A,2,sum)
var <- n*var(B/n)
a=0.8
b=0.8
j=1-a-b
a*b*(1+j)/(1-j)/((a+b)^2)   # wariancja normalna, powinna wyjsc podobna do tej co wyszła zasymulowana

# dla łańcuchów ciągłych: 
# 1/n sum X_i -> E_pi X = 1.5

# X_(n+1) = a*X_n + W_(n+1), W_i = N(0,1)
# X_n -> pi = N(0, sigma^2) -> rozkład X_n dąży do rozkładu stacjonarnego, sigma_st^2 = 1/(1-a^2)
markowC <- function(a,m)
{
  X = 1:m;
  X[1] = 0
  for (i in 1:(m-1))
  {
    X[i+1] <- a*X[i] + rnorm(1,0,1)
  }
  X
}
n <- 10000
m <- 100
a <- 1/2
A <- replicate(n,markowC(a,m))
B <- apply(A,2,sum)
var_as <- m*var(B/m)
var_as
fact_var_as <- (1/(1-a^2))*((1+a)/(1-a))
fact_var_as
# sprawdzamy rozkład stacjonarny
var_st <- var(A[m,])
var_st
fact_var_st <- (1/(1-a^2))
fact_var_st

#model narodzin i smierci

# X(t) -> liczba czastek w pudelku w chiwli t
# intensywnosc wpadania i wypadania, 
# lambda -> parametr rozkładu wykladniczego, rozklad trwania zycia, p -> parametr rozkladu poissona, rozklad narodzin 
# P(X(t+h)=n+1|X(t)=n) = lambda*h + o(h)
# P(X(t+h)=n-1|X(t)=n) = p*n*h + o(h)

# Q(n,n+1) = lambda
# Q(n,n-1) = p*n
# Q(n,n) = -(lambda+p*n)

#stosujemy algorytm Gillespiego do zasymulowania procesu

gillespie <- function(p,lambda,m)
{
  X = 1:m
  T = 1:m
  X[1] = rpois(1,p)
  T[1] = 0
  for (i in 1:(m-1))
  {
    T[i+1] <- T[i] + rexp(-(lambda+p*X[i]))
  }
  X
}


#model Isinga
M <- matrix(1:25,5,5)
image(M)
M <- matrix(rnorm(10000),100,100)
image(M) 

#algorytm

d <- 100 #wielkosc mapy
#swiat poczatkowy
M <- matrix(sample(1:3,(d+2)^2,replace=TRUE),d+2,d+2)  #losowanie ze zwracaniem z krawedziami
M[1,] <- 1
M[d+2,] <- 1
M[,1] <- 1
M[,d+2] <- 1
image(M)
kolory <- c("gray","yellow","blue")
image(M,col=kolory)

nowy <- function(x,y) {
  nbrs <- c(M[x-1,y],M[x+1,y],M[x,y-1],M[x,y+1])
  if (all(nbrs==1)) {
    return(sample(1:3,1,prob=c(0.01,1,1)))
  }
  if (all(nbrs==1 | nbrs == 2)) {
    return(sample(1:2,1,prob=c(0.01,1)))
  }
  if (all(nbrs==1 | nbrs == 3)) {
    return(sample(c(1,3),1,prob=c(0.01,1)))
  }
  return(1)
}

# zapuszczamy automat
n <- 100
for (i in 1:n) {
  #tmp <- matrix(1,d+2,d+2)
  for (i in (2:(d+1))) {
    for (j in (2:(d+1))) {
      if ((i+j) %% 2 == 0) {M[i,j] <- nowy(i,j)}
    }
  }
  for (i in (2:(d+1))) {
    for (j in (2:(d+1))) {
      if ((i+j) %% 2 == 1) {M[i,j] <- nowy(i,j)}
    }
  }
  image(M, col = kolory, asp=1)
}