#data <- dane wczytane z bazy danych (model w pythonie, zmienna data przekazana z pythona)
data <- data.matrix(data)

#rekomendacja najłatwiejszej ścieżki dla studenta

recomEasySub <- function(student) {
  getSupport <- function(row) {length(subset(data, data[,row] != 0)[,row])}
  getMean <- function(row) {mean(subset(data, data[,row] != 0)[,row])}
  studCount <- dim(data)[1]
  supp <- unlist(lapply(31:50,getSupport))/studCount
  mean <- (unlist(lapply(31:50,getMean))-4)/7
  easyRate <- 1/3*supp + 2/3*mean
  A <- matrix(c(easyRate,1:20),20,2)
  A <- A[order(A[,1], decreasing=TRUE),]
  studNotChosen <- which(student == 0)
  recom <- A[A[,2] %in% studNotChosen,2]
  return(recom)
}

#algorytm knn

#wartosc 0, gdy student nie wziął przedmiotu, chcemy mocniej wyrazić to, gdy studenci 
#wzięli różne przedmioty
dist <- function(v,w) {
  sum <- 0
  for (i in 1:length(v)) {
    if (xor((v[i] == 0),w[i] == 0)) {
      sum <- sum + 10
    }
    else {
      sum <- sum + abs(v[i] - w[i])
    }
  }
  return(sum)
}

recomNearestSub <- function(k, student) {
  n <- dim(data)[1]
  A <- matrix(0,n,2)
  for (i in 1:n) {
    A[i,1] <- dist(student,data[i,])
    A[i,2] <- i
  }
  bestNb <- A[order(A[,1])[1:k],2]
  studNotChosen <- which(student == 0)
  recom <- c()
  for (i in 1:length(bestNb)) {
    nbSub <- which(data[bestNb[i],31:50] > 0)
    recom <- unique(c(recom, nbSub[nbSub %in% studNotChosen]))
  }
  return(recom)
}

#algorytm lososwy :)

random <- function(student) {
  studNotChosen <- which(student == 0)
  n <- length(studNotChosen)
  recom <- sample(studNotChosen,prob=rep(1,n))[1:(n/2)]
  return(recom)
}

#rekomendacja seminariów

#funkcja obliczająca modę
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

recomNearestSem <- function(k, student, data) {
  n <- dim(data)[1]
  A <- matrix(0,n,2)
  for (i in 1:n) {
    A[i,1] <- dist(student,data[i,])
    A[i,2] <- i
  }
  bestNb <- A[order(A[,1])[1:k],2]
  bestSem <- data[bestNb,51]
  return(mode(bestSem))
}

#skutecznosc klasyfikatorów

n <- dim(data)[1]
trainingIdx <- sample(n,round(3/5*n))
dataTrain <- data[trainingIdx,]
dataTest <- data[-trainingIdx,]

m <- dim(dataTest)[1]
odp <- c()
for (i in 1:m) {
  odp <- c(odp,recomNearestSem(8,dataTest[i,], dataTrain))
}
mean(odp==dataTest[1:477,51])

classifier<-randomForest(dataTrain[,1:50], dataTrain[,51]) 
odp <- predict(classifier, dataTest[,-51])
odp <- round(odp)
mean(odp[1:477]==dataTest[1:477,51])
#lepszy knn niz randomForest
