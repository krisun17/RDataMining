data <- data.matrix(read.csv("~/Programowanie/DataMining/zadanie2/salesdata_training.csv", header=TRUE))
dec <- data.matrix(read.csv("~/Programowanie/DataMining/zadanie2/salesdata_training_targets.csv", header=FALSE))
test <- data.matrix(read.csv("~/Programowanie/DataMining/zadanie2/salesdata_test.csv", header=TRUE))
colnames(dec) <- c("dec")
dataDec <- cbind(data,dec)

#dyskretyzacja (zaokraglenie wartosci)
dataDec[,sapply(dataDec, is.numeric)] <-round(dataDec[,sapply(dataDec, is.numeric)],0)

# wypełnianie brakującymi wartosciami
dataNew <- NULL
block <- 10000
for (i in 0:20) {
  dataPrep <- dataDec[(i*block+1):(i*block+block),]
  for (row in 1:block) {
    closed <- dataPrep[row,33]
    free <- c((11-closed):10,(18-closed):17,(25-closed):24, (32-closed):31)
    dataPrep[row,free] <- 0
    nas <- is.na(dataPrep[row,4:31])
    v <- dataPrep[row,4:31][!nas]
    n <- length(v)-4*closed
    m <- round(sum(v)/n)
    dataPrep[row,4:31][nas] <- m
  }
  dataNew <- rbind(dataNew, dataPrep)
  print(paste("przetworzony", i, "zestaw danych",Sys.time()))
}
write.csv(dataNew,"~/Programowanie/DataMining/zadanie2/salesdata_new_prepared.csv")


#[reparowanie testu

test[,sapply(test, is.numeric)] <-round(test[,sapply(test, is.numeric)],0)
testLen <- dim(test)[1]
dataPrep <- test
for (row in 1:testLen) {
  closed <- dataPrep[row,33]
  free <- c((11-closed):10,(18-closed):17,(25-closed):24, (32-closed):31)
  dataPrep[row,free] <- 0
  nas <- is.na(dataPrep[row,4:31])
  v <- dataPrep[row,4:31][!nas]
  n <- length(v)-4*closed
  m <- round(sum(v)/n)
  dataPrep[row,4:31][nas] <- m
}

write.csv(dataPrep,"~/Programowanie/DataMining/zadanie2/salesdata_prepared_test.csv")

dataTrain <- read.csv("~/Programowanie/DataMining/zadanie2/salesdata_new_prepared.csv", header=TRUE)
dataTest <- read.csv("~/Programowanie/DataMining/zadanie2/salesdata_prepared_test.csv", header=TRUE)


#podzbior treningu, ktory nas interesuje
weeks <- c(49,50,51,1,2,3,6,7,8)
dataTrain <- subset(dataTrain, weekOfYear %in% weeks)

#klasyfikator
dist <- function(r,m,v,w) {
  d <- 0
  if (v[[1]] != w[[1]]) {d = d + r}
  if (v[[2]] != w[[2]]) {d = d + r}
  if ((v[[3]] == 52 && w[[3]] == 1) || (w[[3]] == 52 && v[[3]] == 1)) {
    d = d + m
  }
  else {
    d = d + m*(abs(v[[3]]-w[[3]]))
  }
  return(d)
}

classify <- function(k, r, m, vec, data) {
  n <- dim(data)[1]
  A <- matrix(0,n,2)
  v <- unlist(vec[c(1:2,36)])
  for (row in 1:n) {
    w <- unlist(data[row,c(1:2,36)])
    ds <- dist(r,m,v,w)
    A[row,1] = row
    A[row,2] = ds
  }
  clRows <- A[order(A[,2])[1:k],1]
  p <- A[order(A[,2])[1:k],2]
  pn <- exp(-p[1]/10)
  p <- sum(p)-p
  p <- p/sum(p)
  n <- length(clRows)
  sum <- 0
  wyn <- 0
  for (i in 1:n) {
    v <- unlist(data[clRows[i],])
    wyn <- wyn + p[i]*mean(c(sum(v[4:10]), sum(v[11:17]), sum(v[18:24]), sum(v[25:31])))
  }
  v <- unlist(vec)
  wyn <- round(pn*wyn+(1-pn)*mean(c(sum(v[4:10]), sum(v[11:17]), sum(v[18:24]), sum(v[25:31]))))
  return(wyn)
}

#klasyfikacja
n <- dim(dataTest)[1]

odp <- c()
for (i in 0:8) {
  for (j in 1:1000) {
    row <- 500*i + j
    week <- dataTest[row,36]
    weeks <- c(week-1,week,week+1)
    posNbrs <- subset(dataTrain, dataTrain[,36] %in% weeks)
    odp <- c(odp,classify(7,50,1,dataTest[row,],posNbrs))
  }
  write.csv(odp, paste("~/Programowanie/DataMining/zadanie2/salesdata_test_odp_new", i, ".csv", sep = ""))
  print(paste("przetworzono",i,"zestaw",Sys.time()))
}

write.csv(odp, paste("~/Programowanie/DataMining/zadanie2/salesdata_test_odp_new_ost.csv", sep = ""))