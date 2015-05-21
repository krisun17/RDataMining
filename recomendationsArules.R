#data <- dane wczytane z bazy danych (model w pythonie, zmienna data przekazana z pythona)
data <- data.matrix(data)
obow <- as.matrix(data[,1:30])
obier <- as.matrix(data[,31:50])
sem <- as.matrix(data[,51])

#-----------reguly dla przedmiotow obieralnych---------------------

#tworzenie koszykow przedmiotow obieralnych
#install.packages('arules')
library(arules)

colnames(obier) <- NULL
rows <- dim(obier)[1]
l <- list()
for (i in 1:rows)
{
  l[[i]] <- which(obier[i,]>0)
}
koszykObier <- as(l, "transactions")

#szukanie regul dla przedmiotow obieralnych, postaci jesli ktos co wzial to prawdopodobnie wzial rowniez to
#bestObier = sort(itemFrequency(koszykObier)[which(itemFrequency(koszykObier) >= sort(itemFrequency(koszykObier), decreasing = T)[5])], decreasing = TRUE)

#confidence ustawiony, tak by nie bylo za duzo regul
ruleObier = apriori(koszykObier, parameter = list(supp = 0.0002, conf = 0.75, minlen = 2,
                                                  target = "rules", originalSupport = FALSE), appearance = NULL, control = list(sort = -1))

#sortujemy po lifcie
ruleObier = sort(ruleObier, decreasing = T, by = "lift")


#funkcja zwracajaca wektor proponowanych przedmiotow dla danego studenta
#pobiera wektor wybranych przedmiotow obieralnych oraz procent przedmiotow branych pod uwage

getRecomSub <- function(student, pr) {
  Idx = sample(1:length(student), round(pr*length(student)))
  student <- student[Idx]
  n <- length(student)
  rules <- ruleObier
  for (i in 1:n)
  {
    rules = subset(rules, lhs %in% paste(student[i]))
  }
  if (length(rules) > 0) {
    subMatr <- as(rhs(rules), "matrix")
    m <- dim(subMatr)[1]
    recomSub <- vector()
    for (i in 1:m)
    {
      recomSub <- c(recomSub, which(subMatr[i,]>0))
    }
    recomSub <- sort(unique(recomSub))
  }
  else {
    recomSub = c()
  }
  return(recomSub)
}