rm(list=ls(all=TRUE))
install.packages("readxl")
library("readxl")


data <- c(9.37, 13.04, 11.69, 8.21, 10.41, 13.15, 11.51, 13.21, 7.75)
qqnorm(data)
qqline(data)
shapiro.test(data)
euroemp<-read_excel("/home/kaliw/GITHUB/STATISTICS/EXAMEN_FINAL/study/Egyptian_skulls.xlsx")

