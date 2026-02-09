library(readxl)
data<-read_xlsx("D:\\IPB\\SEM 4\\Mandiri\\Analisis Regresi\\Data Abreg pertemuan 2.xlsx")
data
summary(data)
colnames(data) <- c("kota", "Y", paste0("X", 1:16))
head(data)
plot(data$X1, data$Y,
     main = "X1 VS Y",
     xlab = "GPM",
     ylab = "NPM")

n <- nrow(data)
x <- data$X1
y <- data$Y

b1 <- (sum(x*y)-sum(x)*sum(y)/n)/(sum(x^2)-(sum(x)^2/n))

b0 <- mean(y)-b1*mean(x)

cat("Koefisien b0:", b0, "\n")
cat("Koefisien b1:", b1, "\n")

r <- (sum(x*y)-sum(x)*sum(y)/n)/
  sqrt((sum(x^2)-(sum(x)^2/n))*(sum(y^2)-(sum(y)^2/n)))
Koef_det <- r^2
Koef_det
