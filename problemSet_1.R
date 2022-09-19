myName = 'Wang Jingyao'

#1(a)
v1 <- 1:20
v1
#(b)
v2 <- 20:1
v2
#(c)
v3 <-  seq(1,20,by=2)
v3
#(d)
tmp <- c(3,7,11)
v4 <- rep(tmp,10)
v4
#(e)
v5 <- rep(tmp, l=31)
v5

#2
tmp1 <- seq(3, 6, by = 0.1)
x1 <- exp(tmp1)*sin(tmp1)
x1

#3
tmp2 <- 10:100
sum1 <- sum(tmp2^3 + 4*tmp2^2)
sum1

#4(a)
str1 <- paste("label", 1:30)
str1
#(b)
str2 <- paste("function", 1:30, sep = "")
str2

#5
vec <- c(1,'function',NA, seq(1,5,2), 0.125)
vs <- paste(vec, sep = " ", collapse = ",")
vs

#6
tmp3 <- matrix(seq(1,9), nrow = 3, ncol = 3)
m1_ans <- tmp3%*%tmp3%*%tmp3
m1_ans

#7
tmp4 <- matrix(c(12, -12, 12), b = T, nrow = 17, ncol = 3)
m2_ans <- t(tmp4)%*%tmp4
m2_ans

#8
yvec <- c(7, -1, -3, 5, 17)
tmp5 <- matrix(0, nrow = 5, ncol = 5)
amtx <- abs(col(tmp5)-row(tmp5))+1
m3_ans <- solve(amtx)%*%yvec
m3_ans

#9
xv = seq(0,1,by = 0.1)
#(a)
function1 <- function(xv){
  xv^(1:length(xv))
}
func1_ans <- function1(xv)
func1_ans
#(b)
function2 <- function(xv)
{
  n <- length(xv)
  (xv^(1:n))/(1:n)
}
func2_ans <- function2(xv)
func2_ans
#(c)
function3 <- function(x, n)
{
  1 + sum((x^(1:n))/(1:n))
}
func3_ans <- function3(xv,length(xv))
func3_ans

#10
cel_to_far <- function(temp) {
  temp <- (9/5) * temp + 32
  return(temp)
}
cel_to_far(27.5)

far_to_cel <- function(temp) {
  temp <- (5/9) * (temp - 32)
  return(temp)
}
far_to_cel(78.6)

#11
odd_function <- function(odd){
  odd[odd %% 2 == 1]
}

odd_ans <- odd_function(1:2000)
odd_ans

#12
sum_fun <- function (x)
{
  tmp_fun <- function (r){
    sum(((1:r)^0.5)/(11+3.5*r^1.2))
  }
  sum(sapply(x, FUN = tmp_fun))
}
sum_ans <- sum_fun(10)
sum_ans

#13
modNumber <- function(x,y){
  n <- x%%y
  if (n == 0)
    return (x)
  else
    return(x+y-n)
}
modNumber(50,16)
modNumber(64,16)

#14
numberOfWheels <- function(wheels){
  switch(wheels,
         unicycle=1,
         motorcycle=2,
         bike=2,
         tricycle=3,
         car=4,
         truck=4,
         )
}

#15
myFactorial <- function(x) {
  if (x == 0)     
    return (1)
  else 
    return (x * myFactorial (x-1))
}
myFactorial(4)

#16
myCustomFactorial <- function(x,y){
  if (y == x)
    return (y)
  else
    return (y * myCustomFactorial (x, y-1))
}
myCustomFactorial(3,5)
myCustomFactorial(5,5)

#17
customRiverMean <- function(x){
  tmp6 <- rivers < x
  sum(rivers[tmp6]/sum(tmp6))
}
customRiverMean(400)

#18
for (i in ToothGrowth$len)
  longTeeth <- ToothGrowth[ToothGrowth$len >= 15,]$len
print(longTeeth)

#19
car_mean = as.list(sapply(mtcars, mean))
averageHorsePower = car_mean$hp
averageWeight = car_mean$wt
averageHorsePower
averageWeight
