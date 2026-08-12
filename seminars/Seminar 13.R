#Question 1
x<-1:20
plot(x,1/x^3,type='l')

integrand <- function(x) {1/x^3} 
integrate(integrand, lower = 2, upper = 5)
-1/2*(1/25-1/4)

#Question 2
1/2*0.5^2
1/2*(1.5-1)+1/2*(1-0.5^2)
1/2*(2-1)

#Question 3
sympy("integrate(2-2*x*x))") 

F_x=2*c*(x-x^3/3)
c=1/(2-2/3)

x<-0:1
plot(x,F_x,type='l')

integrand <- function(x) {c*(2-2*x*x)} 
integrate(integrand, lower = 0.5, upper = 0.7)

#E(x)
integrand <- function(x) {c*(2-2*x*x)*x} 
integrate(integrand, lower = 0, upper = 1)
3/8

#Question 4
#E(x)
integrand <-function(x){0.5*x}
integrate(integrand, lower = -1, upper = 1)

x <- seq(-4, 4, length=100)
y <- dunif(x, min = -1, max = 1)
plot(x, y, type = 'l')

#E[x^(2\7)]
integrand <- function(x) {x^(9/7)} 
integrate(integrand, lower = -1, upper = 1)   #error

#hometask
#question 5

Exy=(2+3)/2*(4+6)/2
Varx=1/(3-2)*(3^3/3-2^3/3)-(2+3)^2/2
Vary=1/(6-4)*(6^3/3-4^3/3)-(6+4)^2/2
Varx*Vary

#1/12*2/12+(5/2)^2*(2/12)^2+(10/2)^2*1/12
151/36


#question 6
intZ<-function(x){dnorm(x)}
#or
intZ<-function(x){1/sqrt(2*pi*1)*exp(-x^2/2)}

integrate(intZ, -Inf, 1.2)

1-pnorm(1.2, lower.tail=FALSE)
#or
pnorm(1.2)

integrate(intZ, 1.33, Inf)
#or
1-pnorm(1.33)
#or
pnorm(1.33, lower.tail = FALSE)

integrate(intZ, -Inf, -1.7)

integrate(intZ, -1, Inf)

#Question 7
pnorm(1.33)-pnorm(1.2)
pnorm(1.2)-pnorm(-1.7)
pnorm(-1)-pnorm(-1.7)

#Question 8
qnorm(0.7)
qnorm(0.25)
qnorm(0.2,lower.tail = FALSE)
qnorm(0.6,lower.tail = FALSE)

#Question 9
intZ<-function(x){dnorm(x, mean=12, sd=sqrt(9))}
integrate(intZ, 6.9, 16.5)
#or
pnorm(16.5, mean=12, sd=3)-pnorm(6.9, 12, 3)
pnorm((16.5-12)/3, 0, 1)-pnorm((6.9-12)/3, 0,1)

#Question 10
pnorm(19.4, mean=15, sd=4, lower.tail = FALSE)
#or
1-pnorm(19.4, mean=15, sd=sqrt(16))

#Question 11
pnorm(21, mean=18, sd=sqrt(25))

#Question 12
qnorm(0.24196, mean=0, sd=1)
sigma<-(-0.7-37.7)/30
var<-sigma^2

#Question 15
#a
integrand <-function(x){dexp(x, rate=1/10)}
integrate(integrand, lower = 0, upper = 20)
#or
pexp(20,rate = 1/10 )

x<-0:20
plot(x,dexp(x,1/10),type='l')

#b
integrate(integrand, lower = 5, upper = Inf)

pexp(5, 1/10, lower.tail = FALSE)
#c
integrate(integrand, lower = 10, upper = 15)
pexp(15,1/10)-pexp(10,1/10)

#question 16
integrand <-function(x){dexp(x, rate=log(2,base=exp(1))/4)} #натуральный логарифм
integrate(integrand, lower = 4, upper = 12)
3/8
#2d variant
rate=log(2,base=2.72)
exp(-rate)-exp(-rate*3)

x<-0:10
plot(x,dexp(x,rate/4),type='l')


#question 17
integrand <-function(x){dexp(x, rate=1/15)} 
integrate(integrand, lower = 18, upper = Inf)
exp(-Inf)
1-exp(-18/15)


1-(9/15+1/6)
