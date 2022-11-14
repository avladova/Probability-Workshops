library(rSymPy) #symbolic calculations
.jinit()

#Question 1
sympy("var('x')")
sympy("integrate(1/(x*x*x))")              # indefinite integral

c<-32

x<-4:20
plot(x,c/x^3,type='l')

integrand <- function(x) {c/x^3} 
integrate(integrand, lower = 4, upper = 5)


#Question 2
sympy("integrate(x*x/18)") 

a<-27^(1/3)

x<- -a:a
plot(x,x*x/18,type='l')

integrand <- function(x) {x*x/18} 
integrate(integrand, lower = -3/2, upper = 3/2)
1/8

#Question 3
x9<-function(x) {x^9/9}
C=1/(x9(5)-x9(-4))

C/9*(3^9-(-3)^9)
#or
integrand <- function(x) {C*x^8} 
integrate(integrand, lower = -3, upper = 3)

#E(x)
Ex <- function(x) {C*x^8*x} 
integrate(Ex, lower = -4, upper = 5)

#E(x^2)
Ex2 <- function(x) {C*x^8*x*x} 
integrate(Ex2, lower = -4, upper = 5)

#Var(x)=E(x^2)-E^2(x)
integrate(Ex2, lower = -4, upper = 5)$value-integrate(Ex, lower = -4, upper = 5)$value^2

#Question 4
Fx <- function(x) {(x^2-3*x)/40} 
Fx(7)-Fx(4)

#to solve quadratic equation with discriminant
quad <- function(a, b, c)
{
  a <- as.complex(a)
  answer <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
              (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
  if(all(Im(answer) == 0)) answer <- Re(answer)
  if(answer[1] == answer[2]) return(answer[1])
  answer
}
quad(a = 1, b = -3, c = -10.4)

#or
disc<-(-3)^2-4*(-10.4)  #Discriminant
(3-sqrt(disc))/2   #x1
(3+sqrt(disc))/2   #x2

#or
polyroot(c(-10.4, -3, 1))

#E(x)
der<-D(expression((x^2-3*x)/40), "x")

fx<-function(x) {(2*x-3)/40*x}
integrate(fx, lower = 3, upper = 8)

#Uniform distribution - an example
x <- seq(-4, 4, length=100)
y <- dunif(x, min = -3, max = 3)
plot(x, y, type = 'l')

#Question 5
#a
x <- seq(-4, 4, length=100)
y<-dunif(x,0,1)
plot(x, y, type = 'l')
#b
y<-punif(x,0,1)
plot(x, y, type = 'l')
#c
punif(0.25,0,1)
#or
0.25/1
#or
P <- function(x) {dunif(x, min = 0, max = 1)} 
integrate(P, lower = 0, upper = 0.25)

#d
1-punif(0.75,0,1)
#or
punif(0.75, 0, 1, lower.tail = FALSE)
#or
0.75/1
#or
P <- function(x) {dunif(x, min = 0, max = 1)} 
integrate(P, lower = 0.75, upper = 1)
#e
integrate(P, lower = 0.2, upper = 0.8)

#Question 6
#same as in quest 5

#Question 7
Ex <- function(x) {dunif(x, min = -14, max = 10)*x} 
integrate(Ex, lower = -14, upper = 10)
#or
(-14+10)/2
#E(x^2)
Ex2 <- function(x) {dunif(x, min = -14, max = 10)*x*x} 
integrate(Ex2, lower = -14, upper = 10)

#Var(x)=E(x^2)-E^2(x)
integrate(Ex2, lower = -14, upper = 10)$value-integrate(Ex, lower = -14, upper = 10)$value^2
#or
(10+14)^2/12

Fx<-function(x) {dunif(x, -14, 10)}
integrate(Fx, lower = 0, upper = Inf)
5/12
#or
1-integrate(Fx, lower = -Inf, upper = 0)$value

integrate(Fx, lower = -1, upper = 5)

#Question 8 (6)
0.6-0.5

#Question 9 (7)
#a
0.6-0.4


#Question 10
mean<-10000+1.5*30000

std<-1.5*8000

#Question 12
mean<-60+0.2*700
std<-0.2*130

#Question 13
yields=c(200000/2, 200000/2)
mean_profit=yields[1]+yields[1]*0.1+yields[2]+yields[2]*0.18
stdB=sqrt(yields[2]^2*0.06^2)
#or
100000*(1+0.1)+100000*(1+0.18)
#or
200000*(1+1/2*0.1+1/2*0.18)
200000*sqrt(0.5^2*0.06^2)

#Question 14
mean<-50000+72000+40000
std<-sqrt(10000^2+12000^2+9000^2)

#Question 15
mean<-20000+25000+15000
std<-sqrt(2000^2+5000^2+4000^2)

#Question 2 old
sympy("integrate(1/(1+81*x*x))") 

1/9*(atan(9*+Inf)-atan(9*0))

c=1/0.1745329
18/pi

x<-0:10
plot(x,c/(1+81*x*x),type='l')

integrand <- function(x) {c/(1+81*x*x)} 
integrate(integrand, lower = 1/9, upper = +Inf)

1/9*(atan(9*+Inf)-atan(9*0))
c=1/0.1745329
18/pi
