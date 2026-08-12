#question 1
curve(0.89*dunif(x, -22,8)+0.11*dexp(x, rate = 1.2), from=-22, to=8, col='blue')

#a Ez
0.89*(-22+8)/2+0.11*1/1.2
#or
integrand <-function(x){x*dunif(x, min = -22, max = 8)}
Ex<-integrate(integrand, lower = -22, upper = 8)$value

integrand <-function(y){y*dexp(y, 1.2)}
Ey<-integrate(integrand, lower = -22, upper = 8)$value

Ez<-0.89*Ex+0.11*Ey

#b Var(z)
0.89^2*(8+22)^2/12+0.11^2/(1.2^2)

#or
integrand <-function(x){x*x*dunif(x, min = -22, max = 8)}
Ex2<-integrate(integrand, lower = -22, upper = 8)$value

integrand <-function(y){y*y*dexp(y, 1.2)}
Ey2<-integrate(integrand, lower = -22, upper = 8)$value

integrand <-function(y){y*dunif(y, min = -22, max = 8)}
Ey<-integrate(integrand, lower = -22, upper = 8)$value

Ez2<-0.89^2*Ex2+2*0.89*0.11*Ex*Ey+0.11^2*Ey2
VarZ=Ez2-Ez^2

#P(x<2, y>=1.4)
(2-(-22))/(8-(-22))*(1-(1-exp(-1.2*1.4)))
#or
punif(2,-22,8)*(1-pexp(1.4, rate=1.2))

#Question 2
cov=0.4*sqrt(3.4*4.9)

Exy<-cov+5.9*4.9
3*Exy+2

Var<-4^2*3.4+2^2*4.9+2*4*2*1.63

#or
#16*Ex2+16*Exy+4*Ey2-(4*Ex+2*Ey)^2

#integrand <-function(x){x*x*dnorm(x, mean=5.9, sd=sqrt(3.4))}
#Ex2<-integrate(integrand, lower = -Inf, upper = Inf)$value

#integrand <-function(y){y*y*dpois(y, lambda = 4.9)}
#Ey2<-integrate(integrand, lower = -Inf, upper = Inf)$value

#integrand <-function(x){x*dnorm(x, mean=5.9, sd=sqrt(3.4))}
#Ex<-integrate(integrand, lower = -Inf, upper = Inf)$value

#integrand <-function(y){y*dpois(y, lambda = 4.9)}
#Ey<-integrate(integrand, lower = -Inf, upper = Inf)$value

#theory
#Question 1
x<-seq(from=0, to=1, by=0.1)
plot(x,1-x,type='l')

#.....
integrand <-function(x){1/4*x-3/8*x*x+1/8}
1-integrate(integrand, lower = 0, upper = 1)$value
7/8

#Question 2 
x=0:5
plot(x,(20-4*x)/5,type='l')
1/16.6
3/50

#E(x)
integrand <-function(y){y*dpois(y, lambda = 4.9)}
Ey<-integrate(integrand, lower = -Inf, upper = Inf)$value

#Question 3
-(exp(-Inf)-exp(0))
exp(-Inf)-exp(0)
exp(-2)-exp(0)


#Question 4
integrand<-function(x) {exp(-2*x)}
C<-1/integrate(integrand, 0, Inf)$value

#b P(x<1)
integrand<-function(x) {2*exp(-2*x)}
integrate(integrand, 0,1)$value

#Question 5
x<-seq(from=0, to=1, by=0.1)
plot(x,52-52*x,type='l')

function(x) {1/(0.5*1*52)}

#Question 6
x<-seq(from=0, to=3, by=0.1)
plot(x,(12-4*x)/3,type='l')

#a Fx(1) ?
1/(0.5*4*3)
Fx<-function(x) {1/6*(4-4/3*x)}
Fx(1)
5/9

#b Ex
integrand<-function(x) {x*1/6*(12-4*x)/3}
Ex<-integrate(integrand, 0, 3)$value



library(pracma)
fun <- function(x, y) {2*exp(-2*x-y)}
integral2(fun, 0, Inf, 0, 1, reltol = 1e-10)

#Question 8


#question 9

#Question 10
q1<-expression((3*x^3*y+2*x^2*y^2)/5) #dx
D(q1,'x')
q2<-expression((3 * (3 * x^2) * y + 2 * (2 * x) #dy * y^2)/5)
D(q2,'y')

fxy<-function(x,y) {(3 * (3 * x^2) + 2 * (2 * x) * (2 * y))/5}
fxy(0.5,0.5)
17/20

#Bivariate normal distribution - an example
S<-matrix(c(9,12,12,25), nrow=2, byrow=TRUE)

#first step - to find the covariance matrix
Si<-solve(S) #to get an inverse matrix
#or
inv(t(S)) 
#check
View(matrix(c(25/81,-4/27,-4/27,1/9), nrow=2, byrow=TRUE))

Sdx=sqrt(Si[1])
Sdy=sqrt(Si[4])
ro=Si[2]/(Sdx*Sdy)

#second step - to find max of x and y which are Ex and Ey
q<-expression(9*x^2+24*x*y+25*y^2-6*x-26*y+10)
D(q,'x')
D(q,'y')
A <- rbind(c(9*2, 24),
           c(24, 25*2))
B<-c(6, 26)
solve(A, B)

#Question 11
P<-function (x, y) {(-x^2-3*x*y-5/2*y^2-10*x-16*y-26)/(-0.5)}

S<-matrix(c(2,3,3,5), nrow=2, byrow=TRUE)

#first step - to find the covariance matrix
Si<-solve(S) #to get an inverse matrix
#or
inv(t(S)) 

Sdx=sqrt(Si[1])
Sdy=sqrt(Si[4])
ro=Si[2]/(Sdx*Sdy)

#second step - to find max of x and y which are Ex and Ey
q<-expression(2*x^2+2*3*x*y+5*y^2+2*10*x+2*16*y+2*26)
D(q,'x')
D(q,'y')
A <- rbind(c(2*2, 2*3),
           c(2*3, 5*2))
B<-c(-20, -2*16)
solve(A, B)

