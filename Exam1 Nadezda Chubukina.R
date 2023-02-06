# Task1 
# a
E1 <- (-3)*0.25+(-1)*0.1+1*0.1+4*0.4
E1
# b
E2<- E1^2
E3<-(-3)^2*0.25+(-1)^2*0.1+1^2*0.1+4^2*0.4
sigma1 <- sqrt(E3-E2)
sigma1
# c
# P(|x-0.85|)<=2.850877
# -2.850877<=P(x-0.85)<=2.850877
# -2.850877+0.85<=P(x)<=2.850877+0.85
lower<--2.850877+0.85
lower
upper<-2.850877+0.85
upper

# Task2
#a
# F(b)-F(a), b=4, a=-1
answer2222<-(4^2+5*4+6-(-1)^2-5*(-1)-6)/90
answer2222
#b
# pdf = (2*x+5)/90
Tonya<-function(x) {(2*x+5)*x/90}
answer5555<-integrate(Tonya, lower=-2, upper=7)$value
answer5555

Nadya<-function(x) {(2*x+5)*(x)^2/90}
answer6666<-integrate(Nadya, lower=-2, upper=7)$value
answer6666

sigma1000<-sqrt(answer6666-answer5555^2)
sigma1000

# Task3
# E(x1+...+x7-2) = 7E(x)-2
E9999 <- 7*7-2
E9999 
# sigma(x1+...+x7-2) = sqrt(7*var(x))
lyambda9999<-1/7
var9999<-1/lyambda9999^2
sigma9999<-sqrt(7*var9999)
sigma9999
E100000<- E9999^2+sigma9999^2
E100000

# Task4
# Z=0.83X+0.17Y
# x~norm(2.1, 0.9)
# y~unif[-1,29]
# E(z) = E(0.83X+0.17Y) = 0.83*e(x)+0.17*e(y)
ey<-(-1+29)/2
ez<-0.83*2.1+0.17*ey
ez
#var(z)=var(0.83X+0.17Y) = 0.83^2*var(x)+0.17^2*var(y)
vary<-(29+1)^2/12
varz<-0.83^2*0.9^2+0.17^2*vary
varz
# P(x<=5, y>6) = P(x<=5)*(1-P(y<=6))
answer_p<-pnorm(5, 2.1, 0.9)*(1-punif(6, -1, 29))
answer_p

# Task5
# x~norm(10.2, sqrt(2.9))
# y~geom (e(x) = 5.6 ) 
corr<-0.1
# cov = corr*sigmax*sigmay
prob<-1/5.6
sigma_x <- sqrt((1-prob)/(prob)^2)
covv<-corr*2.9*sigma_x
covv
# E(3XY+11) = 3(e(x)*e(y)+covv)+11
answer55555<-3*(10.2*5.6+covv)+11
answer55555

# Var(4X+2Y+112) = 16*var(x)+4*var(y)+2*4*2*covv
answer66666<- 16*2.9+4*(sigma_x)^2+2*4*2*covv
answer66666