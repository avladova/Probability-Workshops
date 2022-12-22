#quest 1
x<-c(1,4,5,6,10)
P<-c(0.1,0.05,0.05,0.1,0.7)

plot(x,P, type='l')

mean<-crossprod(x, P) #Excel - sumprod
var<-crossprod(x^2, P)-crossprod(x, P)^2
sqrt(var)

-3.054096+8.15 #left part of the abs
3.054096+8.15 #right part of the abs

#quest2
integrand <-function(x){x^2}
integrate(integrand, lower = -3, upper = 5)$value

C<-3/(5^3-(-3)^3)
integrand <-function(x){x^3}
Ex<-C*integrate(integrand, lower = -3, upper = 5)$value

integrand <-function(x){x^4}
Ex2<-C*integrate(integrand, lower = -3, upper = 5)$value

sqrt(Ex2-Ex^2)
  
#quest 3
24*14  #E(Z)
sqrt(24*14)  #sigma(Z)
24*14+336^2 #E(z^2)


#4
0.73*8.7+0.27/0.3
0.73^2*2.9^2+0.27^2/(0.3^2)
pnorm(8,8.7,2.9)*pexp(1.3,0.3,lower.tail=FALSE)

#quest 5
Varx<-(13-(-5))^2/12
Vary<-80*0.1*0.9
cov<-0.4*sqrt(Varx)*sqrt(Vary)
3*(4*8+cov)+12 #E
16*Varx+9*Vary-2*4*3*sqrt(Varx*Vary)*0.4 #Var
