#Bernoulli Distribution is a special case of Binomial distribution
#3 tosses of a coin to get heads
set.seed(123)
rbinom(n=3,size=1,1/2) #

#Binom law
x<-(-10:10)
plot(x,dbinom(x, size=20, prob=1/6), type='s')

#p(X=k)=dbinom(k,n,p)
#p(X<=k)=pbinom(k,n,p) 
#p(X>=k) =pbinom(݇k,n, ,lower.tail = FALSE)


#question 1
x<-0:1
y<-0:1
fx<-c(0.1, 0.9)
fy<-c(0.9, 0.1)

#E(x^2)+E(y^2)+2*E(x)*E(y)
mean=crossprod(x^2,fx)+crossprod(y^2,fy)+2*crossprod(x,fx)*crossprod(y,fy)
#sumproduct in Excel

#2d variant
x[1]^2*fx[1]+x[2]^2*fx[2]+y[1]^2*fy[1]+y[2]^2*fy[2]+
  2*(x[1]*fx[1]+x[2]*fx[2])*(y[1]*fy[1]+y[2]*fy[2])

# question 2
x<-0:1
f<-c(0.9, 0.1)
sum(f)
mean<-crossprod(x,f) #sumproduct
variance=crossprod(x^2,f)-mean^2
#crossproduct=0*0.9+1*0.1
60*variance+60^2*mean^2

#question 3
x<-c(0,1)
f<-c(0.4, 0.6)

mean=crossprod(4^x,f)
2.8^3


#example
n=5
k=2
p=1/6
#1
factorial(n)/(factorial(k)*factorial(n-k))*p^k*(1-p)^(n-k)
#2
choose(5,2)*p^k*(1-p)^(n-k) #in Excel COMBINE
#or
dbinom(2, size=5, prob=1/6)  #in Excel BINOM.DIST(2, 5, 1/6, 0)


#question 4
n=6
k=0
p=5/100
dbinom(k, n, p)
#or
factorial(n)/(factorial(k)*factorial(n-k))*p^k*(1-p)^(n-k)                
#or
choose(n,k)*p^k*(1-p)^(n-k)  #in excel COMBIN(n,k)

k=1

k=2 
1-(dbinom(x=1, size=6, prob=5/100)+dbinom(x=0, size=6, prob=5/100))
1-pbinom(1,6,5/100)

#5
n=5
k=0 
p=25/100
1-dbinom(0, n, p)

#b
1-pbinom(3, 5, p)
dbinom(3)+dbinom(4),dbinom(5)
1-(dbinom(0)+dbinom(1)+dbinom(2))
x=0:20
sum(dbinom(x))
s=0
for (k=0:2){
  s=dbinom(k)+s
}
1-s

#6
p=0.5
pbinom(3,7,0.5) #k=0:3 

# or
(0.5)^0 * (0.5)^7 * choose(7,0)+
  (0.5)^1 * (0.5)^6 * choose(7,1)+
  (0.5)^2 * (0.5)^5 * choose(7,2)+
  (0.5)^3 * (0.5)^4 * choose(7,3)

sum(dbinom(x=4:7, size=7, prob=0.5))

#question 7
n=110
k=80
p=1-0.27
factorial(n)/(factorial(k)*factorial(n-k))*p^k*(1-p)^(n-k)                
#
#choose * *
#or
dbinom(80, size=110, prob=1-0.27)  # in Excel BINOM.DIST(80, 110, 1-0.27, 0)

#question 8
p=0.7
n=4
dbinom(2, size=n, prob=p)+dbinom(1, size=n, prob=p)+dbinom(0, size=n, prob=p)
#or
pbinom(2, size=n, prob=p)  #cumulative distribution function

#
1-dbinom(3, size=n, prob=p)-dbinom(4, size=n, prob=p)

#question 9
# in Excel BINOM.DIST.RANGE(1200;0.6;699;739)
n=1200
k1=699
k2=739
p=60/100
pbinom(739, size=n, prob=p)-pbinom(699-1, size=n, prob=p)  #cumulative distribution function
sum(dbinom(699:739,1200,0.6))

#question 10
#одинаковое количество удач и неудач (по 3) 
#и поэтому нет разницы вероятность чего cтавить: удачи или не удачи
p=1/6
k=3=n
dnbinom(3,3,1/6) # NEGBINOM.DIST(3,3,5/6,FALSE) or NEGBINOM.DIST(3,3,1/6,FALSE)
dnbinom(3,3,5/6)


#question 11
p=0.8
#Poverbooked=P(X>8)
0.1*choose(9,9)*p^9*(1-p)^0+0.05*(choose(10,9)*p^9*(1-p)^1+0.05*choose(10,10)*p^10*(1-p)^0)
#or
0.1*dbinom(9, size=9, p)+0.05*dbinom(9,10,p)+0.05*dbinom(10,10,p)

#building a plot
1-pbinom(100,110,0.88)

plot(101:115, 1-pbinom(100, 101:115,0.88), type='l',xlab = 'Seats sold', 
     ylab='Probability of >100 passengers')

#12
n=50
p=15/100
n*p
sqrt(7.5*(1-15/100))

#b
250*n*p
250*sqrt(7.5*(1-15/100))

#15
p=1/6
k=4
n=16
choose(16-4, 4-0)/choose(16, 4)
#b
choose(16-4, 2-0)/choose(16, 2)
#c
1-choose(16-1, 4-0)/choose(16, 4)

#home work
#13
n=620
p=0.78
mu=620*0.78
sigma=sqrt(n*p*(1-p))

#b
mu*2
sigma*2

#14
16*5/100
pbinom(1,16,5/100)
pbinom(1,16,15/100)
pbinom(1,16,25/100)

#16
(choose(4,2)*choose(8,1))/choose(12,3)+((choose(4,3)*choose(8,0))/choose(12,3))

#17
(choose(5,1)*choose(5,5))/choose(10,6)+(choose(5,2)*choose(5,4))/choose(10,6)

#18
192*(15/16+(5/2)^2)
#or
192*(4*5/8*3/8+(4*5/8)^2)

#19
27*(10/9+27*(10/3)^2)
#or
27*5*2/3*1/3+(27*5*2/3)^2

#20
p=0.6
4*p*(1-p)+9*3*p*(1-p)+4*3*p*(1-p)
