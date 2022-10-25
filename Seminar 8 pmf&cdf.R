rm(list = ls()) #clear all

#task 1
x<- 0:4
f<-c(0.28, 0.36, 0.23, 0.09, 0.04) #0.28+0.36=0.64 0.64+0.23, 0.96=0.87+0.09
sum(f)
plot(f~x,type="s")
       #probability density function pdf

cumsum(f)
plot(cumsum(f)~x,type = "s") #cumulative density function cdf

#task 2
x<- 0:5
f<-c(0.1, 0.14, 0.26, 0.28, 0.15, 0.07) #pmf
sum(f)
plot(f~x,type = "s") #probability density function pdf

cumsum(f)  
plot(cumsum(f)~x,type = "s") #cumulative density function cdf


#c
0.28+0.15+0.07
3<=x<4
4<=x<5

#task 3
x<-47:53
f<-c(0.04, 0.13, 0.21, 0.29, 0.2, 0.1, 0.03)
plot(f~x,type = "s") #probability density function pdf

cmf<-cumsum(f)
plot(cumsum(f)~x,type = "s") #cumulative density function cdf

# c
0.87-0.17
#or
0.21+0.29+0.2
#or
cmf[5]-cmf[2]

#or
df<-data.frame(x, f)
sum(df[(df$x>=49)&(df$x<=51),]$f)
cumsum(df[(df$x>=49)&(df$x<=51),]$f)
#d
1-(f[1]+f[2]+f[3])^2  #^2 coz of 2 packages, not 1
#or
1-cmf[3]^2  
#or
1-sum(df[df$x<50,]$f)^2

#question 4
#a
p=0.1 #P( defect)
p_2=p^2 #P(2 defect parts) p*p
x<-c(0, 1, 2) #how many defect parts found
p<-c(0.9^2, 1-(0.9^2+0.1^2), 0.1^2)
p

p_0=(1-p)^2         
p_1=1-(p_2+p_0)     #2*(1-p)*p

#b
18*17/(20*19)
2*2*18/(20*19)    #(18*2)+(2*18)
1-(18*17/(20*19)+2*2*18/(20*19))

#or
choose(n=20,k=2) #total  C20(2)
choose(n=2,k=0)*choose(n=18,k=2)/choose(n=20,k=2) #x=0
choose(n=2,k=1)*choose(n=18,k=1)/choose(n=20,k=2) #x=1
choose(n=2,k=2)*choose(n=18,k=0)/choose(n=20,k=2) #x=2

#question 6
x <- c(0, 0.2, 0.4, 0.8, 0.9, 1)

diff(x) #an opposite to cumsum

#question 7
0.5*0.25 #0 book
0.5*0.5+0.25*0.25 #1 book
0.25*0.5+0.25^2+0.25*0.5  #2 books
0.25*0.5+0.25^2
0.25^2

#question 8-1
0.3+0.45+0.2+0.35

#question 8-2
solve(2+1, 1-0.4-0.3) #left part, right part
solve(3, 0.3)

#8-3
1/20
1-1/20

#question 9
Pa=0.8
Pb=0.7
Pc=0.6
Pa+Pb+Pc-Pa*Pb-Pb*Pc-Pa*Pc+Pa*Pb*Pc

#question 10
Pa=0.8
Pb=0.7
Pc=0.6
Pab=Pa*Pb
Pab+Pc-Pab*Pc

#question 11 Hometask
Pa=0.8
Pb=0.7
Pc=0.6

P1=Pa+Pb-Pa*Pb
P2=Pb+Pc-Pb*Pc
P=P1*P2

#question 12 Hometask
Pa=0.8
Pb=0.7
Pc=0.6

P1=Pa+Pb-Pa*Pb
P2=1-Pb+Pc-(1-Pb)*Pc
P=P1*P2

#question 5 hometask
x<-0:5
P<-0.4*0.6^(x-1)
plot(P~x,type = "s") #probability density function pdf

cmf<-cumsum(P)
plot(cumsum(P)~x,type = "s") #cumulative density function cdf


plot(p)


#question 14 Hometask
Pa=0.8
Pb=0.7
Pc=0.6
Pb_=1-Pb
P=Pa+Pb+Pc-Pa*Pb-Pa*Pc-Pb*Pc


cdf <- function(x) {
  cumsum(f)
}  
x<-(-2:10)
plot(cdf(x))

distr(x)


integrate(function(x) f~x, 0, 2*pi)  