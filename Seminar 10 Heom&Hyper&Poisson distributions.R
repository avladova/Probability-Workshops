plot(x=0:40, dhyper(x=0:40, m = 50, n = 20, k = 30), type='s')

plot(x=-40:40, dgeom(x=-40:40, prob=0.5), type='s', col='blue')
plot(x=-40:40, dnbinom(x=-40:40, 100, prob=0.5), type='s', col='green')
plot(x=0:40, dpois(x=0:40, lambda=0.5), type='l', col='green')

#The hypergeometric distribution is used for sampling without replacement.

#distributions
#https://colab.research.google.com/drive/10wECrzOJTdB6Wz0MVvURS84PMeEQEDlY


#task 1
dhyper(8/2,8,8, 8) #8/2 - women, last 4 - how many ppl are in a committee

#task 2
phyper(2, 5,5, 6)

#task 3
0.93^19*0.07^1
dnbinom(19,1,1-93/100)
dgeom(19,1-0.93)

#b
1/(1-0.93)

#task 4
#a
dgeom (x= 4, prob = .03)
#or
dnbinom(4,1,0.03)

#b
pgeom(q=4, prob=0.03)
#or
pnbinom(4,1,0.03)

#task 5
#a
1/0.2
#b
pnbinom(6,1,0.2)
#or
pgeom(q=0:8, prob=0.2)
#or
k=0
while (pnbinom(k,1,0.2)<=0.75) {   
  k=k+1
}
cat('k=', k, " pbinom=",  pnbinom(k,1,0.2))
#or
pnbinom(0:10,1,0.2)

#task 6
dnbinom(3,1,0.25)
#or
dgeom(x=3, prob=0.25)

#task 7
#a
0.7^1*0.3^1
#or
dgeom(1,0.3)

#b
1/0.7 #mean
(1-0.7)/0.7^2 #variance
sqrt((1-0.7)/0.7^2) #std

#task 8
ppois(2,3)

#task 9
#a
ppois(1,2.6)
#b
1-ppois(3,2.6)

#task 10
ppois(1, 3.2)

#b
1-ppois(4, 3.2)

#task 11
ppois(2, 100*0.055) 
#or
pbinom(2,100,0.055) 

#task 12
ppois(3, 250*0.01)
#or
pbinom(3,250,0.01) 

#HOME TASKS
#task 13
mean=4
p=1/mean
var=(1-p)/p^2
10*(var+mean^2)

#task 14
mean=6
p=1/mean
var=(1-p)/p^2
8*var+8^2*mean^2

#task 16
mean=7
p=1/mean
var=(1-p)/p^2
5*var+5^2*mean^2
