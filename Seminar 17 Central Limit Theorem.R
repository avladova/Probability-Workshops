#quest A
1000-sqrt(2.5)*200
1000+sqrt(2.5)*200

#Quest A
#a
295-sqrt(2.5)*63
295+sqrt(2.5)*63

#b
295-sqrt(5)*63
295+sqrt(5)*63

#question B 
#a
sqrt(1/0.16)
9.2-sqrt(2.5)*3.5
9.2+sqrt(2.5)*3.5

#b
#68% of the data-set is ±3.5 of the mean of all normal distribution
9.2-3.5 #lower bound
9.2+3.5 #upper bound

#Quest C
#a
29000-2*3000
29000+2*3000

#b 
#95% of the data-set is ±2S of the mean of all normal distribution
19/20
29000-2*3000
29000+2*3000

m<-function(percent) {sqrt(1/(1-percent/100))}
m(89)

#quest 5
pnorm(2*sqrt(2/3))

#quest 6
1-pnorm(1/sqrt(42))

#quest 7
pnorm(1/sqrt(4))

#quest 8
pnorm(1/sqrt(8))

#quest 9
1-pnorm(1/sqrt(8))
