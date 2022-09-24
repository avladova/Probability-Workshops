#task 3
factorial(7)
1/5040

#task 4
50*49

#task 5
#a order is matter
n=5
k=3
factorial(n)/factorial(n-k)
#b oder does not matter
factorial(n)/(factorial(k)*factorial(n-k))
#c
1/60

#task 6
n=6
k=3
#a
factorial(n)/factorial(n-k)
#b
1/120
#c order is not important
factorial(n)/(factorial(k)*factorial(n-k))

#task 7
n=8
k=2
factorial(n)/(factorial(k)*factorial(n-k))
#or
choose(n,k)

library(combinat)
permn(x=c("A","B","C")) 
permn(x=2:5) 

#task 8
choose(n=5, k=2)*choose(n=6, k=4)

#task 9
choose(n=4,k=3)/choose(n=4+2,k=3)

#task 10
choose(6,2)*choose(4,2)

#task 11
#P(A)+P(B)-P(A*B)
0.3+0.25-0.2

#task 12
0.3+0.2-0.15

#task 16
#a
choose(n=12,k=3)/choose(12+10,3)
1/7
#b
choose(n=12,k=2)*choose(n=10, k=1)/choose(12+10,3)
3/7

#check the rest combinatorics formulas here 
#https://cran.r-project.org/web/packages/DescTools/vignettes/Combinatorics.pdf