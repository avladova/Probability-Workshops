# task 12
df <- data.frame(x = c(3,4,5,6,7), y=c(0.08,0.24,0.41,0.2,0.07))

#a probability of A
sum(df[df$x>4,]$y)
#c
1-sum((df[df$x>4,]$y))
#d probability of B
sum(df[df$x<6,]$y)
#f
sum(df[(df$x>4)&(df$x<6),]$y)
#h
sum(df[(df$x>4)|(df$x<6),]$y)

#task 13
df <- data.frame(x = c(-Inf,-10,0,10,20), y=c(0.04,0.14,0.28,0.33,0.21))
#a probability of A
sum(df[df$x>=10,]$y)
#b probability of B
sum(df[df$x<0,]$y)
#d complement
1-sum(df[df$x>=10,]$y)
#f intersection
sum(df[(df$x>=10)&(df$x<0),]$y)
#h union
sum(df[(df$x>=10)|(df$x<0),]$y)

#task 14
Pa=4/8
Pb=2/8
#c
2/8
#d
4/8

#task 15
df <- data.frame(x = c(0,1,4,7,10,12), y=c(0.14,0.39,0.23,0.15,0.06,0.03))
#a probability of A
sum(df[df$x>=1,]$y)
#b probability of B
sum(df[df$x<10,]$y)
#c
1-sum((df[df$x>=1,]$y))
#d union
sum(df[(df$x>=1)|(df$x<10),]$y)
#e intersection
sum(df[(df$x>=1)&(df$x<10),]$y)

