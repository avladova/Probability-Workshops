#Jointly Distributed Discrete Random Variables
#question 1
A=matrix(c(0.07,0.07,0.06,0.02,0.09, 0.06,0.07,0.04,0.06,
           0.07,0.14,0.16,0.01,0.01,0.03,0.04),nrow=4,ncol=4)
View(A)
#audit 
sum(A)
#a
colSums(A) #sum of column's values
x<-0:3
crossprod(x,colSums(A)) #mean X - sumproduct in Excel
Varx<-crossprod(x^2,colSums(A))-crossprod(x,colSums(A))^2 #var
#b
rowSums(A) #sum of row's values
y<-0:3
crossprod(y,rowSums(A)) #mean Y
Vary<-crossprod(y^2,rowSums(A))-crossprod(y,rowSums(A))^2 #var

#c
A[,4]/sum(A[,4]) #conditional probability function of Y, given X = 3
crossprod(y,A[,4]/sum(A[,4])) #mean
19/9

#d
Exy<-crossprod(x[1]*A[,1],y)+crossprod(x[2]*A[,2],y)+crossprod(x[3]*A[,3],y)+crossprod(x[4]*A[,4],y)
cov=Exy-crossprod(x,colSums(A))*crossprod(y,rowSums(A))
cor=cov/sqrt(Varx*Vary)

#question 2
A=matrix(c(0.09,0.07,0.03,0.14,0.23, 0.10,0.07,0.16,0.11),nrow=3,ncol=3)
#a
0.09+0.14+0.07+0.23
#or
sum(A[1:2, 1:2])
#or
sum(A[c(1, 2), c(1, 2)])
#or
for (i in 1:2)
  for (j in 1:2) {
    print(A[i,j])
    F=F+A[i,j]
  }
#b
#0.09+0.07+0.03
Pyx=A[,1]/sum(A[,1])
#or
colSums(A)[1]
Pyx=A[,1]/colSums(A)[1]

#c
denom=rowSums(A)
Pxy=A[3,]/denom[3]

#d
x<-0:2
y<-3:5
Exy<-crossprod(x[1]*A[,1],y)+crossprod(x[2]*A[,2],y)+crossprod(x[3]*A[,3],y)
Ex<-crossprod(x,colSums(A))
Ey<-crossprod(y,rowSums(A))
cov=Exy-Ex*Ey

4.64-1.15*3.94
sum(crossprod(A,x*y))

#e independency Pxy=Px*Py
colSums(A)[1]
Pyx=A[,1]/colSums(A)
Pxy=A[1,]/denom[3]
Pxy*Pyx

#question 4
B<-matrix(c(1-0.45-(0.16-0.15), 0.16-0.15, 0.45-0.15, 0.15),byrow=TRUE ,nrow=2)
rownames(B) <- c(0, 1)
colnames(B) <- c(0, 1)
View(B)
#b
B[,2]/colSums(B)[2]
1/16
15/16

#c
x<-0:1
y<-0:1
Exy<-crossprod(x*y[1],B[,1])+crossprod(x*y[2],B[,2])
Ex<-crossprod(x,colSums(B))
Ey<-crossprod(y,rowSums(B))
cov=Exy-Ex*Ey
