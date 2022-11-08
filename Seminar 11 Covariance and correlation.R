#question 1
p=1/4
Dx=(1-1/4)/(1/4)^2
Dx+Dx-2*sqrt(Dx*Dx)*0.5

#question 2
40+70+2*0.8*sqrt(40*70)+(40+70)^2
#or
Exy2<-function(Ex, Ey, Dx, Dy, ro) {Dx+Dy+2*ro*sqrt(Dx*Dy)+(Ex+Ey)^2}
Exy2(40,70,40,70,0.8)

#question 3
Dx=0.5-0.5^2
2^2*Dx+3^2*Dx-2*2*3*sqrt(Dx*Dx)*0.7

#question 4
0.25*sqrt(8*32)+2*3

#question 5
D_S=3^2+3^2+2^2+1+1
D_T=9^2+3^2+2^2+2^2+1+1
D_ST=12^2+6^2+4^2+2^2+2^2+1
cov_=(D_ST-D_S-D_T)/2
ro=cov_/sqrt(D_S*D_T)

#question 6
Exy2(8, 8, 80, 80, 0.1) #check the function in question 2

#question 7
Rxy=(68-52)/(2*sqrt(100))
Rxz=(64-50.2)/(2*sqrt(2*50))
Ryz=(196-100)/(2*sqrt(50*50))
row1<-c(1, Rxy, Rxz)
row2<-c(Rxy, 1, Ryz)
row3<-c(Rxz, Ryz, 1)

matrix(data = rbind(row1, row2, row3), nrow = 3, ncol = 3)

#question 8
fr <- function(x) {   
  x1 <- x[1]
  x2 <- x[2]
  1 * x1^2 + 2* x2^2 + 2*(-0.5)*1*2* x1 * x2
}  
optim(c(7,14), fr)