,rm(list=ls())
#=============Q2===========================#
              #==Q2(a)==#

set.seed(134)
x<-matrix(0,n,1)
n=1000              
u<-runif(n,0,1)
a=.5
b=.1
c=.4
x[1]<-ifelse(u[1]<a,1,ifelse(u[1]<(a+b),2,3))
for(i in 1:(n-1)){
if(x[1]==1){
a=.4
b=.5
c=.1
}
if(x[1]==2){
a=.3
b=.4
c=.3
}
if(x[1]==3){
a=.3
b=.1
c=.6
}
x[i+1]<-ifelse(u[i+1]<a,1,ifelse(u[i+1]<(a+b),2,3))
}
head(x)
            #============Q2(b)=========#

table(x)
nn<-length(x)
TM<-matrix(0,3,3)
for(i in 1:(nn-1)){
if(x[i]==1 & x[i+1]==1){TM[1,1]=TM[1,1]+1}
if(x[i]==1 & x[i+1]==2){TM[1,2]=TM[1,2]+1}
if(x[i]==1 & x[i+1]==3){TM[1,3]=TM[1,3]+1}

if(x[i]==2 & x[i+1]==1){TM[2,1]=TM[2,1]+1}
if(x[i]==2 & x[i+1]==2){TM[2,2]=TM[2,2]+1}
if(x[i]==2 & x[i+1]==3){TM[2,3]=TM[2,3]+1}

if(x[i]==3 & x[i+1]==1){TM[3,1]=TM[3,1]+1}
if(x[i]==3 & x[i+1]==2){TM[3,2]=TM[3,2]+1}
if(x[i]==3 & x[i+1]==3){TM[3,3]=TM[3,3]+1}

}
TM
TPM<-matrix(0,3,3)
k<-NULL
for(i in 1:3){
k[i]=sum(TM[i,])
TPM[i,]=TM[i,]/k[i]
 }
k
TPM

               #=====2Q(c)====#
MC<-matrix(c(.4,.5,.1,.3,.4,.3,.3,.1,.6),3,3,1)
TM
ni<-k
ss<-matrix(0,3,3)
for(i in 1:3){
 for(j in 1:3){
  ss[i,j]<-TM[i,j]*log((TM[i,j])/(ni[i]*MC[i,j]))
 }
  }
2*sum(ss)
qchisq(0.95,6)

#install.package("markovchain")
library(markovchain)
x
mc<-matrix(c(.4,.5,.1,.3,.4,.3,.3,.1,.6),3,3,1)
rownames(mc)<-colnames(mc)<-1:3
TMC<-as(mc,"markovchain")
verifyEmpiricalToTheoretical(data=c(x),object=TMC)

#========================Q3============================#
rm(list=ls())
     #=================Q3(a)=================#
set.seed(12345)
nnn=10000
lamda=2
x3=matrix(0,nnn,1)
u=runif(nnn,0,1)
for(k in 1:nnn){
i=ifelse(k==1,1,x3[(k-1)])
j=round(runif(1,(i-1),(i+1)),0)
alpha<-ifelse(j==(i+1),min(1,lamda/(i+1)),ifelse(j==(i-1),min(1,i/lamda),0))
x3[k]<-ifelse(u[k]<=alpha,j,i)
}
x3
table(x3)
#=======================Q3(b)==================#
xx<-ifelse(x3<=1,1,ifelse(x3<=3,2,ifelse(x3<=5,3,4)))
xx
n3<-length(xx)
TM3<-matrix(0,4,4)
for(i in 1:(n3-1)){
if(xx[i]==1 & xx[i+1]==1){TM3[1,1]=TM3[1,1]+1}
if(xx[i]==1 & xx[i+1]==2){TM3[1,2]=TM3[1,2]+1}
if(xx[i]==1 & xx[i+1]==3){TM3[1,3]=TM3[1,3]+1}
if(xx[i]==1 & xx[i+1]==4){TM3[1,4]=TM3[1,4]+1}

if(xx[i]==2 & xx[i+1]==1){TM3[2,1]=TM3[2,1]+1}
if(xx[i]==2 & xx[i+1]==2){TM3[2,2]=TM3[2,2]+1}
if(xx[i]==2 & xx[i+1]==3){TM3[2,3]=TM3[2,3]+1}
if(xx[i]==2 & xx[i+1]==4){TM3[2,4]=TM3[2,4]+1}

if(xx[i]==3 & xx[i+1]==1){TM3[3,1]=TM3[3,1]+1}
if(xx[i]==3 & xx[i+1]==2){TM3[3,2]=TM3[3,2]+1}
if(xx[i]==3 & xx[i+1]==3){TM3[3,3]=TM3[3,3]+1}
if(xx[i]==3 & xx[i+1]==4){TM3[3,4]=TM3[3,4]+1}

if(xx[i]==4 & xx[i+1]==1){TM3[4,1]=TM3[4,1]+1}
if(xx[i]==4 & xx[i+1]==2){TM3[4,2]=TM3[4,2]+1}
if(xx[i]==4 & xx[i+1]==3){TM3[4,3]=TM3[4,3]+1}
if(xx[i]==4 & xx[i+1]==4){TM3[4,4]=TM3[4,4]+1}
 }
TM3
kk<-NULL
for(i in 1:4){
kk[i]<-sum(TM3[i,])
TM3[i,]<-TM3[i,]/kk[i]
}
TM3
#=============================Q3(c)============================#
TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3%*%TM3

#===================================Q4===========================#
A<-matrix(c(93,3,4,7,90,3,3,2,95),3,3,1)
B<-matrix(c(96,5,2,2,90,3,4,5,93),3,3,1)
C<-matrix(c(85,15,2,1,98,1,3,14,81),3,3,1)
D<-matrix(c(85,2,2,8,108,11,2,2,80),3,3,1)
E<-matrix(c(91,2,2,4,103,5,3,1,89),3,3,1)
RA<-rowSums(A)
RB<-rowSums(B)
RC<-rowSums(C)
RD<-rowSums(D)
RE<-rowSums(E)
PM<-A+B+C+D+E
h<-NULL
TPM4<-matrix(0,3,3)
for(i in 1:3){
h[i]<-sum(PM[i,])
TPM4[i,]<-PM[i,]/h[i]
 }
TPM4
DA<-matrix(0,3,3)
DB<-matrix(0,3,3)
DC<-matrix(0,3,3)
DD<-matrix(0,3,3)
DE<-matrix(0,3,3)
for(i in 1:3){
 for(j in 1:3){
DA[i,j]<-round(A[i,j]*log(A[i,j]/(RA[i]*TPM4[i,j])),2)
DB[i,j]<-round(B[i,j]*log(B[i,j]/(RB[i]*TPM4[i,j])),2)
DC[i,j]<-round(C[i,j]*log(C[i,j]/(RC[i]*TPM4[i,j])),2)
DD[i,j]<-round(D[i,j]*log(D[i,j]/(RD[i]*TPM4[i,j])),2)
DE[i,j]<-round(E[i,j]*log(E[i,j]/(RE[i]*TPM4[i,j])),2) 

 }
  }
2*sum(DA+DB+DC+DD+DE)
qchisq(.95,24)
########################order############

dd<-matrix(c(416,45,0,44,388,18,0,18,70),3,3,1)
R<-rowSums(dd)
C<-colSums(dd)
T<-sum(R)
ss<-matrix(0,3,3)
for(i in 1:3){
  for( j in 1:3){
    ss[i,j]<-(dd[i,j]-((R[i]*C[j])/T))^2/((R[i]*C[j])/T)
  }
}
sum(ss)


   





































