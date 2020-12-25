B<-100000
B2<-10000

r<-5
p<-vector(mode="double",length=2)


Sim<-replicate(B,{p<-runif(2,-r,r)
             p[1]^2+p[2]^2<=r^2
     })
PI<-4*mean(Sim)
