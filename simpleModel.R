tt=10
n=100
v=2
j=5

distrib=c()

for(i  in 1:tt)distrib=rbind(distrib,rnorm(n,i/j,v))

write.csv
