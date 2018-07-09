### Central Limit Theorem 

library(ggplot2)

clt=function(samples , sample_size)
{
temp=numeric()
for(i in 1:samples) 
{
  set.seed(i)
  a1=runif(n=sample_size)
  temp=c(temp,mean(a1))
}
return(temp)
}

output=clt(samples=20,sample_size=100000)
print(output)
#####################################################