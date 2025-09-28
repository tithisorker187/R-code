#problem-1
#Cumulative Total method 
Orchards sizes : 150,50,100,200,160,40

size=c(150,50,100,200,160,40)
size
orchard_name=paste0("Orchard_",1:length(size))
orchard_name
names(size)=orchard_name

total_size=sum(size)
total_size
cum_sum=cumsum(size)
cum_sum

#PPSWR : Cumulative Total Method 

pps_cumulative_draw=function(size){
  total_size=sum(size)  
  u=sample.int(total_size,1)
  idx=which(u<= cumsum(size))[1]
  return(idx)
}

n=3
n
draws_cum= replicate(n,pps_cumulative_draw(size))
draws_cum

