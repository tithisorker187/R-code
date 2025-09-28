#Problem-2 
#holding : 50,30,45,25,40,26,24,35,28,27

size=c(50,30,45,25,40,26,24,35,28,27)
size
holding_name=paste0("Holding_",1:length(size))
holding_name
names(size)=holding_name

total_size=sum(size)
total_size
cum_sum=cumsum(size)
cum_sum

pps_cumulative_draw=function(size){
     
     total_size=sum(size)
     u=sample.int(total_size,1)
     idx=which(u<=cumsum(size))[1]
     return(idx)
} 

n=4
n
draws_cum=replicate(n,pps_cumulative_draw(size))
draws_cum












