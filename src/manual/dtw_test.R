library(dtw)
idx<-seq(0,6.28,len=100);
query<-sin(idx)+runif(100)/10;

template<-cos(idx)

plot(query)
plot(template)

alignment<-dtw(query,template,keep=TRUE);

plot(alignment,type="threeway")

plot(
  dtw(query,template,keep=TRUE,
      step=rabinerJuangStepPattern(6,"c")),
  type="twoway",offset=-2)

rabinerJuangStepPattern(6,"c")
plot(rabinerJuangStepPattern(6,"c"))

daily <- daily %>% 
  group_by(msoa_code) %>% 
  group_split()

a <- daily[[1]]$cases_rolling_sum_interp
b <- daily[[2]]$cases_rolling_sum_interp

plot(daily[[1]]$cases_rolling_sum_interp, type ='l')
plot(daily[[2]]$cases_rolling_sum_interp, type ='l')

plot(
  dtw(a, b, keep=TRUE,
      step=rabinerJuangStepPattern(6,"c")),
  type="twoway",offset=-2)

m <- matrix(0,ncol=3,nrow=4)
m <- row(m)

# Do this for a matrix of timeseries - each row is a single TS
dist(m,method="DTW")
