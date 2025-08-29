##Setting the directory
setwd("C:\\Users\\it24102713\\Desktop\\Lab 5")


#1
data<-read.table("Data.txt",header=TRUE,sep = ",")
fix(data)

attach(data)

names(data)<-c("X1","X2")
attach(data)
hist(X2,main= "Histogram for number of shareholders")

#2
histogram<-hist(X2,main="Histogram of number of shareholders",brasks = seq(130, 270,length = 8),right = FALSE)

?hist

#3
breaks <- round(histogram$breaks)
freq <- histogram$counts
mids <- histogram$mids

classes <- c()

for(i in 1:length(breaks)-1){
  classes[i] <- paste0("[",breaks[i], ",", breaks[i+1],")")
}

#4
lines(mids, freq)
plot(mids, freq, type = 'l',main = "Frequency polygon for shareholders",xlab = "Shareholders",ylab = "Frequency",ylim = c(0,max(freq)))

cbind(classes = classes,Frequency = freq)
#5
cum.freq <- cumsum(freq)

new<-c()
for(i in 1:length(breaks)){
  if(i==1){
    new[i]=0
  }else{
    new[i]=cum.freq[i-1]
  }
}
plot(breaks, new, type = 'l',main = "Cumalative Frequency polygon for Shareholders",xlab = "Shareholders", ylab = "Cumulative Frequency", ylim = c(0,max(cum.freq)))
cbind(Upper = breaks, cumFreq = new)


