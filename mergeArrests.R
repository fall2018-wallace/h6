
library("ggplot2")
localUSArrests<-USArrests
localUSArrests
df
df<-df[-9,]
row.names(localUSArrests)
colnames(df)
localUSArrests$stateName <- df$stateName  
df <- merge(df, localUSArrests, by = "stateName")   #merging by the common column stateName
df                        

#Q4
summary(df$population)
sd(df$population)
g <- ggplot(df, aes(x=population)) + geom_histogram(binwidth = 1102046, bins=30, fill='white', color='steelBlue') + theme_classic()  #Set the binwidth to minimum value.
g
summary(df$Murder)
f <- ggplot(df, aes(x=Murder)) + geom_histogram(binwidth=0.4695 ,bins=30, fill='white', color='red') + theme_classic() #Set the binwidth to minimum value.
f

#Q5
a <- ggplot(df, aes(x="", y = population)) + geom_boxplot(outlier.color='red', outlier.size=2, outlier.shape=16, notch=TRUE, color='steelBlue', fill='gray') + theme_classic() 
a
b <- ggplot(df, aes(x="", y = Murder)) + geom_boxplot(outlier.color='red', outlier.size=2, outlier.shape=16, notch=TRUE, color='red', fill='gray') + theme_classic() + labs(y='Murder rate')
b

#Q6
#I find boxplot more useful because the detailed distribution range-wise and other summary can also be 
#inferred.

#Q7
df$MurderNumber <- df$Murder*df$population/100000  
summary(df$MurderNumber)
df[,c("MurderNumber","stateName")] #Outputting number of murders with state names for reference

#Q8
c <- ggplot(df, aes(x = stateName,y= MurderNumber)) + geom_col() #Creating a simple bar chart for number of murders per state
c 

#Q9
w<-ggplot(data=df, aes(stateName,MurderNumber)) + geom_col()
w1<-w+geom_line()
w1<-w1+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Total Murders")
w1

#Q10
w2<-ggplot(data=df, aes(x=reorder(stateName,MurderNumber), y=MurderNumber)) + geom_col()
w2<-w2+geom_line()
w2<-w2+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Total Murders")
w2


#Q11
w3<-ggplot(data=df, aes(x=reorder(stateName,MurderNumber), y=MurderNumber, fill=percentOver18)) + geom_col()
w3<-w3+geom_line()
w3<-w3+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Total Murders")
w3


#Q12
d <- ggplot(df, aes(x= population, y=percentOver18)) + geom_point(color=df$Murder, size=df$Murder) + labs(x='Population percentage over 18', y='Percentage') 
d
