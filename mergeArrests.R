
 localUSArrests<-USArrests
 localUSArrests

df
df<-df[-9,]
row.names(localUSArrests)     #Checking if the row names are stateNames, which they are not. 
#We will have to create a column with these names so that we have a common column to merge the dataframes by.

colnames(df)            #Checking if dfstates has statenames as a column. It does.
localUSArrests$stateName <- df$stateName   #making the required column.
#View(localUSArrests)       #checking if rownames match the stateName for consistency

df <- merge(df, localUSArrests, by = "stateName")   #merging by the common column stateName
df                        
#View(df)         #viewing the column for proper merging

#Step B

#Q4
summary(df$population) #finding out minimum and maximum to decide binwidth.
#best way to define binwidth would be to use product of standard deviation and inverse of cube root of number of observations.
# which would be (0.2714 * 7345270)/35.35 which comes out to be 56393. however the histogram appears too crowded with lines and is hard to work with.
#so approx binwidth for population i am determining by (max-min)/(square root of number of observations) which comes out to be 1102046
sd(df$population)
g <- ggplot(df, aes(x=population)) + geom_histogram(binwidth = 1102046, bins=30, fill='white', color='steelBlue') + theme_classic()  #Set the binwidth to minimum value.
g
summary(df$Murder) #finding out minimum and maximum to decide binwidth.
#binwidth here becomes 0.4695 for murder.
f <- ggplot(df, aes(x=Murder)) + geom_histogram(binwidth=0.4695 ,bins=30, fill='white', color='red') + theme_classic() #Set the binwidth to minimum value.
f

#Q5
#Creating boxplot of population with outliers set to be shown as red and with a viewable size. Adding notch to see the distribution across
#overlapping median line
#barplot for population
a <- ggplot(df, aes(x="", y = population)) + geom_boxplot(outlier.color='red', outlier.size=2, outlier.shape=16, notch=TRUE, color='steelBlue', fill='gray') + theme_classic() 
a
#barplot for Murder
b <- ggplot(df, aes(x="", y = Murder)) + geom_boxplot(outlier.color='red', outlier.size=2, outlier.shape=16, notch=TRUE, color='red', fill='gray') + theme_classic() + labs(y='Murder rate')
b

#Q6
#Boxplot as a whole seems more useful to me since you can understand the distribution in a more details manner as you get an idea of median, outliers
#and majority of the distribution being focused in a certain range.
#histogram only provides bins according to a set of values which might tell us how many states may exist in what range but you can not get a clear
#idea of which state(s) lie at the median point. 
#in boxplot outliers will tell you the extremeties of the values in this case and as we can see 4 states have much more population than other states.
#We can also tell that there is precisely one state which reaches close to 40 million population.
# we can also tell that the majority of the states lie in between 3 million and 7 million

#Q7

#Murder rate is per 1,00,000. 
#So we multiply with actual population and divide by 1,00,000 to get the number of murders
df$MurderNumber <- df$Murder*df$population/100000  
#View(df)
summary(df$MurderNumber)
df[,c("MurderNumber","stateName")] #Outputting number of murders with state names for reference

#Q8
c <- ggplot(df, aes(x = stateName,y= MurderNumber)) + geom_col() #Creating a simple bar chart for number of murders per state
c 

#Q9
c + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x='Total Murders', y='Number Of Murders') #rotating text and adding lable using theme and labs function.

#Q10
df$stateName <- factor(df$stateName, levels = df$stateName[order(df$Murder)]) #Giving levels to stateName by ordering itself by Murder rate.
c <- ggplot(df, aes(x = stateName,y= MurderNumber)) + geom_col() #ggplot sorts the x axis by itself by recognizing the levels from x attribute which is stateName
c + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x='Total Murders', y='Number Of Murders') #output

#Q11
c <- ggplot(df, aes(x = stateName,y= MurderNumber)) + geom_col(color=df$percentOver18) + theme_classic() #etting color of the barchart by using percentOver18 as levels
c + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x='Total Murders', y='Number Of Murders') #output

#Q12
#generating a barchart by using geom_point function, color and size is set to levels by Murder rate, legend is disabled.
#added alpha transparency to see the overlaps between points.
#labels are added for better understanding and theme is set to minimal to emphasize the overlap between colors using alpha.
d <- ggplot(df, aes(x= population, y=percentOver18)) + geom_point(color=df$Murder, size=df$Murder, show.legend = FALSE, alpha=0.8) + labs(x='Population percentage over 18', y='Percentage') + theme_minimal()
d
