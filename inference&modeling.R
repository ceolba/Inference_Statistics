library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads
p<- 0.51
sqrt((p*(1-p))/(1000))

#Write a line of code that calculates the standard error se of a 
#sample average when you poll 25 people in the population.
#Plot se versus p for the 100 different proportions.
p <- seq(0,1,length = 100)
estd_err <- function (p){
    sqrt(p*(1-p)/25)
}
se<- lapply(p,estd_err)
se
plot(p,se)

#Exercise 6. Multiple plots of se versus p
#Using the same code as in the previous exercise, create a for-loop that
#generates three plots of p versus se when the sample sizes equal N=25,N=100 ,
#and N=1000
p <- seq(0, 1, length = 100)
sample_sizes <- c(25, 100, 1000)
estd_err <- function (p,s){
    sqrt(p*(1-p)/s)
}
se <-estd_err(p,sample_sizes)
se
plot(p,se)
class(sample_sizes)
class(p)
class(se)
class(estd_err)
p <- seq(0, 1, length = 100)
sample_sizes <- c(25, 100, 1000)
for (N in sample_sizes){
    se <-sqrt(p*(1-p)/N)
    plot(p,se,ylim = c(0,0.5/sqrt(25)))
}


#which we know is about 95%:
pnorm(1.96)-pnorm(-1.96)

X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

#Code: Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
x_hat
# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
    x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
    mean(x)
})

#Code: Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
    ggplot(aes(x_hat)) +
    geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
    ggplot(aes(sample = x_hat)) +
    stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
    geom_abline() +
    ylab("X_hat") +
    xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

#Code: Plotting margin of error in an extremely large poll over a range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
    ggplot(aes(p, SE)) +
    geom_line()

#Code: Plotting margin of error in an extremely large poll 
#over a range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
    ggplot(aes(p, SE)) +
    geom_line()


#Write function called take_sample that takes the proportion of Democrats 
#and the sample size N as arguments and returns 
#the sample average of Democrats (1) and Republicans (0).
take_sample <- function(p,N){x <- sample(c(0,1), size = N, replace = TRUE,
                                         prob = c(1-p, p))
x_hat <- mean(x)
x_hat}
take_sample(0.45, 100)

#Exercise 2. Distribution of errors - 1
#Assume the proportion of Democrats in the population 
#equals 0.45 and that your sample size N 
#is 100 polled voters. The take_sample function you defined 
#previously generates our estimate X-, 
#Replicate the random sampling 10,000 times and calculate 
#for each random sample. Save these differences as a vector called errors.
#Find the average of errors and plot a histogram of the distribution.
p <- 0.45
N <- 100
B<- 10000
#Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, {   
    p - take_sample(p,N)
})
# Calculate the mean of the errors. Print this value to the console.
mean(errors)

hist(errors)

#Exercise 4. Average size of error
#The error is a random variable p - X-. In practice, the error 
#is not observed because we do not know the actual proportion
#of Democratic voters. However, we can describe the size of the 
#error by constructing a simulation.
#Exercise 4. Average size of error
#The error is a random variable. In practice, the error is not observed
#because we do not know the actual proportion of Democratic voters, 
# However, we can describe the size of the error by constructing
#a simulation. abs|p-X-|
mean(errors) 

#Exercise 5. Standard deviation of the spread
## Calculate the standard deviation of `errors`
sd_errors <- sqrt(mean(errors^2))
sd_errors

#Exercise 6. Estimating the standard error
SE<- sqrt((1-p)*p/N)
SE

#Exercise 7. Standard error of the estimate
#In practice, we don't know 
#, so we construct an estimate of the theoretical prediction 
#based by plugging in X-
# for p. Calculate the standard error of the estimate:SE^(X-)
x <- sample(c(0,1), size = N, replace = TRUE,
            prob = c(1-p, p))

# Define `X_bar` as the average sampled proportion
X_bar <- mean(x)
# Calculate the standard error of the estimate. Print the result to the console.
SE <- sqrt(X_bar*(1-X_bar)/N)
SE

#Exercise 8. Plotting the standard error
#Create a plot of the largest standard error for ranging from 100 to 5,000.
#Based on this plot, how large does the sample size have to be to have a 
#standard error of about 1%?
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
N<- 10000*p*(1-p)
N

#Exercise 11. Plotting the errors
#Make a qq-plot of the errors you generated previously to see 
#if they follow a normal distribution.
library(ggplot2)
qqnorm(errors)
qqline(errors)
ggplot(data=data.frame(errors=errors), aes(sample=errors)) +
    geom_qq() +
    geom_abline()

#Exercise 12. Estimating the probability of a specific value of X-bar
#If p=0.45 and N=100, use the central limit theorem to estimate
#the probability that X->0.5 
p<- 0.45
N<- 100
std_dev <- sqrt(p*(1-p)/N)
1-pnorm(0.5,p,std_dev)

#Exercise 13. Estimating the probability of a specific error size
X_hat <- 0.51
# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)
# Calculate the probability that the error is 0.01 or larger
1-pnorm(0.01, 0,se_hat) + pnorm(-0.01,0,se_hat)


#Code: geom_smooth confidence interval example
#The shaded area around the curve is related to the concept of 
#confidence intervals.
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
    ggplot(aes(year, temperature)) +
    geom_point() +
    geom_smooth() +
    ggtitle("Average Yearly Temperatures in New Haven")

#Code: Monte Carlo simulation of confidence intervals
#Note that to compute the exact 95% confidence interval, 
#we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))   
# generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean
#of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)# build interval of 2*SE above and below mean

#: Solving for Z with qnorm
z <- qnorm(0.995)# calculate z to solve for 99% confidence interval
z
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability

#Code: Monte Carlo simulation
#Note that to compute the exact 95% confidence interval,
#we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
B <- 10000
inside <- replicate(B, {
    X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
    X_hat <- mean(X)
    SE_hat <- sqrt(X_hat*(1-X_hat)/N)
    between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

#Code: Confidence interval for the spread with sample size of 25
#Note that to compute the exact 95% confidence interval, we would 
#use c(-qnorm(.975), qnorm(.975)) instead of 1.96.
N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)

#Code: Computing a p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))


#Exercise 1. Confidence interval for p
#Assume there are only two candidates and construct a 95% confidence 
#interval for the election night proportion p
library(dslabs)
data("polls_us_election_2016")
head(polls_us_election_2016)
polls <- polls_us_election_2016 %>%
filter(enddate >= 2016-10-31 & state =="U.S.")
nrow(polls_us_election_2016)
N <- polls$samplesize[1]
N
# For the first poll in `polls`, assign the estimated
#percentage of Clinton voters to a variable called `X_hat`.
#Print this value to the console.
X_hat <- polls$rawpoll_clinton[1]/100
X_hat
# Calculate the standard error of `X_hat` and save it to a variable
#called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion 
#of Clinton voters. Save the lower and then the upper confidence interval
#to a variable called `ci`.
ci<- c(X_hat - qnorm(.975)*se_hat, X_hat + qnorm(.975)*se_hat)
ci

#Exercise 2. Pollster results for p
#Create a new object called pollster_results that contains the pollster's name,
#the end date of the poll, the proportion of voters who declared a vote for Clinton, the standard error of this estimate, and the lower and upper bounds of the confidence interval for the estimate.
head(polls)

# Create a new object called `pollster_results` that contains columns 
#for pollster name, end date, X_hat, se_hat, lower confidence interval, 
#and upper confidence interval for each poll.
pollster_results <- polls %>%
    mutate(X_hat=rawpoll_clinton/100, se_hat=sqrt(X_hat*(1-X_hat)/samplesize),lower=X_hat - qnorm(.975)*se_hat, upper=X_hat + qnorm(.975)*se_hat) %>%
    select(pollster,enddate,X_hat,se_hat, lower,upper)
head(pollster_results)

#Exercise 3. Comparing to actual results - p
#The final tally for the popular vote was Clinton 48.2% and Trump 46.1%. 
#Add a column called hit to pollster_results that states if the confidence interval
#included the true proportion p=0.42
#or not. What proportion of confidence intervals included p?
avg_hit <- pollster_results %>%
    mutate(hit= lower <= 0.482 & upper>= 0.482) %>%
    summarize(mean_hit=mean(hit))
head(pollster_results)

avg_hot <- pollster_results %>%
    mutate(hit= lower <= 0.482 & upper>= 0.482)
mean(avg_hot$hit)



#Exercise 5. Confidence interval for d
# Add a statement to this line of code that will add a new column named `d_hat` 
#to `polls`. The new column should contain the difference in the proportion of
#voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>%
    mutate(d_hat=(rawpoll_clinton - rawpoll_trump)/100)
# Assign the sample size of the first poll in `polls` to a variable called `N`.
#Print this value to the console.
N <- polls$samplesize[1]
N
# Assign the difference `d_hat` of the first poll in `polls` to a variable 
#called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1]
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat<- (d_hat+1)/2
X_hat
# Calculate the standard error of the spread and save it to a variable 
#called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the difference in 
#the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(d_hat - qnorm(.975)*se_hat, d_hat + qnorm(.975)*se_hat)
ci

#Exercise 6. Pollster results for d
# Create a new object called `pollster_results` that contains 
#columns for pollster name, end date, d_hat, lower confidence interval 
#of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <- polls %>%
    mutate(X_hat=(d_hat+1)/2, se_hat=2*sqrt(X_hat*(1-X_hat)/samplesize),lower=d_hat - qnorm(.975)*se_hat, upper=d_hat + qnorm(.975)*se_hat) %>%
    select(pollster,enddate,d_hat, lower,upper)
head(pollster_results)

#Exercise 7. Comparing to actual results - d
#What proportion of confidence intervals for the difference between 
#the proportion of voters included d, the actual difference in election day?
avg_hit <- pollster_results %>%
    mutate(hit= lower <= 0.021 & upper>= 0.021)
head(avg_hit)

mean(avg_hit$hit)
#Otra forma de realizar el ejercicio
avg_hot <- pollster_results %>%
    mutate(hit= lower <= 0.021 & upper>= 0.021) %>%
    summarize(mean_hit=mean(hit))
avg_hot


#Exercise 8. Comparing to actual results by pollster
#Although the proportion of confidence intervals that include the actual 
#difference between the proportion of voters increases substantially, it is
#still lower that 0.95. In the next chapter, we learn the reason for this.
#To motivate our next exercises, calculate the difference between each poll's
#estimate d and the actual d=0.021 . Stratify this difference, or error, by pollster 
#in a plot.
polls %>% mutate(error =d_hat - 0.021)%>%
    ggplot(aes(x = pollster, y = error)) +
    
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Exercise 9. Comparing to actual results by pollster - multiple polls
#Remake the plot you made for the previous exercise, but only for pollsters that 
#took five or more polls.
#You can use dplyr tools group_by and n to group data by a variable of interest 
#and then count the number of observations in the groups. The function filter 
#filters data piped into it by your specified condition.
polls %>% mutate(error =d_hat - 0.021)%>%
    group_by(pollster) %>% 
    filter(n() >= 5) %>%
    ggplot(aes(x = pollster, y = error)) +
    geom_point ()
polls$pollster


#Overview
#In June 2016, the United Kingdom (UK) held a referendum to determine whether the
#country would “Remain” in the European Union (EU) or “Leave” the EU. This referendum is 
#commonly known as Brexit. Although the media and others interpreted poll results as 
#forecasting “Remain” (p > 0.5), the actual proportion that voted “Remain” was only 
#48.1% (p > 0.481) and the UK thus voted to leave the EU. Pollsters in the UK were 
#criticized for overestimating support for “Remain”.
#In this project, you will analyze real Brexit polling data to develop polling models 
#to forecast Brexit results. You will write your own code in R and enter the answers on
#the edX platform.

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
d

#Question 2: Actual Brexit poll estimates
#Load and inspect the brexit_polls dataset from dslabs, which contains actual polling
#data for the 6 months before the Brexit vote. Raw proportions of voters preferring
#“Remain”, “Leave”, and “Undecided” are available (remain, leave, undecided) The 
#spread is also available (spread), which is the difference in the raw proportion of 
#voters choosing “Remain” and the raw proportion choosing “Leave”.
#Calculate x_hat for each poll, the estimate of the proportion of voters choosing “Remain”
#on the referendum day (), given the observed spread and the relationship . 
#Use mutate() to add a variable x_hat to the brexit_polls object by filling in the 
#skeleton code below:
head(brexit_polls)
    brexit_polls <- brexit_polls %>%
    mutate(x_hat = (spread + 1)/2)
# What is the average of the observed spreads (spread)?
mean(brexit_polls$spread)

# What is the standard deviation of the observed spreads?
sd(brexit_polls$spread)

# What is the average of x_hat, the estimates of the parameter p?
mean(brexit_polls$x_hat)  

# What is the standard deviation of x_hat?
sd(brexit_polls$x_hat)

#Question 3: Confidence interval of a Brexit poll
#Consider the first poll in brexit_polls, a YouGov poll run on the same day as the
#Brexit referendum:
    brexit_polls[1,]
    
# What is the lower bound of the 95% confidence interval?
    0.52 - qnorm(.975)*sqrt(.52*(1-.52)/4772)

# What is the upper bound of the 95% confidence interval?
    0.52 + qnorm(.975)*sqrt(.52*(1-.52)/4772)    
    
#Question 4: Confidence intervals for polls in June
#Create the data frame june_polls containing only Brexit polls ending in June 2016 
#(enddate of “2016-06-01” and later). We will calculate confidence intervals for all
#polls and determine how many cover the true value of d
# Create the data frame june_polls containing only Brexit polls ending in June 2016
    june_polls <- data.frame(brexit_polls %>%
                                 filter(enddate >= "2016-06-01" & enddate <= "2016-06-30") %>%
                                 mutate(se_x_hat = sqrt(x_hat * (1-x_hat)/samplesize),
                                        se_d = 2*se_x_hat, 
                                        lower = (2 * x_hat - 1) - qnorm(.975)*se_d,
                                        upper = (2 * x_hat - 1) + qnorm(.975)*se_d,
                                        hit = -0.038 >= lower & -0.038 <= upper,
                                        zero_ci = c(lower <= 0 & upper >= 0),
                                        above_zero = c(lower > 0)
                                 )
    )
    
# How many polls are in june_polls?
    print(nrow(june_polls))  
    head(june_polls)
    
# What proportion of polls have a confidence interval that covers the value 0?
    mean(june_polls$zero_ci)

# What proportion of polls predict "Remain" (confidence interval entirely above 0)?
    mean(june_polls$above_zero)

# What proportion of polls have a confidence interval covering the true value of d?
    mean(june_polls$hit)

#Question 5: Hit rate by pollster
# Group and summarize the june_polls object by pollster to find the proportion of 
# hits for each pollster and the number of polls per pollster. Use arrange() to sort 
#by hit rate.
    june_polls %>% group_by(pollster) %>%
        summarize(proportion = mean(hit), polls = n()) %>%
        arrange(desc(proportion)) 
    
#Question 6: Boxplot of Brexit polls by poll type
    june_polls %>%
        ggplot(aes(poll_type, spread, colour = poll_type))+
        labs(title="Spread per poll types", x="Poll type", y = "Spread")+
        geom_boxplot()+
        geom_jitter(shape=16, position=position_jitter(0.2), colour = "black", legend.position = "none")
    
#Question 7: Combined spread across poll type
#Calculate the confidence intervals of the spread combined across all polls in june_polls
#grouping by poll type. Recall that to determine the standard error of the spread, 
#you will need to double the standard error of the estimate.
    combined_by_type <- june_polls %>%
        group_by(poll_type) %>%
        summarize(N = sum(samplesize),
                  spread = sum(spread*samplesize)/N,
                  p_hat = (spread + 1)/2)
    combined_by_type %>%
        mutate(lower = spread - qnorm(.975)*2*sqrt(p_hat*(1-p_hat)/N),
               upper = spread + qnorm(.975)*2*sqrt(p_hat*(1-p_hat)/N),
               ci_amplitude = abs(upper-lower),
               hit_true_d = lower <= -0.038 & upper >= -0.038
        )

#Question 9: Chi-squared p-value
#Define brexit_hit, with the following code, which computes the confidence intervals 
#for all Brexit polls in 2016 and then calculates whether the confidence interval 
#covers the actual value of the spread d = -0.038:
 # suggested libraries
    library(tidyverse)
    
    # load brexit_polls object and add x_hat column
    library(dslabs)
    data(brexit_polls)
    brexit_polls <- brexit_polls %>%
        mutate(x_hat = (spread + 1)/2)
    
    # final proportion voting "Remain"
    p <- 0.481
    
    brexit_hit <- brexit_polls %>%
        mutate(p_hat = (spread + 1)/2,
               se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
               spread_lower = spread - qnorm(.975)*se_spread,
               spread_upper = spread + qnorm(.975)*se_spread,
               hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
        select(poll_type, hit)
    
    # Contingency table for brexit_hit
    brexit_table <- t(table(brexit_hit$poll_type, brexit_hit$hit))
    brexit_table
    
## Chi-square
    chisq.test(brexit_table)

#Question 10: Odds ratio of online and telephone poll hit rate
#Use the two-by-two table constructed in the previous exercise to calculate the odds 
#ratio between the hit rate of online and telephone polls to determine the magnitude 
#of the difference in performance between the poll types.
# Calculate the odds that an online poll generates a confidence interval that covers 
#the actual value of the spread.
    odds_table <- data.frame(poll_type = c("no hit", "hit"),
                             online = c(brexit_table[1,1], brexit_table[2,1]),
                             telephone = c(brexit_table[1,2], brexit_table[2,2])
    )
    odds_table
    odds_online <- with(odds_table, (online[2]/sum(online))/(online[1]/sum(online)))
    odds_online 
    
## Calculate the odds that a telephone poll generates a confidence interval that covers 
#the actual value of the spread.
    odds_telephone <- with(odds_table, (telephone[2]/sum(telephone))/(telephone[1]/sum(telephone)))
    odds_telephone
    
## Calculate the odds ratio to determine how many times larger the odds are for online 
#polls to hit versus telephone polls.
    odds_online/odds_telephone
    