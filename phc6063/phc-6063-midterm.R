## question 2:

weight <- c(107, 119, 99, 114, 120, 125, 114, 95, 117, 116, 124, 114, 88,
            121, 100, 152, 113, 119, 150)

summary(weight)

boxplot(weight,
        main = "Ideal Body Weight Among Sample",
        ylab = "Ideal Body Weight",
        col = "pink")

hist(weight,
     main = "Ideal Body Weight Among Sample Population of Diabetics",
     xlab = "Ideal Body Weight", col = "pink")

sd(weight)


## question 3:

q3data <- read.csv("Q3.csv")

cor(q3data$x, q3data$y, method = "pearson")

plot(q3data$x, q3data$y, xlab = "x1", ylab = "y1", main = "Q3 Data", pch = 19)

## question 5:


library(rpact)

getSampleSizeSurvival(
  alpha = 0.05, beta = 0.2, median1 = 9, ## control 9 months
  median2 = 12,  ## treatment expected 12 months
  allocationRatioPlanned = 1
)

## question 7:

library(readxl)

dataq7 <- read_excel("Data-SEERxStat.xlsx")

freq <- dataq7 %>% 
  filter(Race == "All") %>% 
  summarise(across('2000':'2017', sum))


male.T1 <-sum(dataq7[dataq7$Sex=="Male", c("2000", "2001", "2002", "2003", "2004","2005","2006","2007","2008")])
male.T2 <- sum(dataq7[dataq7$Sex=="Male", c("2009","2010","2011","2012","2013","2014","2015","2016","2017")])

female.T1 <- sum(dataq7[dataq7$Sex=="Female", c("2000","2001","2002","2003","2004","2005","2006","2007","2008")])
female.T2 <- sum(dataq7[dataq7$Sex== "Female",c("2009", "2010","2011","2012", "2013","2014","2015","2016","2017")] )

q7table <- matrix(
  c(male.T1, male.T2, female.T1, female.T2),nrow = 2,
  byrow = TRUE)

rownames(q7table) <- c("Male", "Female")
colnames(q7table) <- c("T1","T2")

chisq.test(q7table)






white.T1 <- sum(dataq7[dataq7$Race == "White", c("2000","2001", "2002","2003","2004","2005","2006","2007","2008")])
white.T2 <- sum(dataq7[dataq7$Race=="White", c("2009","2010", "2011", "2012","2013", "2014", "2015", "2016","2017")])

black.T1 <- sum(dataq7[dataq7$Race=="Black", c("2000", "2001", "2002","2003","2004","2005","2006","2007","2008")])
black.T2 <- sum(dataq7[dataq7$Race=="Black", c("2009","2010","2011","2012","2013","2014","2015", "2016","2017")])

q7table2 <- matrix(
  c(white.T1, white.T2, black.T1, black.T2), nrow = 2, byrow = TRUE)

rownames(q7table2) <- c("White", "Black")
colnames(q7table2) <- c("T1","T2")

chisq.test(q7table2)

freq2 <- freq %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Frequency"
  )

freq2 %>% 
  ggplot(aes(x = as.numeric(Year), y = Frequency)) +
  geom_line(color = "hotpink") +
  geom_point() +
  labs(
    title = "Frequency of Pancreatic Cancer Cases Over Period 2000–2017",
    x = "Year",
    y = "Number of Cases"
  )


gender.df <- data.frame(
  Period = c("2000-2008","2000-2008","2009-2017","2009-2017"),
  Sex = c("Male","Female","Male","Female"),
  Count = c(male.T1, female.T1, male.T2, female.T2)
)


gender.df %>% 
  ggplot(aes(x = Period, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Gender by Time Period", y = "Cases (count)", x = "")

race.df <- data.frame(
  Period =c("2000-2008","2000-2008","2009-2017","2009-2017"),
  Race = c("White","Black","White","Black"),
  Count = c(white.T1, black.T1, white.T2, black.T2)
)


race.df %>% 
  ggplot(aes(x = Period, y = Count, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Race by Time Period",
       x = "", y = "Cases (count)")


