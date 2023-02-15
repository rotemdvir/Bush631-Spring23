# Week 5
# Congress data
# UN Voting data
# QB contracts data

library(tidyverse)

## Upload Congress data
congress <- read.csv("~/Week5_Measurement_II/congress.csv")
View(congress)
head(congress)

# Explore data
summary(congress)

# Create scatter plots for ideology: 80th and 112th congress (same as textbook pp.98-99)
# Create subsets for Republican and Democratic party
rep <- subset(congress, subset = (party == "Republican"))
dem <- subset(congress, subset = (party == "Democrat"))

# Create subsets for 80th/112th congress for each party
rep80 <- subset(rep, subset = (congress == 80))
dem80 <- subset(dem, subset = (congress == 80))
rep112 <- subset(rep, subset = (congress == 112))
dem112 <- subset(dem, subset = (congress == 112))

# Scatterplot (using base R)
plot(dem80$dwnom1, dem80$dwnom2, pch = 16, col = "blue",
     xlim = c(-1.5,1.5), ylim = c(-1.5,1.5),
     xlab = "Liberalism/Conservatism: Economic scale",
     ylab = "Liberalism/Conservatism: Racial scale",
     main = "The 80th Congress")
points(rep80$dwnom1,rep80$dwnom2, pch = 16, col = "red")
text(-0.75,1,"Dems")
text(1,-1, "Reps")
abline(v = 0, col = "grey")
abline(h = 0, col = "grey")

## Calculate median ideology score by party (across full time frame)
dem.med <- tapply(dem$dwnom1, dem$congress, median)
rep.med <- tapply(rep$dwnom1, rep$congress, median)

# Plot trend line of ideology over time by party (same as textbook p. 100)
plot(names(dem.med), dem.med, col = "blue", type = "l",
     xlim = c(80,115), ylim = c(-1,1), xlab = "Congress",
     ylab = "DW-NOMINATE Score")
lines(names(rep.med), rep.med, col = "red")
text(110, -0.6, "Democrats")
text(110,0.8, "Republicans")

# Plot trend line of ideology over time by party (using tidyverse)
median_dw1 <- congress %>%
  filter(party %in% c("Republican","Democrat")) %>% group_by(party,congress) %>%
  summarise(median_dw1 = median(dwnom1))

ggplot(median_dw1, aes(x=congress,y=median_dw1,color = party)) + 
  geom_line(size = 2) + xlab("Congress") +
  ylab("DW-NOMINATE score") + 
  scale_color_manual(values = c("blue","red")) + 
  theme_bw()

### UN Voting Data   ###
mydata <- read.csv("~/Week5_Measurement_II/unvoting.csv")
View(mydata)

## Tidyverse code to manage data
# Create data set of mean proportion of voting with US and Russia by year
annual.agree <- mydata %>%
  group_by(Year) %>%
  summarise(us.agree = mean(PctAgreeUS, na.rm = T),
            ru.agree = mean(PctAgreeRUSSIA, na.rm = T))

# Plot the proportion of voting with US/USSR over time 
ggplot(data = annual.agree) +
  geom_line(mapping = aes(x = Year, y = us.agree), color = "blue") +
  geom_line(mapping = aes(x = Year, y = ru.agree), color = "red") +
  geom_text(aes(x = 2000, y = 0, label = "Voting with US"), color = "blue")+
  geom_text(aes(x = 2000, y = 1, label = "Voting with Russia"), color = "red") +
  geom_vline(aes(xintercept = 1989), linetype = "dotted", color = "grey") +
  geom_text(aes(x = 1993, y = 0.5, label = "Cold War Ends"), color = "black") +
  ylab("Proportion voting with Superpower") + theme_classic()

## Tables that show who votes with US/Russia (tidyverse approach)
# USA: create data of mean proportion of voting with US for all countries (over time)
# Arrange the data from highest to lowest proportion, display top 10 observations
# Remove US from list
mydata %>%
  group_by(CountryName) %>% 
  summarise(mean.pctUS = mean(PctAgreeUS)) %>%
  arrange(desc(mean.pctUS)) %>%
  head(n = 11) %>%
  filter(CountryName != "United States of America")
  
# Russia (same procedure as USA)
mydata %>%
  group_by(CountryName) %>%
  summarise(mean.pctRU = mean(PctAgreeRUSSIA)) %>% 
  arrange(desc(mean.pctRU)) %>%
  head(n=11) %>%
  filter(CountryName != "Russia")

### Q-Q plot
# Relationship b-w proportion of voting with US/USSR (explore the entire distribution)
qqplot(mydata$PctAgreeUS, mydata$PctAgreeRUSSIA, xlab = "UN voting with US",
       ylab = "UN voting with Russia",
       main = "UN voting with superpower: trend over time")
abline(0,1) 

### Correlation ###
cor(mydata$idealpoint, mydata$PctAgreeUS, use = "pairwise")
cor(mydata$idealpoint, mydata$PctAgreeRUSSIA, use = "pairwise")

# PLot correlation of ideal point and voting with US
cor_dat <- mydata %>%
  group_by(CountryName) %>% 
  summarise(mn1 = mean(idealpoint),
            mn2 = mean(PctAgreeUS)) %>%
  arrange(desc(mn1)) %>%
  head(n = 21) %>%
  filter(CountryName != "United States of America")

ggplot(cor_dat, aes(x=mn1,y=mn2)) +
  geom_point() + xlab("Liberal FP measure - Avg.") + ylab("Mean voting with US") +
  geom_label(aes(label = CountryName), size = 2, fill = "yellow") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme_classic() + ggtitle("Liberal FP and UN voting correlation - top 20 countries") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))


