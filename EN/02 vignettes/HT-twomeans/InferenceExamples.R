before <- runif(100, 50, 70)
after <- runif(100, 60, 90)

measure <- c(before, after)
moment <- c(rep("before", 100), rep("after", 100))


id <- rep(1:100, 2)
sample <- data.frame(id, measure, moment)
head(sample) #first observations
tail(sample) #last observations


t.test(sample$measure ~ sample$moment, paired=TRUE)


class1 <- runif(80, 4, 6)
class2 <- runif(60, 3, 9)

print(class1, digits=2)
print(class2, digits=2)

t.test(class1, class2)

class3 <- runif(90, 4, 7)
class4 <- runif(70, 4, 7)

t.test(class3, class4)

