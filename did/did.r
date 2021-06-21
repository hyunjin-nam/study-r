
library(utils)
library(dplyr)
data <- read.csv('did.csv')
data <- data %>%
  mutate( is_ios = as.numeric(is_ios),
          after_update = as.numeric(after_update)
  )


summary(data)

did_model <- lm( app_download~is_ios*after_update, data=data)
summary(did_model)

b1 <- coef(did_model)[[1]]
b2 <- coef(did_model)[["is_ios"]]
b3 <- coef(did_model)[["after_update"]]
delta <- coef(did_model)[["is_ios:after_update"]]
C <- b1+b2+b3+delta 
E <- b1+b3 
B <- b1+b2 
A <- b1 
D <- E+(B-A) 
# This creates an empty plot:
plot(1, type="n", xlab="period", ylab="fte", xaxt="n",
     xlim=c(-0.01, 1.01), ylim=c(40000, 280000))
segments(x0=0, y0=A, x1=1, y1=E, lty=1, col=2)#control
segments(x0=0, y0=B, x1=1, y1=C, lty=3, col=3)#treated
segments(x0=0, y0=B, x1=1, y1=D,      #counterfactual
         lty=4, col=4)
legend("topright", legend=c("control", "treated", 
                            "counterfactual"), lty=c(1,3,4), col=c(2,3,4))
axis(side=1, at=c(0,1), labels=NULL)
