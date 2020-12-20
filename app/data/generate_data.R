setwd("./data")
set.seed(1)
n <- 150
n.s <- 3

id <- rep(1:n)
name <- rep(c("excavator", "compactor", "loader"), each=50)
hours_usage <- rnorm(n, 500, 100)
time_to_malfunction <- rnorm(n, 16000, 3000) - hours_usage  
q1 <- quantile(time_to_malfunction, 1/3)
q2 <-  quantile(time_to_malfunction, 2/3)

condition <- rep(NA, n)

condition[which(time_to_malfunction < q1)] <- "bad"
condition[which(time_to_malfunction > q1 & time_to_malfunction < q2)] <- "moderate"
condition[which(time_to_malfunction > q2)] <- "excellent"
machine <- data.frame(id, name, hours_usage, time_to_malfunction, condition)


id <- (1000:(1000+(n*n.s)-1))
machine_id <- unlist(rep(machine$id, each=3))
type <- rep(rep(c("thickness", "pressure", "filter"), times=3), time=50)
value <- rep(NA, n*n.s)

condition.s <- rep(condition, each=3)
value[which(condition.s=="bad")] <- rnorm(length(which(condition.s=="bad")), 100, 30)
value[which(condition.s=="moderate")] <- rnorm(length(which(condition.s=="moderate")), 200, 30)
value[which(condition.s=="excellent")] <- rnorm(length(which(condition.s=="excellent")), 300, 30)
senser <- data.frame(id, machine_id, type, value)

write.csv(machine, "machine.csv", row.names=FALSE)
write.csv(senser, "senser.csv", row.names=FALSE)
