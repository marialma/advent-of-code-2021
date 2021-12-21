advent <- scan("data/day7_input", sep=",")
print(sum(abs(advent - median(advent))))

form <- function(advent, x) { 
  sum((abs(advent - x) * (abs(advent - x) + 1)) / 2)
}

#start from average

start <- floor(mean(advent))
old_x <- start
current_answer <- form(advent, old_x)
print(current_answer)
trace <- c()

for (iter in 1:1000) {
  randnum <- floor(runif(n = 1, min = -50, max = 50))
  new_x <- old_x + randnum
  new_answer <- form(advent, new_x)
  trace <- c(trace, new_answer)
  if(new_answer < current_answer) {
    current_answer <- new_answer
    old_x <- new_x} else {next}
}
print(current_answer)

# well that was stupid. obviously starting with the mean was the answer. 
# but i suppose it was a good opportunity to write an optimization function? 

# added a plot so you can see the trace
steps <- seq(1:length(trace))
trace_plot <- data.frame(steps, trace)
ggplot() + geom_line(aes(x=steps, y = trace))