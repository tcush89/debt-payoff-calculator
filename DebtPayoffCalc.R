#########################################################################
#########################################################################
######### Debt Payoff Calculator  #######################################
######### (c) 2022 Thomas Cushman #######################################
#########################################################################
#########################################################################

#Set up the environment
require(tidyverse)
require(data.table)
gc()
cat("\014")
rm(list = ls())

#1: Set principal as p
p <- 15020.28

#2: Set APY as i represented as full decimal percentage
i <- 25.99

#3: Set desired monthly payment as pmt
pmt <- 3007

#Now run and enjoy the output
cnt <- 0
tot_interest <- 0

#All the calculation goodness
i <- i/100
while (p > 0) {
  tot_interest <- tot_interest + ((i/12)*p)
  p <- p + ((i/12)*p)
  if (exists("payoff_table")) {
    month <- cnt
    rem_principal <- p
    payoff_table <- add_row(payoff_table, month = cnt, rem_principal = p)
  } else {
    month <- cnt
    rem_principal <- p
    payoff_table <- tibble(month, rem_principal)
  }
  if (p < pmt) {
    final_p <- round(p, 2)
  }
  p <- p - pmt
  cnt <- cnt + 1
}

#Output variable parsing/reformatting
tot_interest <- round(tot_interest, 2)
final_pmt <- round(as.double(payoff_table[cnt, "rem_principal"]), 2)
final_month <- cnt + 1
rem_principal <- round(rem_principal, 2)
payoff_table$rem_principal <- round(payoff_table$rem_principal, 2)

#Uncomment below section if you would like payoff_table as data.table in addition to tibble
#payoff_table <- as.data.table(payoff_table)
#payoff_tibble <- as_tibble(payoff_table)

#Final output code
index <- 1
for (index in 1:nrow(payoff_table)) {
  print(sprintf("Month %i remaining balance: $%.2f",index-1, payoff_table$rem_principal[index]))
}

print(sprintf("Your debt will be paid off in %i months", index))

print(sprintf("Total interest paid will be $%.2f", tot_interest))

line_plot <- ggplot(data=payoff_table, aes(x=month, y=rem_principal, group=1))
line_plot <- line_plot + geom_line()
line_plot <- line_plot + geom_point()
line_plot <- line_plot + ggtitle("Principal over Time") + xlab("Month") + ylab("Principal")
line_plot <- line_plot + theme(plot.title = element_text(face = "bold", size = 18))
line_plot

#Uncomment to save plot to working directory
#ggsave("rem_principal_plot.png", width = 5, height = 4, dpi = 300)
