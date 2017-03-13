## Offenders are now buying and selling credit card numbers on the
## Internet. To do so, they meet on online discussion forums and post
## ads for the credit numbers they want to buy and sell. The aim of
## this research is to determine the structure of ties of the online
## financial fraud on a discussion forum. The dataset contains the
## ACTOR ID that sold the credit card data, the ACTOR ID that bought
## it and, in the third column, the date of the transactions (FROM TO
## DATE). The structure of the ties should be analyzed on a monthly
## basis so that we can analyze the evolution of the structure over
## time. The three metrics used to assess the structure of ties are
## degree centrality, flow betweenness and density.
library(data.table)
transactions <- fread("transactions.txt")
setnames(transactions, c("seller", "buyer", "date.chr"))
transactions[, date.POSIXct := strptime(date.chr, "%Y-%m-%d")]

ggplot()+
  geom_bar(aes(date.POSIXct), data=transactions)

transactions[, month.chr := strftime(date.POSIXct, "%Y-%m")]

ggplot()+
  geom_bar(aes(month.chr), data=transactions)

transactions[, month.POSIXct := strptime(paste0(month.chr, "-01"), "%Y-%m-%d")]

ggplot()+
  geom_bar(aes(month.POSIXct), data=transactions)

per.month <- transactions[, list(
  transactions=.N
  ), by=list(seller, buyer, month.POSIXct)]
seller.info <- per.month[, list(
  buyers=.N
  ), by=list(seller, month.POSIXct)]
buyer.info <- per.month[, list(
  sellers=.N
  ), by=list(buyer, month.POSIXct)]
per.month2 <- per.month[seller.info, on=list(
  seller, month.POSIXct)][buyer.info, on=list(
    buyer, month.POSIXct)]
