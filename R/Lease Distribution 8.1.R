#' The Length Function
#'
#' This function returns the length of a lease determined by the sales price balance.  The function returns the length in weeks.
#' @param sales.balanace The sales price balance (sales price-down payent) for the item on the contract. Defaults to 1500.
#' @param Option "LowPayment" or "LowCost" should be entered to return the correspoding length
#' @param gamma Do not adjust this parameter in production. Defaults to -1.8
#' @param alpha Do not adjust this parameter in production. Defaults to -0.0018
#' @param c Do not adjust this parameter in production. Defaults to 1.2
#' @param delta Do not adjust this parameter in production. Defaults to 136.8
#' @keywords Length
#' @export
#' @examples
#' length_function(sales.balance=1500,Option=c("LowCost","LowPayment"))
length_function=function(sales.balance=1500,Option="LowPayment",gamma=-1.8,alpha=0.0018,c=1.2,delta=136.8){
  #gamma=-1.8
  #alpha=0.0018
  #delta=136.8
  #c=1.2
  #sales.balance=seq(0,5000,100)#needs to be sales price balance
  #multiplier=1 #this is the length adjustment to get low total cost call back
  #a value of 1 is our standard low payment option
  multiplier=ifelse(Option=="LowCost",.7,1)
  length=c+(delta)/(1+exp(-1*(gamma+alpha*sales.balance)))
  adjustedlength=length*multiplier
  return(adjustedlength)
}
length_function(sales.balance=1000,Option=c("LowPayment","LowCost"))
