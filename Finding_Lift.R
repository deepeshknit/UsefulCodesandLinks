#Function to find out the response rate and lift of the model for given percentile
lift1 <- function(Data, dv, pred.dv, percent){
  str <- paste0("cutoff <- quantile(Data$", pred.dv,", (1 - percent/100), na.rm = T)")
  eval(parse(text = str))
  str <- paste0("ResponseRate <- round((sum(Data[which(Data$", pred.dv, " >= cutoff), ]$", dv,")/nrow(Data[which(Data$",pred.dv," >= cutoff), ])) * 100, 2)")
  eval(parse(text = str))
  str <- paste0("OverallResponseRate <- round((sum(Data$", dv,")/nrow(Data) * 100), 2)")
  eval(parse(text = str))
  Lift <- round(ResponseRate / OverallResponseRate, 2)
  Lift <- list(cutoff, ResponseRate, OverallResponseRate, Lift)
  names(Lift) <- c("cutoff", "ResponseRate", "OverallResponseRate", "Lift")
  return(Lift)
}