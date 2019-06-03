#' @name reconstruct_dataset
#' @title Reconstruct data set
#' @description Function to reconstruct in spread format the table with the values predicted by the model
#' @usage reconstruct_dataset(test_data_unscaled, harvest_predicted)
#' @param test_data_unscaled generate test data object with scale parameter set to FALSE (scale = F). For mode details see \link{generate_test_data}.
#' @param harvest_predicted data frame with the predicted harvest value
#' @author Marcos Alves \email{mpplaves@gmail.com}
#' @examples reconstruct_dataset(test_data_unscaled, harvest_predicted)
#' @export

reconstruct_dataset <- function(test_data_unscaled, harvest_predicted){
  x = data.frame(test_data_unscaled, "gmC2" = harvest_predicted)
  y = spread(x,"LSU", "gmC2")
  y = y[,dim(test_data_unscaled)[2]:dim(y)[2]]
  return(y)
}
