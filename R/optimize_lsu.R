#' @name optimize_lsu
#' @title Optmize LSUs
#' @description Optimize LSU for each cell using a neural net model.
#' @usage Optimize_LSU(test_data, model, min_Lsu, max_Lsu, col_means,
#'   col_stddevs, range)
#' @param test_data Generated test data object, for mode details see
#'   \link{generate_test_data}
#' @param model .h5 saved keras model
#' @param min_Lsu lower boundary for constrained optmization
#' @param max_Lsu upper boundary for constrained optmization
#' @param col_means Center attributes from scaled training data of the model
#'   used in the parameter model.
#'   Example: train_data <- scale(train_data)
#'   col_means_train_harvest <- attr(train_data, "scaled:center")
#' @param col_stddevs Scale attributes from scaled training data of the model
#'   used in the parameter model.
#' @param range Cell interval to be optmized
#' @return Vector of optimized LSUs per grid cell
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @examples Optimize_LSU(test_data, ML_model_harvest,min_Lsu = -2, max_Lsu = 2,
#'                        col_means_train_harvest,col_stddevs_train_harvest,
#'                        range = 28415:28416)
#' @export optimize_lsu
#' @import tidyr


optimize_lsu <- function(test_data, model,min_Lsu, max_Lsu,col_means,col_stddevs, range){

  optimization = function(test_data){

    network_lsu = function(lsu){

      test_data["LSU"] = lsu
      y = model %>% predict(t(test_data))
      return(y)

    }

  opt = optimize(network_lsu, lower = min_Lsu, upper = max_Lsu, maximum = T)
  print(opt$maximum)
  LSU_optmimum = opt$maximum * col_stddevs["LSU"] + col_means["LSU"]
  return(LSU_optmimum)
  }
  # optmizing all cells using apply function
  optimum_lsu = apply(test_data[range,], 1, optimization)

  # combining the output with the grid indormation
  output_data = cbind(optimum_lsu,grid[range,])

  return(output_data)
}
