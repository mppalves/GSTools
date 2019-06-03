#'@name ploting_individual_cells
#'@title Plotting individual cells behaviour in 3D
#'@description Function to plot selected cells response to changes in LSU and
#'  climate variables
#'@usage ploting_individual_cells(test_data,cell,Lsu_range, climate_variable, climate_variable_range, ML_model)
#'@param test_data Output from the function test_data containing neat data for
#'  machine learning model tests.
#'@param cell Cell number from 1 to 67420.
#'@param lsu_range Normalized ploting range ex: from -1.5 to 2.5
#'@param climate_variable One of the following: "temperature_mean", "precipitation_mean", "radiation_mean"
#'@param climate_variable_range Normalized ploting range ex: from -1.5 to 2.5
#'@param ML_model Trained machine learning model saved in hd5
#'@author Marcos Alves \email{mppalves@gmail.com}
#'@examples ploting_individual_cells(test_data=test_data, cell = 28415,Lsu_range =c(-1.5,1.5) ,climate_variable = "precipitation_mean",
#'                                   climate_variable_range = c(-1.5,1.5), ML_model_harvest)
#'@export

library(plotly)

ploting_individual_cells = function(test_data,cell,Lsu_range, climate_variable, climate_variable_range, ML_model){
  #Ploting the model results for one cell

  x = seq(Lsu_range[1],Lsu_range[2],0.1)
  r = seq(climate_variable_range[1],climate_variable_range[2],0.1)
  test_input = test_data[cell,]

  res=data.frame()
  for (i in 1:length(x)) {
    test_input["LSU"] = x[i]
    for (j in 1:length(r)) {
      test_input[climate_variable] = r[j]
      y = predict(ML_model, t(matrix(test_input)))
      res[j,i] = y
    }
  }

  axx = list(title = "lsu")
  axy = list(title = climate_variable)
  axz = list(title = "output")

  p <- plot_ly(y=r, x=x, z = as.matrix(res)) %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))  %>% add_surface()
  return(p)
}

