#'@title Run simulation
#'@name Run_simulation
#'@author Marcos Alves \email{mppalves@gmail.com}
#'@description Function to take as input a testing formula, LSUs and predictors
#'  and output staticts and plots that describe how good the formula is
#'  simmulating the original data.
#'@usage Run_simulation(func, w, x, input_data, title, cor ="red", comment = NULL)
#'@param func R formula to be evaluated
#'@param w LSU values
#'@param x secondary parameter (preciptation, radiation, temperature or mean
#'  harvest for example)
#'@param input_data data frame with three columns: x variable, LSU, y output
#'@param title String used to name the plots and the exported files
#'@param cor hexadecimal or color name used to color details in plots
#'@param comment Comment useful to copy the formula being analysed
#'
#'
#'@examples
#'
#'Run_simulation(func = y ~ 8.76366*exp(-0.0163042*x + (-1.56578 + 0.0103395*x)*w)*x*w,
#'               w=input_data$w,
#'               x=input_data$x,
#'               input_data = input_data,
#'               title = "Formula Evaluation Harvest, Yield mean",
#'               cor = "#217A00",
#'               comment = "y ~ 8.76366*exp(-0.0163042*x + (-1.56578 + 0.0103395*x)*w)*x*w")
#'@import ggplot2
#'@import stringr
#'@export

#tranforming formulas in functions
as.function <- function(formula, w, x) {
  cmd <- tail(as.character(formula),1)
  exp <- parse(text=cmd)
  function(w,x) eval(exp, list(w=w,x=x))
}

evaluate_expression <- function(func,w, x){

  func = as.function(func,w,x)
  y = do.call(func,list(w=w,x=x))
  return(y)
}


Run_simulation <- function(func, w, x, input_data, title, cor ="red", comment = NULL) {

  #pre-processing data
  colnames(input_data) = c("x","w","y")

  #simulating y
  y = evaluate_expression(func, input_data$w, input_data$x)

  #combining the results in a dataframe and constructing a matrix to be ploted
  combineded_output = data.frame(x = input_data$x, w=input_data$w, y)

  output_data = combineded_output$y
  input_data = input_data$y

  #running statistical analysis on the results
  Run_analysis(output_data,input_data, title,cor, comment)
}


#'@name Run_analysis
#'@title Run Analysis
#'@author Marcos Alves \email{mppalves@gmail.com}
#'@description Run Analysis take as input two sets of values (input and output
#'  data) and run a series of analysis outputing statistics and graphs to check
#'  how well the two datasets fit each other.
#'@usage Run_analysis(output_data, input_data, title, cor, comment = NULL)
#'@param output_data Model predictions
#'@param input_data Original data
#'@param title String used to name the plots and the exported files
#'@param cor Hexadecimal or color name used to color details in plots
#'@param comment Comment to printed in the .txt statistical file results
#'
#'@examples Run_analysis(output_data,input_data,title = "Model Evaluation harvest",cor = "green")
#'@export

Run_analysis <- function(output_data, input_data, title, cor, comment = NULL){

  #comparing results
  ttest = t.test(input_data, output_data, paired =F, var.equal = T)
  correlation = cor(input_data, output_data)
  Residuals = output_data-input_data
  variances = var.test(output_data,input_data)

  # par(mfrow =c(3,1))
  # hist(input_data, xlim = c(0,max(input_data)), main = paste0("Data input "), breaks = 50)
  # hist(output_data, xlim = c(0,max(input_data)), main = paste0("Simulation output"), breaks = 50)
  # hist(residuals, main = "Residual distribution")
  # par(mfrow =c(1,1))
  #
  # print(plot(output_data,input_data, pch='.', main = paste0("Correlation between real and predicted output for", title) , xlab = "Predicted output", ylab = "real output", col=rgb(0,0,0,alpha=0.1)))
  # dev.copy(jpeg,paste0(title,".jpeg"))
  # dev.off()

  # Overlaid histograms
  a = data.frame(input_data)
  b = data.frame(output_data)
  a$Distribution = "Original"
  b$Distribution = "Predicted"
  colnames(a)[1] = "Output"
  colnames(b)[1] = "Output"
  dat = rbind(a,b)

  size=max(input_data)*0.02

  histo1 = ggplot(dat, aes(x=Output, fill= Distribution)) +
    geom_histogram(binwidth=size, alpha=.5, colour="#595959", position = 'identity') +
    #facet_grid(Distribution ~ .) +
    theme(text = element_text(size = 18)) +
    #coord_cartesian(xlim = c(0,125000)) +
    xlab("Output") +
    ylab("Density")

  print(histo1)

  ggsave(paste0(title,"_histogram_", str_remove(str_remove(Sys.time(),":"),":"), ".pdf"),
         width = 22,
         height = 15,
         units = "cm",
         limitsize = TRUE)

  histo2 = ggplot(as.data.frame(Residuals), aes(x=Residuals)) +
    geom_histogram(binwidth=size, alpha=.5, colour="#595959") +
    theme(text = element_text(size = 18)) +
    xlab("Residuals") +
    ylab("Density")

  print(histo2)

  ggsave(paste0(title,"_residuals_", str_remove(str_remove(Sys.time(),":"),":"), ".pdf"),
         width = 15,
         height = 15,
         units = "cm",
         limitsize = TRUE)



  #plotting with ggplot and adding a density function
  ggplot_dataset = data.frame(input_data, output_data)
  scatter = ggplot(data = ggplot_dataset,aes(x=input_data ,y=output_data)) +
    geom_point( size = 0.1, stroke = 0, shape = 16, alpha = 0.3) +
    ylab("Predicted output (gC/m2)") +
    xlab("Original output (gC/m2)") +
    ggtitle(paste0(title), subtitle = "Output correlation between Orginal and Predicted") +
    geom_density_2d(alpha = 0.7, colour = cor) +
    geom_abline(intercept = 0, slope = 1, linetype="dashed") +
    #coord_cartesian(xlim = c(0,125000), ylim = c(0,125000)) +
    theme(text = element_text(size = 18))


  print(scatter)

  ggsave(paste0(title,"_dispersion_", str_remove(str_remove(Sys.time(),":"),":"), ".pdf"),
         width = 15,
         height = 15,
         units = "cm",
         limitsize = F)



  if(!is.null(comment)){
    x = list("t test" = ttest, "correlation" = correlation, "Variances" = variances, "function" = comment)
    y = capture.output(x)
    writeLines(y, con = file(paste0(title,"_statistical_analysis",str_remove(str_remove(Sys.time(),":"),":"),".txt")))
    return(x)
  }else{
    x = list("t test" = ttest, "correlation" = correlation, "Variances" = variances)
    y = capture.output(x)
    writeLines(y, con = file(paste0(title,"_statistical_analysis_",str_remove(str_remove(Sys.time(),":"),":"),".txt")))

    return(x)
  }

}
