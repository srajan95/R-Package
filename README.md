# R-Package
A new library is created using R, with the name Climr

A new method for the class “climr” called gp_fit. This function takes in input the class object and a string argument,
which should be one of c(“Nelder-Mead”, “BFGS”, “SANN”, “Brent”). The method gp_fit is an adaptation of the function regression_fit
which  and optimises the hyperparameters of the Gaussian process using optim with the optimisation method given by the string argument above.
The method returns an object of class climr_gp_fit. 

A plot method for the class climr_gp_fit. Similarly to the plot method for a climr_fit object, this method will use ggplot2 to 
illustrate the data as a scatter- plot, and will add the smoothed Guassian process regression line. 
