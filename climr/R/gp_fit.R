#' Fit basic statistical models to climate data
#'
#' @param obj An object of class \code{climr} from \code{\link{load_clim}}
#' @param method By default optim performs minimization and requires quasi-Newton method (\code{BFGS}) or SANN method  (\code{SANN}) or Brent method (\code{Brent}) , or Nelder-Mead (\code{Nelder-Mead})
#'
#' @return Returns a list of class \code{climr_gp_fit} which includes the model details as well as the data set and fit type used
#' @seealso \code{\link{load_clim}}, \code{\link{plot.climr_gp_fit}}
#' @export
#' @importFrom magrittr "%$%"
#' @importFrom mvtnorm "dmvnorm" 
#' @importFrom stats "optim" "sd" 
#' 
#' @examples
#' data1 = load_clim('NH')
#' data2 = gp_fit(data1)

gp_fit <- function(obj, method = c("BFGS","Nelder-Mead","SANN","Brent")) {
  UseMethod('gp_fit')
}

gp_fit.climr<-function(obj,method=c("BFGS","Nelder-Mead","SANN","Brent"))
{
  # Create global variables to avoid annoying CRAN notes
  DJF = Dec = `J-D` = Jan = SON = Year = month = pred = quarter = temp = x = year =pred_gp=sdt=meant=out1= NULL
  #arguement is taken from the set of arguements
  method_arg<-match.arg(method)
  #from the data temp is extracted
  y<-scale(obj$clim_year %$% temp)
  #since the exctracted data was in tibble, this was used to avoid discrepency in the dmvnorm
  y<-y[,]
  #Year is extracted from the data
  x<-obj$clim_year %$% year
  x_g<-pretty(x, n = 100)
  
  p=1
  p1=rep(0, 3)
  
  gp_criterion = function(p1,x,y) {
    sig_sq = exp(p1[1])
    rho_sq = exp(p1[2])
    tau_sq = exp(p1[3])
    Mu = rep(0, length(y))
    Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))
    ll = dmvnorm(y, Mu, Sigma, log = TRUE)
    return(-ll)
  }
  
  # Create design matrices
  x_rep = matrix(rep(x, p+1), ncol = (p+1), nrow = length(x))
  X = sweep(x_rep, 2, 0:p, '^')
  X_g_rep = matrix(rep(x_g, p+1), ncol = (p+1), nrow = length(x_g))
  X_g = sweep(X_g_rep, 2, 0:p, '^')
  # Calculate predicted values
  pred_lm  = X_g %*% solve(t(X)%*%X, t(X)%*%y)
  
  # based on the method optim function is used
  if(method_arg=="BFGS")
  {
    optim_res = optim(rep(0, 3), gp_criterion, x = x, y = y, method = "BFGS")
  }
  else if(method_arg=="Nelder-Mead")
  {
    optim_res = optim(rep(0, 3), gp_criterion, x = x, y = y, method = "Nelder-Mead")
  }
  else if(method_arg=="SANN"){
    optim_res = optim(rep(0, 3), gp_criterion, x = x, y = y, method = "SANN")
  }
  else
  {
    optim_res = optim(rep(0, 3), gp_criterion, x = x, y = y, method = method_arg)
  }
  
  
  # Extract the results
  sig_sq = exp(optim_res$par[1])
  rho_sq = exp(optim_res$par[2])
  tau_sq = exp(optim_res$par[3])
  
  # Create covariance matrices
  C = sig_sq * exp( - rho_sq * outer(x_g, x, '-')^2 )
  Sigma = sig_sq * exp( - rho_sq * outer(x, x, '-')^2 ) + tau_sq * diag(length(x))
  
  # Create predictions
  pred_gp = C %*% solve(Sigma, y)
  
  #rescaling of the predicted paramters are done
  sdt<- sd(obj$clim_year$temp)
  meant<- mean(obj$clim_year$temp)
  pred_gp = (pred_gp)*sdt
  pred_gp = pred_gp + meant
  out1<-list(pred=pred_gp,data=obj$clim_year)
  class(out1) <- 'climr_gp_fit'
  return(out1)
  
}
