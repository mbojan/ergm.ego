#' ERGM-based predicted tie probabilities for the pseudo-population network
#' 
#' @param object model fit as returned by [ergm.ego()]
#' @param ... other arguments passed to/from other methods
#' 
#' @return See [ergm::predict.ergm()]
#' 
#' @export

predict.ergm.ego <- function(object, ...) {
  # Extract network
  net <- object$network
  # Update formula with pseudo-population network
  frm <- statnet.common::nonsimp_update.formula(object$formula, net ~ .)
  assign("net", object$network, envir=environment(frm))
  # thetas without the offset(s)
  th <- ergm.eta(object$coef, object$etamap)[-1L] # drop offset(netsize.adj)
  predict(frm, theta=th, ...)
}
