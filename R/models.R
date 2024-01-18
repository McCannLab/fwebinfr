#' Create models 
#' 
#' @param model model to be used.
#' @export 

fw_model <- function(model = c("lv_fr1")) {
    model <- match.arg(model)
    switch(
        model, 
        lv_fr1 = mod_lv_fr1
    )
}



## Compute model with functional response type I
mod_lv_fr1 <- function(t, y, pars) {
    return(list((pars$A %*% y + pars$R) * y))
}
