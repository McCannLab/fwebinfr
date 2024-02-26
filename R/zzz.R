#' Internal
#' @noRd
#' @import patchwork
#' @importFrom utils globalVariables
#' @useDynLib fwebinfr

globalVariables(
    c("leading_ev", "time", "times", "value", "variable", "var", "val")
)
globalVariables(c("a_12", "a_21", "res_stab", "most_stable"))


scale_min_max <- function(x) {
    mx <- max(x)
    mn <- min(x)
    if (mx != mn) {
        (x - mn) / (mx - mn)
    } else {
        rep(1, length(x))
    }
}