#' Internal
#' @noRd 
#' @import patchwork
#' @importFrom utils globalVariables
#' @useDynLib fwebinfr

globalVariables(
    c("leading_ev", "time", "times", "value", "variable", "var", "val")
)
globalVariables(c("a_12", "a_21", "res_stab"))
