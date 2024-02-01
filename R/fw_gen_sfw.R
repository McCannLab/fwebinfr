#' Helper to generate structured model foodwebs
#' 
#' This function is an implementation of the niche model (Williams & Martinez,
#' 2000) which generates food webs based on three elements: 
#'   - a number of species,
#'   - a niche axis and
#'   - a value of connectance.
#'
#' @param nsp An integer giving the number of species considered.
#' @param connec A real positive between 0 and .5 indicating the connectance
#' of the network to be generated.
#' @param connect_all Logical. If `TRUE`, then all species in the network have a
#' least one prey (but the niche with the lowest niche value).
#' @param unbias Logical. If `TRUE`, then the first species may not be a basal 
#' species.
#' @param niche A vector real positive between 0 and 1 standing for the niche 
#' axis. Default is set to `NULL`, in such case the niche axis is automatically 
#' generated.
#'
#' @return 
#' A logical matrix describing pairwise interactions. A given line
#' describes the diet of a given species while a column describes the set of
#' predator associated to a particular species.
#'
#' @details
#' Three remarks. First, according to Williams and Martinez (2000),
#' the species with the lowest niche value is considered as a basal species and
#' therefore cannot feed upon another species. This introduces a slight bias 
#' (\emph{e.g} the expected connectance is lower than the expected values 
#' `connectance`). Second, forcing all the species to be connected introduces 
#' another bias (on connectance values) as they tends to be more connected than 
#' expected. Third, if with custom niche axes, the expected connectance may not
#' be the one expected (`connectance`) as niche values distribution will likely 
#' differs from the uniform distribution used in Williams and Martinez (2000).
#'
#' @references
#' Williams, R.J. and Martinez, N.D. (2000) Simple rules yield complex food 
#' webs. \emph{Nature}, 404:180–183.
#' 
#' @export 
#' @examples
#' fw_niche_model(10, 0.2)

fw_niche_model <- function(nsp, connectance, connect_all = FALSE,
                           unbias = FALSE, niche = NULL) {
    stopifnot(connectance > 0 && connectance < 0.5)
    if (is.null(niche)) {
        niche <- runif(nsp, 0, 1)
    } else {
        cli::cli_alert_info("custom `niche` used, connectance may be biased")
        stopifnot(all(niche > 0 & niche < 1))
        stopifnot(length(niche) == nsp)
    }
    webFromNicheModel(nsp, connectance, niche, connect_all, unbias)
}