// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// webFromNicheModel
LogicalMatrix webFromNicheModel(int nsp, double connec, NumericVector niche, bool connect_all, bool unbias);
RcppExport SEXP _fwebinfr_webFromNicheModel(SEXP nspSEXP, SEXP connecSEXP, SEXP nicheSEXP, SEXP connect_allSEXP, SEXP unbiasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nsp(nspSEXP);
    Rcpp::traits::input_parameter< double >::type connec(connecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type niche(nicheSEXP);
    Rcpp::traits::input_parameter< bool >::type connect_all(connect_allSEXP);
    Rcpp::traits::input_parameter< bool >::type unbias(unbiasSEXP);
    rcpp_result_gen = Rcpp::wrap(webFromNicheModel(nsp, connec, niche, connect_all, unbias));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fwebinfr_webFromNicheModel", (DL_FUNC) &_fwebinfr_webFromNicheModel, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_fwebinfr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}