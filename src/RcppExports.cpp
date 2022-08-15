// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// RC_C
arma::mat RC_C(arma::mat coeflistS, double U_pre, double alpha, double z);
RcppExport SEXP _QIoT_RC_C(SEXP coeflistSSEXP, SEXP U_preSEXP, SEXP alphaSEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type coeflistS(coeflistSSEXP);
    Rcpp::traits::input_parameter< double >::type U_pre(U_preSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(RC_C(coeflistS, U_pre, alpha, z));
    return rcpp_result_gen;
END_RCPP
}
// RS_C
Rcpp::List RS_C(Rcpp::NumericMatrix lst, Rcpp::IntegerVector C, Rcpp::NumericVector Aplus, Rcpp::NumericVector Aminus, Rcpp::NumericVector values);
RcppExport SEXP _QIoT_RS_C(SEXP lstSEXP, SEXP CSEXP, SEXP AplusSEXP, SEXP AminusSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type lst(lstSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type C(CSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Aplus(AplusSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type Aminus(AminusSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type values(valuesSEXP);
    rcpp_result_gen = Rcpp::wrap(RS_C(lst, C, Aplus, Aminus, values));
    return rcpp_result_gen;
END_RCPP
}
// add_C
arma::mat add_C(arma::mat L, arma::mat R, int p);
RcppExport SEXP _QIoT_add_C(SEXP LSEXP, SEXP RSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type L(LSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type R(RSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(add_C(L, R, p));
    return rcpp_result_gen;
END_RCPP
}
// LpGreedy_On_C
Rcpp::List LpGreedy_On_C(Rcpp::List coeflist, double p);
RcppExport SEXP _QIoT_LpGreedy_On_C(SEXP coeflistSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type coeflist(coeflistSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(LpGreedy_On_C(coeflist, p));
    return rcpp_result_gen;
END_RCPP
}
// DP_C
Rcpp::List DP_C(Rcpp::List coeflist, double p);
RcppExport SEXP _QIoT_DP_C(SEXP coeflistSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type coeflist(coeflistSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(DP_C(coeflist, p));
    return rcpp_result_gen;
END_RCPP
}
// rank_score
arma::vec rank_score(int n, Rcpp::List method_list);
RcppExport SEXP _QIoT_rank_score(SEXP nSEXP, SEXP method_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type method_list(method_listSEXP);
    rcpp_result_gen = Rcpp::wrap(rank_score(n, method_list));
    return rcpp_result_gen;
END_RCPP
}
// fir_rank
Rcpp::NumericVector fir_rank(Rcpp::NumericVector x);
RcppExport SEXP _QIoT_fir_rank(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(fir_rank(x));
    return rcpp_result_gen;
END_RCPP
}
// min_stat
double min_stat(arma::vec Z, arma::vec Y, int k, double c, Rcpp::List method_list, Rcpp::Nullable<Rcpp::NumericVector> score);
RcppExport SEXP _QIoT_min_stat(SEXP ZSEXP, SEXP YSEXP, SEXP kSEXP, SEXP cSEXP, SEXP method_listSEXP, SEXP scoreSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type method_list(method_listSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::NumericVector> >::type score(scoreSEXP);
    rcpp_result_gen = Rcpp::wrap(min_stat(Z, Y, k, c, method_list, score));
    return rcpp_result_gen;
END_RCPP
}
// combnCpp
arma::umat combnCpp(int n, int k);
RcppExport SEXP _QIoT_combnCpp(SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(combnCpp(n, k));
    return rcpp_result_gen;
END_RCPP
}
// null_dist
arma::vec null_dist(int n, int m, Rcpp::Nullable<Rcpp::List> method_list_, Rcpp::Nullable<Rcpp::NumericVector> score_, Rcpp::Nullable<Rcpp::NumericMatrix> Z_perm_, float null_max);
RcppExport SEXP _QIoT_null_dist(SEXP nSEXP, SEXP mSEXP, SEXP method_list_SEXP, SEXP score_SEXP, SEXP Z_perm_SEXP, SEXP null_maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::List> >::type method_list_(method_list_SEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::NumericVector> >::type score_(score_SEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::NumericMatrix> >::type Z_perm_(Z_perm_SEXP);
    Rcpp::traits::input_parameter< float >::type null_max(null_maxSEXP);
    rcpp_result_gen = Rcpp::wrap(null_dist(n, m, method_list_, score_, Z_perm_, null_max));
    return rcpp_result_gen;
END_RCPP
}
// test_stat_matrix_block
Rcpp::List test_stat_matrix_block(arma::vec Z, arma::vec Y, Rcpp::NumericVector block, double c, Rcpp::List method_list_all);
RcppExport SEXP _QIoT_test_stat_matrix_block(SEXP ZSEXP, SEXP YSEXP, SEXP blockSEXP, SEXP cSEXP, SEXP method_list_allSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type block(blockSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type method_list_all(method_list_allSEXP);
    rcpp_result_gen = Rcpp::wrap(test_stat_matrix_block(Z, Y, block, c, method_list_all));
    return rcpp_result_gen;
END_RCPP
}
// null_dist_block_C
arma::vec null_dist_block_C(arma::vec Z, Rcpp::NumericVector block, Rcpp::List method_list_all, float null_max, Rcpp::Nullable<Rcpp::NumericMatrix> Z_perm_);
RcppExport SEXP _QIoT_null_dist_block_C(SEXP ZSEXP, SEXP blockSEXP, SEXP method_list_allSEXP, SEXP null_maxSEXP, SEXP Z_perm_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type Z(ZSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type block(blockSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type method_list_all(method_list_allSEXP);
    Rcpp::traits::input_parameter< float >::type null_max(null_maxSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::NumericMatrix> >::type Z_perm_(Z_perm_SEXP);
    rcpp_result_gen = Rcpp::wrap(null_dist_block_C(Z, block, method_list_all, null_max, Z_perm_));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_QIoT_RC_C", (DL_FUNC) &_QIoT_RC_C, 4},
    {"_QIoT_RS_C", (DL_FUNC) &_QIoT_RS_C, 5},
    {"_QIoT_add_C", (DL_FUNC) &_QIoT_add_C, 3},
    {"_QIoT_LpGreedy_On_C", (DL_FUNC) &_QIoT_LpGreedy_On_C, 2},
    {"_QIoT_DP_C", (DL_FUNC) &_QIoT_DP_C, 2},
    {"_QIoT_rank_score", (DL_FUNC) &_QIoT_rank_score, 2},
    {"_QIoT_fir_rank", (DL_FUNC) &_QIoT_fir_rank, 1},
    {"_QIoT_min_stat", (DL_FUNC) &_QIoT_min_stat, 6},
    {"_QIoT_combnCpp", (DL_FUNC) &_QIoT_combnCpp, 2},
    {"_QIoT_null_dist", (DL_FUNC) &_QIoT_null_dist, 6},
    {"_QIoT_test_stat_matrix_block", (DL_FUNC) &_QIoT_test_stat_matrix_block, 5},
    {"_QIoT_null_dist_block_C", (DL_FUNC) &_QIoT_null_dist_block_C, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_QIoT(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
