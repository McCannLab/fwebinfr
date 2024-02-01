#include <Rcpp.h>
using namespace Rcpp;

//' @name webFromNicheModel
//'
//' @title Core implementation of the niche model.
//'
//' @importFrom Rcpp evalCpp
//'
// [[Rcpp::export]]
LogicalMatrix webFromNicheModel(int nsp, double connec, NumericVector niche,
                                bool connect_all = false, bool unbias = false)
{

  LogicalMatrix metaweb(nsp, nsp);
  double c, r, rg1, rg2, beta;
  int i, j, k, l, m, count;
  //
  if (unbias)
  {
    m = 0;
  }
  else
  {
    m = 1;
  }
  //
  NumericVector niche_sorted = clone(niche);
  // Sorting the niche axis (using standard library)
  std::sort(niche_sorted.begin(), niche_sorted.end());
  //
  beta = .5 / connec - 1;
  count = 0;
  k = 0;
  while (k == 0)
  {
    if (count > 100000)
    {
      stop("100,000 unsuccessful attempts.");
    }
    // if unbias, then the first species is a basal species
    for (i = m; i < nsp; i++)
    {
      r = rbeta(1, 1, beta)[0] * niche_sorted[i];
      c = runif(1, .5 * r, niche_sorted[i])[0];
      rg1 = c - .5 * r;
      rg2 = c + .5 * r;
      l = 0;
      for (j = 0; j < nsp; j++)
      {
        metaweb(i, j) = FALSE;
        if ((niche_sorted[j] > rg1) && (niche_sorted[j] < rg2))
        {
          metaweb(i, j) = TRUE;
          l++;
        }
      }
      // If a species has no interaction, we break the loop if required
      if (!l && connect_all)
      {
        k--;
        break;
      }
    }
    count++;
    k++;
  }
  //
  return metaweb;
}
