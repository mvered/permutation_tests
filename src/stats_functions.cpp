#include <Rcpp.h>
#include <algorithm> // for std::shuffle
#include <random>    // for std::default_random_engine
#include <vector>

using namespace Rcpp;

// The core U-statistic logic
// [[Rcpp::export]]
double multinomial_u_cpp(CharacterVector sample1, CharacterVector sample2) {
  int n1 = sample1.size();
  int n2 = sample2.size();
  double running_sum = 0;
  
  std::vector<std::string> s1 = as<std::vector<std::string>>(sample1);
  std::vector<std::string> s2 = as<std::vector<std::string>>(sample2);
  
  for (int i = 0; i < (n1 - 1); ++i) {
    for (int j = i + 1; j < n1; ++j) {
      for (int a = 0; a < (n2 - 1); ++a) {
        for (int b = a + 1; b < n2; ++b) {
          int g_yy   = (s1[i] == s1[j]) ? 1 : 0;
          int g_zz   = (s2[a] == s2[b]) ? 1 : 0;
          int g_yz1  = (s1[i] == s2[b]) ? 1 : 0;
          int g_yz2  = (s1[j] == s2[a]) ? 1 : 0;
          running_sum += (g_yy + g_zz - g_yz1 - g_yz2);
        }
      }
    }
  }
  
  double frac = 1.0 / (double(n1) * (n1 - 1.0) * double(n2) * (n2 - 1.0));
  return frac * running_sum;
}

// The Permutation Wrapper: Moves the R 'for' loop into C++
// [[Rcpp::export]]
NumericVector get_perm_dist_cpp(CharacterVector pooled, int n1, int n_perms) {
  NumericVector dist(n_perms);
  int n_total = pooled.size();
  int n2 = n_total - n1;
  
  for (int p = 0; p < n_perms; ++p) {
    // Shuffle the pooled data
    CharacterVector shuffled = clone(pooled);
    std::random_shuffle(shuffled.begin(), shuffled.end());
    
    // Split into two groups
    CharacterVector group1(n1);
    CharacterVector group2(n2);
    
    for(int i = 0; i < n1; i++) group1[i] = shuffled[i];
    for(int j = 0; j < n2; j++) group2[j] = shuffled[n1 + j];
    
    dist[p] = multinomial_u_cpp(group1, group2);
  }
  return dist;
}

// Pure C++ function to calculate average ranks (no R callback)
NumericVector fast_rank(NumericVector x) {
  int n = x.size();
  NumericVector ranks(n);
  std::vector<int> index(n);
  std::iota(index.begin(), index.end(), 0);
  
  // Sort indices based on values in x
  std::sort(index.begin(), index.end(), [&](int i, int j) {
    return x[i] < x[j];
  });
  
  for (int i = 0; i < n; ) {
    int j = i + 1;
    // Find range of tied values
    while (j < n && x[index[j]] == x[index[i]]) {
      j++;
    }
    // Calculate average rank for the tie group
    double avg_rank = (i + j + 1) / 2.0; 
    for (int k = i; k < j; k++) {
      ranks[index[k]] = avg_rank;
    }
    i = j;
  }
  return ranks;
}

// Helper to calculate Mann-Whitney U for a single split
// [[Rcpp::export]]
double mw_u_logic(NumericVector pooled, int n_x) {
  int n_total = pooled.size();
  int n_y = n_total - n_x;
  
  // Calculate ranks
  NumericVector ranks = fast_rank(pooled); // Rcpp's equivalent to R's rank()
  
  double rank_sum_x = 0;
  for(int i = 0; i < n_x; ++i) {
    rank_sum_x += ranks[i];
  }
  
  // Standard Mann-Whitney U formula
  return (double)n_x * n_y + ((double)n_x * (n_x + 1.0) / 2.0) - rank_sum_x;
}

// Full Permutation Wrapper
// [[Rcpp::export]]
NumericVector mw_perm_dist_cpp(NumericVector pooled, int n_x, int n_perms) {
  NumericVector dist(n_perms);
  NumericVector shuffled = clone(pooled);
  
  for (int p = 0; p < n_perms; ++p) {
    // Use Rcpp's sample to keep seed consistency
    shuffled = sample(shuffled, shuffled.size(), false);
    dist[p] = mw_u_logic(shuffled, n_x);
  }
  return dist;
}