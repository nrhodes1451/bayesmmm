functions {
  // function that is comparable to R's %in% function
  // pos is the value to test for matches
  // array pos_var is the 1-dimensional array of possible matches
  // returns pos_match=1 if pos matches at least one element of pos_var and pos_match=0 otherwise
  // example code:
  // if(r_in(3,{1,2,3,4})) will evaluate as TRUE
  int r_in(int pos,int[] pos_var) {
    int pos_match;
    int all_matches[size(pos_var)];
    for (p in 1:(size(pos_var))) {
      all_matches[p] = (pos_var[p]==pos);
    }
    if(sum(all_matches)>0) {
      pos_match = 1;
      return pos_match;
    } else {
      pos_match = 0;
      return pos_match;
    }
  }
  // returns the index of an element in an integer array
  // e.g. get_index(2, [1,4,3,2,5]) = 4
  int get_index(int needle, int[] haystack){
    for(i in 1:size(haystack)){
      if(needle==haystack[i]){
        return i;
      }
    }
    return 0;
  }
}
data {
  int<lower=1> num_pools; // the number of pools
  int<lower=1> num_vars; // the number of variables
  int<lower=1> total_obs; // the total number of observations
  int<lower=1> model_obs; // the number of modelled observations

  int<lower=1> obs_ix[model_obs*num_pools]; // a vector of model observation indices
  int<lower=1> pool_ix[model_obs*num_pools]; // a vector of pool indices
  vector[model_obs*num_pools] y; // dependent variable vector

  // scaling factors for relative coefficient weights for variables by pool
  matrix[num_vars, num_pools] pool_scale;

  row_vector[total_obs*num_pools] X[num_vars]; // 2d vector of independent variables

  // Indices of variable groups
  int num_vars_flo; // count of floating variables
  int flo_ix[num_vars_flo]; // Floating variable indices
  int num_vars_fix; // count of fixed variables
  int fix_ix[num_vars_fix]; // Fixed variable indices

  int num_vars_med; // count of transformed media variables
  int med_ix[num_vars_med]; // Transformed media variable indices

  // Transformation priors
  vector[2] adstock_range; // Adstock lower and upper bounds

  // Prior means and standard deviations
  vector[num_vars] pmeans;
  vector[num_vars] psds;
}
parameters{
  vector[num_pools] a; // the intercepts
  real<lower=0> sigma; // residual variance

  // Top-level coefficients
  vector[num_vars_flo] global_beta_flo;
  vector<lower=0>[num_vars_fix] global_beta_fix;

  // Pool-level coefficients
  row_vector[num_pools] pool_beta_flo[num_vars_flo];
  row_vector<lower=0>[num_pools] pool_beta_fix[num_vars_fix];

  vector<lower=adstock_range[1], upper=adstock_range[2]>[num_vars_med] ads_tra; // the adstocks for media variables
  vector<lower=0.25, upper=2>[num_vars_med] den_tra; // the denominators for media variables
}
transformed parameters {
  vector[model_obs*num_pools] mu;

  // Final coefficients
  vector[num_vars] global_beta;
  row_vector[num_pools] pool_beta[num_vars];

  row_vector[total_obs*num_pools] X_tra[num_vars]; // transformed independent variables
  X_tra = X;

  for (v in 1:num_vars){
    // Populate final coefficients
    if(r_in(v, flo_ix)){
      global_beta[v] = global_beta_flo[get_index(v, flo_ix)];
      pool_beta[v, ] = pool_beta_flo[get_index(v, flo_ix), ];
    }
    else{
      global_beta[v] = global_beta_fix[get_index(v, fix_ix)];
      pool_beta[v, ] = pool_beta_fix[get_index(v, fix_ix), ];
    }

    // If variable "v" is transformed:
    if(r_in(v, med_ix)){
      // Transform media variables
      // Create adstocks
      for(n in 1:total_obs){
        // Transform
        for(p in 1:num_pools){
          if(n>1){
            X_tra[v, n+(p-1)*total_obs] = X_tra[v, n+(p-1)*total_obs] +
              X_tra[v, n+(p-1)*total_obs-1] * ads_tra[get_index(v, med_ix)];
          }
          X_tra[v, n+(p-1)*total_obs] = 1 - exp(-X_tra[v, n+(p-1)*total_obs] / den_tra[get_index(v, med_ix)]);
        }
      }
    }
  }

  // calculate mean estimate
  for (n in 1:(model_obs*num_pools)){
    mu[n] = a[pool_ix[n]] + dot_product(pool_beta[, pool_ix[n]], X_tra[, obs_ix[n]]);
  }
}
model{
  // Intercepts
  for (p in 1:num_pools){
    a[p] ~ normal(0, 1);
  }
  sigma ~ cauchy(0, 1);
  ads_tra ~ normal(0.5, 0.2);
  den_tra ~ normal(1, 1);

  // Coefficient hierarchy
  for (v in 1:num_vars) {
    global_beta[v] ~ normal(pmeans[v], psds[v]);
    for (p in 1:num_pools) {
      pool_beta[v, p] ~ normal(global_beta[v] / pool_scale[v, p], psds[v] / 4);
    }
  }

  for(n in 1:model_obs*num_pools){
    y[n] ~ normal(mu[n], sigma);
  }
}
