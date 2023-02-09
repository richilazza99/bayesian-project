
data
{
    int I; // number of areal locations
    int T; // number of time steps
    int P; // number of covariates
    int N; // number of clusters
    
    array[I] vector[T] y; // value of interest
    array[I] matrix[T,P+1] X; // covariates matrices for each province
      
    // hyperpar vector of regressors
    vector[P+1] mu_0_mean;
    real        mu_0_var;
    real        a_sigma2_0;
    real        b_sigma2_0;
    
    // mean of w_1 (random effect for every province at time 1)
    vector[I] mu_w_1;
    
    // alpha
    real a_alpha;
    real b_alpha;
    
    // tau^2
    real a_tau2;
    real b_tau2;
    
    // sigma^2
    real a_sigma2;
    real b_sigma2;
    
    // rho 
    real rho;
    
    //xis
    real mu_xi;
    real sigma2_xi;

    // Qinv
    matrix[I,I] inv_Q;
    
    // s
    array[I] int s;
}

transformed data {
    matrix[I, I] L;
    L = cholesky_decompose(inv_Q);
    
    real sigma_xi = sqrt(sigma2_xi);
    real mu_0_sd = sqrt(mu_0_var);
    
}

parameters
{
    real<lower=0> sigma2; 
    real<lower=0> tau2;
    vector[P+1] mu_0;
    real<lower=0> sigma2_0;
    
    // autoregressive coefficient
    real xi;
    
    // betas for the mixture of the dirichlet process
    array[N] vector[P+1] betas;
    
    // for the random effect construction 
    array[T] vector[I] w_raw;
}

transformed parameters
{   
    
    // random effects tmp
    matrix[I,T]                ws_tmp;
    
    ws_tmp[1:I,1] =  mu_w_1 + L*w_raw[1];
    
    for (t in 2:T)
        ws_tmp[1:I,t] = ws_tmp[1:I,t-1]*xi + L*w_raw[t]; 
    
    matrix[T,I]   ws = (ws_tmp)'; //otherwise I have to transpose in the for loop at each iteration
    
    // stan wants std
    real sigma = sqrt(sigma2);
    real tau = sqrt(tau2);
    real sigma_0 = sqrt(sigma2_0);

}

model
{
    sigma2 ~ inv_gamma(a_sigma2,b_sigma2);
    tau2   ~ inv_gamma(a_tau2,b_tau2);
    sigma2_0 ~ inv_gamma(a_sigma2_0, b_sigma2_0);
    mu_0 ~ normal(mu_0_mean, mu_0_sd);
    
    for (t in 1:T) 
        w_raw[t] ~ normal(0, tau); 
        
    xi ~ normal(mu_xi,sigma_xi);
    
    for (n in 1:N)
        betas[n] ~ normal(mu_0, sigma_0);
    
    for (i in 1:I) {
        int clus_i=s[i];
        y[i] ~ normal(X[i]*betas[clus_i] + ws[1:T,i], sigma);
    }
}

generated quantities 
{   
    // log likelihood for each areal location 
    vector[I] log_lik;
    
    for (i in 1:I) {
        int clus_i = s[i];
        log_lik[i] = normal_lpdf(y[i] | X[i]*betas[clus_i] + ws[1:T,i], sigma);
    }
    
        
}
