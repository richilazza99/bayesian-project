
data
{
    int I; // number of areal locations
    int T; // number of time steps
    int P; // number of covariates
    
    array[I] vector[T] y; // value of interest
    array[I] matrix[T,P+1] X; // covariates matrices for each province
      
    // hyperpar vector of regressors
    vector[P+1] mu_0; 
    real        sigma_0;
    
    // mean of w_1 (random effect for every province at time 1)
    vector[I] mu_w_1;
    
    // tau^2
    real a_tau2;
    real b_tau2;
    
    // sigma^2
    real a_sigma2;
    real b_sigma2;
    
    // proximity matrix
    matrix[I,I] W_raw;
    
}

transformed data
{
    vector[T] ones_T;
    for (t in 1:T)
        ones_T[t] = 1;
    
    matrix[T,T] eye_T = diag_matrix(ones_T);
 
    vector[I] ones_I;
    for (i in 1:I)
        ones_I[i] = 1;
    
    matrix[I,I] eye_I;
    eye_I = diag_matrix(ones_I);
        
    matrix[I,I] W;
    W = diag_matrix(W_raw*ones_I) - W_raw;
}

parameters
{
    real<lower=0> sigma2; 
    real<lower=0> tau2; 
    real<lower=0,upper=1> rho_t;
    real<lower=0,upper=1> rho_s;
    
    // beta
    vector[P+1] beta;

    // random effects tmp
    matrix[I,T]  ws_tmp;
    
}

transformed parameters
{   
    // Stan wants std
    real sigma = sqrt(sigma2);
    
    matrix[I,I] inv_Q = inverse_spd(rho_s*W + (1-rho_s)*eye_I);
    
    matrix[T,I]   ws = (ws_tmp)'; //otherwise I have to transpose in the for loop at each iteration
}

model
{
    sigma2 ~ inv_gamma(a_sigma2,b_sigma2);
    tau2   ~ inv_gamma(a_tau2,b_tau2); 
    rho_t  ~ uniform(0,1);
    rho_s  ~ uniform(0,1);
    
    ws_tmp[1:I,1] ~ multi_normal(mu_w_1, tau2*inv_Q);
    
    for (t in 2:T)
        ws_tmp[1:I,t] ~ multi_normal(rho_t*ws_tmp[1:I,t-1], tau2*inv_Q);
    
    beta ~ normal(mu_0, sigma_0);
    
    for (i in 1:I) {
        
        y[i] ~ normal(X[i]*beta + ws[1:T,i], sigma);
    
    }
}

generated quantities 
{   
    // log likelihood for each areal location 
    vector[I] log_lik;
    
    for (i in 1:I) 
    {
        log_lik[i] = normal_lpdf(y[i] | X[i]*beta + ws[1:T,i], sigma);
    }
    
}
