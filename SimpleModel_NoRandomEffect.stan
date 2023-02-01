
data
{
    int I; // number of areal locations
    int T; // number of time steps
    int P; // number of covariates
    int H; // truncation of stick breaking construction dp
    
    vector[I*T]     y; // output values
    matrix[I*T,P+1] X; // covariate matrix
    // syntax: y(i,t) = y[T*(i-1) + t]
    
    //To build sigma2
    real<lower=0> a_sigma2;
    real<lower=0> b_sigma2;
    
    // To build P_zero of dirchlet process
    vector[P+1] mu_0;
    matrix[P+1,P+1] Sigma_0;
    
    //to build alpha of dp
    real<lower=0> a_alpha;
    real<lower=0> b_alpha;
    
    int<lower=1> ngrid;
    vector[ngrid] xgrid;
    
}
parameters{
    real<lower=0> alpha;
    real<lower=0> sigma2;
    matrix[P+1,H] betas;
    vector<lower=0,upper=1>[H-1] vs;       // to build mixture Dirichlet process
}
transformed parameters
{
    simplex[H] omegas;    // wights of sbc
    
    vector[H-1] cumprod_one_mv;    
    cumprod_one_mv = exp(cumulative_sum(log1m(vs)));
    
    omegas[1] = vs[1];
    omegas[2:(H-1)] = vs[2:(H-1)] .* cumprod_one_mv[1:(H-2)];
    omegas[H] = cumprod_one_mv[H-1];
}
model
{
    alpha ~ gamma(a_alpha,b_alpha);
    sigma2 ~ inv_gamma(a_sigma2,b_sigma2);
    vs ~ beta(1,alpha);
    
    for (h in 1:H)
        betas[1:(P+1),h] ~ multi_normal(mu_0, Sigma_0);
        
    for (i in 1:I) {
    
        vector[H] log_probs;
        
        for (h in 1:H) 
            log_probs[h] = log(omegas[h]) + normal_lpdf(y[(T*(i-1)+1):i*T] | X[(T*(i-1)+1):(i*T), 1:(P+1)]*betas[1:(P+1),h] , sigma2);
        
        target += log_sum_exp(log_probs);
    }
}
generated quantities 
{   
    // vector of cluster allocations
    vector[I] s;
    
    matrix[I,H] log_probs;
    for (i in 1:I) 
    {
        for (h in 1:H) 
            log_probs[i,h] = log(omegas[h]) + 
            normal_lpdf(y[(T*(i-1)+1):(i*T)] | X[(T*(i-1)+1):(i*T), 1:(P+1)]*betas[1:(P+1),h], sigma2);
    
    }
    for (i in 1:I)
        s[i] = categorical_rng(softmax(log_probs[i,1:H]'));

    // log_lik for goodness of fit 
    vector[I] log_lik;
    for (i in 1:I)
      log_lik[i] = log_sum_exp(log_probs[i,1:H]);
    
            
}
