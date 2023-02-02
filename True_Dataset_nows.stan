
data
{
    int I; // number of areal locations
    int T; // number of time steps
    int P; // number of covariates
    int H; // truncation of stick breaking construction dp
    
    array[I] vector[T] y; // value of interest
    array[I] matrix[T,P+1] X; // covariates matrices for each province
    
    // prior mean for mu0
    vector[P+1] eta_0;
    
    // prior variance for mu0
    real tau_0;
    
    // hyperparam sigma20
    real a_0;
    real b_0;
    
    // alpha
    real alpha;
    
    // sigma^2
    real a_sigma2;
    real b_sigma2;
}

parameters
{
    real<lower=0> sigma2; 
    real<lower=0> sigma20;  
    
    vector[P+1] mu_0;
    
    // betas for the mixture of the dirichlet process
    array[H] vector[P+1] betas; 

    // for the construction of the dirichlet process
    vector<lower=0,upper=1>[H-1] vs;
    
}

transformed parameters
{   
    // weights stick breaking construction
    simplex[H] omegas; 
    
    // sbc stuff
    vector[H-1] cumprod_one_mv;    
    cumprod_one_mv = exp(cumulative_sum(log1m(vs)));
    
    omegas[1] = vs[1];
    omegas[2:(H-1)] = vs[2:(H-1)] .* cumprod_one_mv[1:(H-2)];
    omegas[H] = cumprod_one_mv[H-1];
    
    // Stan wants std
    real sigma = sqrt(sigma2);
    real sigma_0 = sqrt(sigma20);
    
}

model
{
    sigma2 ~ inv_gamma(a_sigma2,b_sigma2);
    vs     ~ beta(1, alpha); 
    mu_0  ~ normal(eta_0, tau_0);
    sigma20 ~ gamma(a_0,b_0);
    
    
    for (h in 1:H)
        betas[h] ~ normal(mu_0, sigma_0);
    
    vector[H] log_probs;
    
    for (i in 1:I) {
        
        for (h in 1:H) 
    
            log_probs[h] = log(omegas[h]) + normal_lpdf(y[i] | X[i]*betas[h], sigma);
        
        target += log_sum_exp(log_probs);
    }
}

generated quantities 
{   
    // vector of cluster allocations
    vector[I] s;
    
    // log likelihood for each areal location 
    vector[I] log_lik;
    
    array[I] vector[H] log_probs;
    
    for (i in 1:I) 
    {
        for (h in 1:H) 
            log_probs[i,h] = log(omegas[h]) + normal_lpdf(y[i] | X[i]*betas[h], sigma);
        
        s[i] = categorical_rng(softmax(log_probs[i]));
        
        log_lik[i] = log_sum_exp(log_probs[i]);
    }
    
        
}
