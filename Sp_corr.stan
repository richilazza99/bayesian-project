
data
{
    int I; // number of areal locations
    int T; // number of time steps
    int P; // number of covariates
    int H; // truncation of stick breaking construction dp
    
    vector[T] y[I]; // for each prov the asnwer at all the time
    matrix[T,P+1] X[I]; // for each province its covariance matrix
      
    // hyperpar vector of regressors
    vector[P+1] mu_0; 
    real        sigma_0;
    
    // w_1
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
    real a_xi;
    real b_xi;

    // Qinv
    matrix[I,I] inv_Q;
}

transformed data {
    matrix[I, I] L;
    L = cholesky_decompose(inv_Q);
}

parameters
{
    real<lower=0> alpha;
    real<lower=0> sigma2; 
    real<lower=0> tau2;  
    
    // autoregressive coefficients 
    real<lower=0,upper=1> xi_constructor;
    
    // betas for the mixture of the dirichlet process
    matrix[P+1,H]  betas;

    
    // for the construction of the dirichlet process
    vector<lower=0,upper=1>[H-1] vs;
    
    // for the random effect construction 
    vector[I] alpha_w;
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
    
    real xi = 2*xi_constructor-1;
    
    // random effects
    matrix[I,T]                ws_tmp;
    
    ws_tmp[1:I,1] =  mu_w_1 + L*alpha_w;
    
    for (t in 2:T)
        ws_tmp[1:I,t] = ws_tmp[1:I,t-1]*xi + L*alpha_w; 
    
    matrix[T,I]   ws = (ws_tmp)'; //otherwise I have to transpose in the for loop at each iteration
    
    real sigma = sqrt(sigma2);
    
    real tau = sqrt(tau2);
    
    matrix[T,I]  means[H];

    for (i in 1:I) {
        for (h in 1:H) 
            means[h,1:T,i] = X[i]*betas[1:(P+1),h] + ws[1:T,i];
    }

}

model
{
    alpha  ~ gamma(a_alpha,b_alpha);
    sigma2 ~ inv_gamma(a_sigma2,b_sigma2);
    tau2   ~ inv_gamma(a_tau2,b_tau2);
    vs     ~ beta(1,alpha);
    alpha_w ~ normal(0, tau);
    xi_constructor ~ beta(a_xi,b_xi);
    
    for (h in 1:H)
        betas[1:(P+1),h] ~ normal(mu_0, sigma_0);
    
    vector[H] log_probs;
    for (i in 1:I) {
        
        for (h in 1:H) 
            log_probs[h] = log(omegas[h]) + 
            normal_lpdf(y[i] | means[h,1:T,i], sigma);
        
        target += log_sum_exp(log_probs);
    }
}

generated quantities 
{   
    // vector of cluster allocations
    vector[I] s;
    
    matrix[H,I] log_probs; 
    for (i in 1:I) 
    {
        for (h in 1:H) 
            log_probs[h,i] = log(omegas[h]) + 
            normal_lpdf(y[i] | means[h,1:T,i], sigma);
    
    }
    for (i in 1:I)
        s[i] = categorical_rng(softmax(log_probs[1:H,i]));
        
        
    // log likelihood
    vector[I] log_lik;
    for (i in 1:I)
      log_lik[i] = log_sum_exp(log_probs[1:H,i]);
    

    
}
