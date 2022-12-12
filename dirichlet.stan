
data {
    int<lower=1> I; // number of areal locations
    int<lower=1> T; // number of time steps
    
    matrix[I,T] y; // output values 
    
    int<lower=1> p; // number of covariates
    
    // P_0 (gaussian part)
    vector[p+1]     mu_0;    
    matrix[p+1,p+1] Sigma_0; 
    
    // W
    vector[I]   mu_w_1;
    matrix[I,I] inv_Q_w; // rho*(diag(W*I)-W) + (1-rho) 
    
    // alpha
    real<lower=0> a_alpha;
    real<lower=0> b_alpha;
    
    // tau^2
    real<lower=0> a_tau2;
    real<lower=0> b_tau2;
    
    // sigma^2
    real<lower=0> a_sigma2;
    real<lower=0> b_sigma2;
    
    // xi
    real<lower=0> a_xi;
    real<lower=0> b_xi;
    
    // rho
    // real<lower=0> alpha_rho;
    // real<lower=0> beta_rho;
    
    int<lower=1> H; // truncation of stick breaking construction dp
    
    int<lower=1>  ngrid; // plot stuff 
    vector[ngrid] xgrid;
    
    matrix[I*T,p+1] X; // covariate matrix
}

parameters {
    real<lower=0> alpha;                     // mass parameter of dp
    matrix[p+1,H] betas;                     // regressors
    matrix<lower=-1,upper=1>[I,H]   xis;     // autoregressive coefficients
    matrix[I,T]   ws;                        // random effects
    real<lower=0> sigma2;                    // output variance
    real<lower=0> tau2;
    real<lower=0> rho;
    
    vector<lower=0,upper=1>[H-1] vs; // needed to construct omegas
}

transformed parameters {
    real<lower=0> sigma = sqrt(sigma2); // output sd
    
    simplex[H] omegas; // weights of sbc
    
    // construct omegas
    vector<lower=0, upper=1>[H-1] cumprod_one_mv;    
    cumprod_one_mv = exp(cumulative_sum(log1m(vs)));
    
    omegas[1] = vs[1];
    omegas[2:(H-1)] = vs[2:(H-1)] .* cumprod_one_mv[1:(H-2)];
    omegas[H] = cumprod_one_mv[H-1];
}

model {
    alpha  ~ gamma(a_alpha, b_alpha);
    vs     ~ beta(1,alpha);
    sigma2 ~ inv_gamma(a_sigma2, b_sigma2);
    tau2   ~ inv_gamma(a_tau2, b_tau2);
    //rho    ~ beta(alpha_rho, beta_rho);
    
    for (h in 1:H) {
        betas[1:p+1,h] ~ multi_normal(mu_0, Sigma_0); // P_0 of dp
    }
    
    for (h in 1:H) {
        for (i in 1:I) {
            xis[i,h] ~ beta(a_xi, b_xi);
        }
    }
    ws[1:I,1] ~ multi_normal(mu_w_1, tau2*inv_Q_w);
    
    for (t in 2:T){
        for(h in 1:H){
        ws[1:I,t] ~ multi_normal(diag_matrix(xis[1:I,h])*ws[1:I,t-1], tau2*inv_Q_w);
        }
    }
    
    for (t in 1:T) {
        for (i in 1:I){
            vector[H] log_probas;
            for (h in 1:H) {
                log_probas[h] = 
                    log(omegas[h]) + normal_lpdf(y[i,t] | dot_product(X[i,1:p+1],betas[1:p+1,h]) + ws[i,t], sigma);
            }
            target += log_sum_exp(log_probas);
        }
    }
    
}

generated quantities {
    
    // allocation just in space not also in time @TODO
    /*vector[I*T] s; // cluster allocations
    
    for (t in 1:T) {
        for (i in 1:I) {
            vector[H] log_probas;
            for (h in 1:H) {
                log_probas[h] = log(omegas[h]) + normal_lpdf(y[i,t] | dot_product(X[i,1:p+1],betas[1:p+1,h]), sigma);
            }
        s[i] = categorical_rng(softmax(log_probas));
    }
   }*/
    
    // generated quantities with random effect ?
    /*vector[ngrid] dens; // evaluate densities on grid (same dimension of data for now)

     for (i in 1:ngrid) {
        vector[H] log_probas;
        for (h in 1:H) {
            log_probas[h] = log(omegas[h]) + normal_lpdf(xgrid[i] | dot_product(X[i,1:p+1],betas[1:p+1,h]) , sigma);
        }
        dens[i] =  exp(log_sum_exp(log_probas));
    }*/
}

