

data {
    int<lower=1> N;
    vector[N] y;
    
    real<lower=0> alpha;
    real mu0;
    real<lower=0> s0;
    
    real<lower=0> a;
    real<lower=0> b;
    
    int<lower=1> H;
    
    int<lower=1> ngrid;
    vector[ngrid] xgrid;
}

parameters {
    vector[H] means;
    vector<lower=0>[H] vars;
    
    vector<lower=0, upper=1>[H-1] nus;
}

transformed parameters{
    vector[H] sds= sqrt(vars);
    
    simplex[H] ws;
    
    vector<lower=0, upper=1>[H-1] cumprod_one_mv;
    cumprod_one_mv = exp(cumulative_sum(log1m(nus)));
    
    ws[1] = nus[1];
    ws[2:(H-1)] = nus[2:(H-1)] .* cumprod_one_mv[1:(H-2)];
    ws[H] = cumprod_one_mv[H-1];
}

model {
    nus ~ beta(1,alpha);
    means ~ normal(mu0, s0);
    vars ~ inv_gamma(a,b);
    
    for (i in 1:N) {
        vector[H] log_probas;
        for (h in 1:H) {
            log_probas[h] = log(ws[h]) + normal_lpdf(y[i] | means[h], sds[h]);
        }
        target += log_sum_exp(log_probas);
    }
}

generated quantities {
    vector[N] clus_allocs;
    
     for (i in 1:N) {
        vector[H] log_probas;
        for (h in 1:H) {
            log_probas[h] = log(ws[h]) + normal_lpdf(y[i] | means[h], sds[h]);
        }
        clus_allocs[i] = categorical_rng(softmax(log_probas));
    }
    vector[ngrid] dens;
    
     for (i in 1:ngrid) {
        vector[H] log_probas;
        for (h in 1:H) {
            log_probas[h] = log(ws[h]) + normal_lpdf(xgrid[i] | means[h], sds[h]);
        }
        dens[i] =  exp(log_sum_exp(log_probas));
    }
}




