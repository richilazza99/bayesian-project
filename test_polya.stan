
data 
{
    real<lower=0> alpha;
    int<lower=1> I;
}

parameters 
{
    vector<lower=0>[I] s;
}

model
{
    // declare variables
    vector[I] n;
    int K_I;
    vector[I] theta;
    int tmp;
    
    // initialize to zero    
    for (i in 1:I) {
        n[i] = 0;
    }
    
    // set first values
    n[1] = 1;
    K_I = 0;
    
    //
    for (i in 1:I) {
    
        for (j in 1:K_I)
            theta[j] = n[j] / (i - 1 + alpha);
            
        theta[K_I + 1] = alpha / (i - 1 + alpha);
        
        tmp ~ categorical(theta);
        
        s[i] = tmp;
        
        if (s[i] == K_I + 1)
            K_I += 1;
            
        n[s[i]] += 1;
    }
    
}
