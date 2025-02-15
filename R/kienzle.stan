//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> k;        // number of data points
  vector[k] Mc;            // observed cutting torque
  vector[k] phi;         // observed cutting tooth position <lower=0, upper=(2*pi)>
  real<lower=0> kappa;   // cutting tool angle
  int<lower=0> z;        // number of cutting teeth
  real<lower=0> fz;      // feed per tooth
  real<lower=0> ap;      // cutting depth
  real<lower=0> rtool;   // tool radius
  real<lower=0> m_kc;    // prior mean for kc11
  real<lower=0> alpha_mc; // alpha parameter for beta prior of mc
  real<lower=0> beta_mc;  // beta parameter for beta prior of mc
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
    real<lower=0> kc11;    // parameter kc11 for normal distribution
    real<lower=0, upper=1> mc; //  exponente mc (parameter for beta distribution)
    real<lower=0> tau;     // precision parameter for normal distribution
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Priors
    kc11 ~ normal(m_kc, 50);     // Normal prior for kc11 with standard deviation 50
    mc ~ beta(alpha_mc, beta_mc); // Beta prior for mc
    tau ~ gamma(0.5, 0.5);        // Gamma prior for tau
    real sigma = sqrt(1/tau);
    real sin_sum = 0;
    real pred = 0;
    real fact = 1.0/z;
    real base_arg = 0;
    // Likelihood
    for (i in 1:k) {
        sin_sum = 0;
        for (j in 1:z) {
          base_arg = phi[i] + (j - 1) * 2 * pi() / fact;
          if(base_arg >=0){
            //print("pow(sin(phi[i] + (j - 1) * 2 * pi() / z), 1 - mc): ", base_arg);
            sin_sum += pow(base_arg, 1 - mc);
          }
        }
       
        //print("sin_sum:", sin_sum);
        //print("inv. z:", fact);
        
        pred = (ap * pow(fz, (1 - mc)) * pow(sin(kappa), -mc) * kc11 * rtool) * sin_sum;
        Mc[i] ~ normal(pred, abs(sigma));  // Normal likelihood with precision tau
    }
}
generated quantities {
    // Posterior predictive reaction times
  }
