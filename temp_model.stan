data {
 int < lower = 1 > N; // Sample size
 vector[N] temp;
}

parameters {
 real mean_; // Intercept
 real < lower = 0 > sigma; // Error SD
}

model {
 temp ~ normal(mean_ , sigma);
}
