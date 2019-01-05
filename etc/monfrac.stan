data {
  vector[12] a;
  vector[12] b;
}
parameters {
  simplex[12] monfrac; 
}
model {
  monfrac ~ beta(a, b);
}

