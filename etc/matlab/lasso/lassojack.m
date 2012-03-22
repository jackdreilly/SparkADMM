function z = lassojack(A, b, lambda, rho)

MAX_ITER = 1000;
[m, n] = size(A);


Atb = A'*b;
AtA = A'*A + rho*eye(n);

kappa = lambda/rho;



x = zeros(n,1);
z = zeros(n,1);
u = zeros(n,1);

for k = 1:MAX_ITER
    
    x = AtA \ (Atb + rho*(z - u));    % temporary value
    z =  max( 0, x+u - kappa ) - max( 0, -x-u - kappa );
    u = u + (x - z);

end

end