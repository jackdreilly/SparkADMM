clear all; clc; close all;
m = 2;

C = 3*ones(m);
x = ones(m,1);
u = 3*ones(m,1);
z = 5*ones(m,1);
rho = .25;

f = @(w) (sum(log(1 + exp(C*w))) + (rho/2)*norm(w - z + u).^2);
gfn = @(w) (C'*(exp(C*w)./(1 + exp(C*w))) + rho*(w - z + u));

alpha = .1;
beta = .5;

for i = 1:10
    grad = gfn(x);
    dx = -grad;
    t = 1.0;
    while true
        lhs = f(x + t*dx);
        rhs = f(x) + alpha*t*dx'*grad;
        if lhs < rhs
            break;
        end
        t = beta*t;
    end
    x = x + t*dx;
    f(x)
end




