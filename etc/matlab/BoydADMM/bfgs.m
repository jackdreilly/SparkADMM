function output = bfgs(fn, grad, Binv0, x0)

Binv = Binv0;
x = x0;
while true
    p = -1.*Binv*grad(x);
end


end