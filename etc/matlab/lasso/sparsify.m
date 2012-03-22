function out = sparsify(x,prob)

probselector = @ (prob) 1/2*(1 + sign(rand() - (1 - prob)));
out = zeros(size(x));
for i = 1:length(x)
    out(i) = x(i)*probselector(prob);
end
end
