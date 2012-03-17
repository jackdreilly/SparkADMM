function w = safe_lasso_cd(X,y,lambda,nsweeps)
%% function w = safe_lasso_cd(X,y,lambda,nsweeps)
% SAFE_LASSO_CD solves the LASSO problem 
%   min_w .5*norm(X'*w-y,2)^2 + lambda*norm(w,1)
% using a coordinate descent method, combined with a safe feature
% elimination method. 
% inputs:
% X         nxm matrix (n=#features, m=#observations)
% y         mx1 response vector
% lambda    penalty parameter
% nsweeps   number of sweeps through all elements
% outputs
% w         optimal primal variable

%% preliminaries
[n,m] = size(X);
w = sparse(n,1);
theta = sparse(m,1);
inds_keep = 1:n;
inds_zero = [];
y2 = norm(y,2)^2;
Xy = X*y;
theta0 = y;
gamma = -inf; % we keep track of the best lower bound so far

%% coordinate descent method
% do a certain number of column sweeps
for ii = 1:nsweeps,
    txt = sprintf('sweep=%d:\n',ii);
    fprintf(txt)
    
    % go through each coordinate
    for k = 1:n,
        % check if feature is not already set to zero
        if any(k == inds_zero), continue, end
        
        % select k-th feature
        xk = X(k,:)';
        
        % set a vector equal to the current w except for zero in k-th
        % position
        w0 = w; w0(k) = 0; w0 = w0(inds_keep);
        
        % set up data for one-dimensional problem
        ynew = y-X(inds_keep,:)'*w0;
        alpha = xk'*xk;
        beta = xk'*ynew;
        
        % solve one-dimensional problem to update k-th element in w
        w(k) = (sign(beta)/alpha)*max(abs(beta)-lambda,0);
        
        % set corresponding optimal dual point and lambda-value
        theta0 = ynew-xk*w(k);
        lambda0 = norm(X(inds_keep,:)*theta0,inf);
        
        % scale that point and get the lower bound
        alpha0 = norm(theta0,2)^2;
        beta0 = abs(theta0'*y);
        gamma0 = (beta0^2)/(2*alpha0);
        gammanew = gamma0* ...
            (1-(max(1-(alpha0/beta0)*(lambda/lambda0),0)^2));
        
        % select the best lower bound so far
        gamma = max(gamma,gammanew);
        
        % now find features to eliminate based on that bound
        check = (lambda<=abs(Xy)+sqrt(y2-2*gamma));
        inds_keep = find(check);
        inds_zero = find(~check);
        %length(inds_zero)
        
        % set corresponding indices in w to zero
        w(inds_zero) = 0; % we won't modify this anymore
        
        % print outs
       % txt = sprintf('k=%d, num el = %d \n',k,length(inds_zero));
       % fprintf(txt)
    end
end
