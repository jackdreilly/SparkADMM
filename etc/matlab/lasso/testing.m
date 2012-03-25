clc; close all; clear all;
nSamples = 100;
nFeatures = 10;
sparsity = .5;

A = randn(nSamples,nFeatures);
x = sparsify(randn(nFeatures,1),sparsity);
y = A*x;
xjack = lassojack(A,y,0.1,1.0);
xlasso = lasso(A,y,0.1,1.0,1.0);

[x xjack xlasso]