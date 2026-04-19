function g1 = static_g1(T, y, x, params, T_flag)
% function g1 = static_g1(T, y, x, params, T_flag)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T         [#temp variables by 1]  double   vector of temporary terms to be filled by function
%   y         [M_.endo_nbr by 1]      double   vector of endogenous variables in declaration order
%   x         [M_.exo_nbr by 1]       double   vector of exogenous variables in declaration order
%   params    [M_.param_nbr by 1]     double   vector of parameter values in declaration order
%                                              to evaluate the model
%   T_flag    boolean                 boolean  flag saying whether or not to calculate temporary terms
%
% Output:
%   g1
%

if T_flag
    T = IM_peru_ramsey.static_g1_tt(T, y, x, params);
end
g1 = zeros(12, 12);
g1(1,1)=(-(2*params(2)));
g1(1,8)=1;
g1(1,9)=1;
g1(1,10)=T(1)-1;
g1(2,2)=(-(2*(1-params(2))));
g1(2,8)=(-1);
g1(3,8)=1;
g1(4,9)=params(1)-params(9);
g1(4,10)=params(3)*params(4);
g1(5,8)=(-1);
g1(5,11)=1+params(9)*(-params(5));
g1(6,10)=(-(params(3)*params(4)));
g1(6,12)=1+params(9)*(-params(7));
g1(7,10)=(-(params(3)*params(4)));
g1(8,1)=1;
g1(8,2)=(-1);
g1(8,3)=1;
g1(8,5)=(-1);
g1(9,1)=1;
g1(9,4)=params(1)-1;
g1(10,4)=params(3)*params(4);
g1(10,6)=(-(params(3)*params(4)));
g1(10,7)=(-(params(3)*params(4)));
g1(11,5)=1-params(5);
g1(12,6)=1-params(7);

end
