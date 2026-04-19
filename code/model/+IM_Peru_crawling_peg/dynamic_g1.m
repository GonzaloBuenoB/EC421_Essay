function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
% function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T             [#temp variables by 1]     double   vector of temporary terms to be filled by function
%   y             [#dynamic variables by 1]  double   vector of endogenous variables in the order stored
%                                                     in M_.lead_lag_incidence; see the Manual
%   x             [nperiods by M_.exo_nbr]   double   matrix of exogenous variables (in declaration order)
%                                                     for all simulation periods
%   steady_state  [M_.endo_nbr by 1]         double   vector of steady state values
%   params        [M_.param_nbr by 1]        double   vector of parameter values in declaration order
%   it_           scalar                     double   time period for exogenous variables for which
%                                                     to evaluate the model
%   T_flag        boolean                    boolean  flag saying whether or not to calculate temporary terms
%
% Output:
%   g1
%

if T_flag
    T = IM_Peru_crawling_peg.dynamic_g1_tt(T, y, x, params, steady_state, it_);
end
g1 = zeros(9, 18);
g1(1,6)=1;
g1(1,7)=(-1);
g1(1,8)=1;
g1(1,10)=(-1);
g1(2,6)=1;
g1(2,1)=(-1);
g1(2,9)=params(1);
g1(3,6)=(-1);
g1(3,15)=1;
g1(3,9)=params(3)*params(4);
g1(3,11)=(-(params(3)*params(4)));
g1(3,12)=(-(params(3)*params(4)));
g1(4,16)=(-1);
g1(4,14)=1;
g1(5,7)=1;
g1(5,8)=T(2);
g1(5,1)=(-((y(8)-y(5))*(-(T(1)*(-(2*(y(3)+y(4)-y(1))))))));
g1(5,3)=(-((y(8)-y(5))*(-(T(1)*2*(y(3)+y(4)-y(1))))));
g1(5,4)=(-((y(8)-y(5))*(-(T(1)*2*(y(3)+y(4)-y(1))))));
g1(5,5)=(-T(2));
g1(6,6)=1;
g1(6,15)=(-1);
g1(6,8)=1;
g1(6,16)=(-1);
g1(6,13)=1;
g1(7,2)=(-params(5));
g1(7,10)=1;
g1(7,17)=(-1);
g1(8,3)=(-params(7));
g1(8,11)=1;
g1(8,18)=(-1);
g1(9,11)=params(9);
g1(9,12)=1;

end
