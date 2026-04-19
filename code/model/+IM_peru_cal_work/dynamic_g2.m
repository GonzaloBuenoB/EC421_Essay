function g2 = dynamic_g2(T, y, x, params, steady_state, it_, T_flag)
% function g2 = dynamic_g2(T, y, x, params, steady_state, it_, T_flag)
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
%   g2
%

if T_flag
    T = IM_peru_cal_work.dynamic_g2_tt(T, y, x, params, steady_state, it_);
end
g2_i = zeros(21,1);
g2_j = zeros(21,1);
g2_v = zeros(21,1);

g2_i(1)=5;
g2_i(2)=5;
g2_i(3)=5;
g2_i(4)=5;
g2_i(5)=5;
g2_i(6)=5;
g2_i(7)=5;
g2_i(8)=5;
g2_i(9)=5;
g2_i(10)=5;
g2_i(11)=5;
g2_i(12)=5;
g2_i(13)=5;
g2_i(14)=5;
g2_i(15)=5;
g2_i(16)=5;
g2_i(17)=5;
g2_i(18)=5;
g2_i(19)=5;
g2_i(20)=5;
g2_i(21)=5;
g2_j(1)=127;
g2_j(2)=8;
g2_j(3)=129;
g2_j(4)=44;
g2_j(5)=130;
g2_j(6)=62;
g2_j(7)=1;
g2_j(8)=3;
g2_j(9)=37;
g2_j(10)=4;
g2_j(11)=55;
g2_j(12)=5;
g2_j(13)=73;
g2_j(14)=39;
g2_j(15)=40;
g2_j(16)=57;
g2_j(17)=41;
g2_j(18)=75;
g2_j(19)=58;
g2_j(20)=59;
g2_j(21)=76;
g2_v(1)=T(1)*(-(2*(y(3)+y(4)-y(1))));
g2_v(2)=g2_v(1);
g2_v(3)=T(1)*2*(y(3)+y(4)-y(1));
g2_v(4)=g2_v(3);
g2_v(5)=T(1)*2*(y(3)+y(4)-y(1));
g2_v(6)=g2_v(5);
g2_v(7)=(-((y(8)-y(5))*(-(2*T(1)))));
g2_v(8)=(-((y(8)-y(5))*(-(T(1)*(-2)))));
g2_v(9)=g2_v(8);
g2_v(10)=(-((y(8)-y(5))*(-(T(1)*(-2)))));
g2_v(11)=g2_v(10);
g2_v(12)=(-(T(1)*(-(2*(y(3)+y(4)-y(1))))));
g2_v(13)=g2_v(12);
g2_v(14)=(-((y(8)-y(5))*(-(2*T(1)))));
g2_v(15)=(-((y(8)-y(5))*(-(2*T(1)))));
g2_v(16)=g2_v(15);
g2_v(17)=(-(T(1)*2*(y(3)+y(4)-y(1))));
g2_v(18)=g2_v(17);
g2_v(19)=(-((y(8)-y(5))*(-(2*T(1)))));
g2_v(20)=(-(T(1)*2*(y(3)+y(4)-y(1))));
g2_v(21)=g2_v(20);
g2 = sparse(g2_i,g2_j,g2_v,9,324);
end
