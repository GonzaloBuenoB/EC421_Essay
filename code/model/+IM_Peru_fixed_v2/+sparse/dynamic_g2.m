function [g2_v, T_order, T] = dynamic_g2(y, x, params, steady_state, T_order, T)
if nargin < 6
    T_order = -1;
    T = NaN(2, 1);
end
[T_order, T] = IM_Peru_fixed_v2.sparse.dynamic_g2_tt(y, x, params, steady_state, T_order, T);
g2_v = NaN(12, 1);
g2_v(1)=T(1)*(-(2*(y(6)+y(7)-y(4))));
g2_v(2)=T(1)*2*(y(6)+y(7)-y(4));
g2_v(3)=T(1)*2*(y(6)+y(7)-y(4));
g2_v(4)=(-((y(12)-y(9))*(-(2*T(1)))));
g2_v(5)=(-((y(12)-y(9))*(-(T(1)*(-2)))));
g2_v(6)=(-((y(12)-y(9))*(-(T(1)*(-2)))));
g2_v(7)=(-(T(1)*(-(2*(y(6)+y(7)-y(4))))));
g2_v(8)=(-((y(12)-y(9))*(-(2*T(1)))));
g2_v(9)=(-((y(12)-y(9))*(-(2*T(1)))));
g2_v(10)=(-(T(1)*2*(y(6)+y(7)-y(4))));
g2_v(11)=(-((y(12)-y(9))*(-(2*T(1)))));
g2_v(12)=(-(T(1)*2*(y(6)+y(7)-y(4))));
end
