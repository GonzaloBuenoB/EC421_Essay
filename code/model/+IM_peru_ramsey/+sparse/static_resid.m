function [residual, T_order, T] = static_resid(y, x, params, T_order, T)
if nargin < 5
    T_order = -1;
    T = NaN(1, 1);
end
[T_order, T] = IM_peru_ramsey.sparse.static_resid_tt(y, x, params, T_order, T);
residual = NaN(12, 1);
residual(1) = y(8)+y(9)-y(10)+y(10)*T(1)-params(2)*2*y(1);
residual(2) = (-y(8))-(1-params(2))*2*y(2);
residual(3) = y(8);
residual(4) = y(10)*params(3)*params(4)+y(9)*params(1)+params(9)*(-y(9));
residual(5) = y(11)+params(9)*y(11)*(-params(5))-y(8);
residual(6) = y(12)+y(10)*(-(params(3)*params(4)))+params(9)*y(12)*(-params(7));
residual(7) = y(10)*(-(params(3)*params(4)));
residual(8) = y(3)-(y(2)+y(5)-y(1));
residual(9) = params(1)*y(4)-(y(4)-y(1));
residual(10) = y(1)-(y(1)+params(3)*params(4)*(y(6)+y(7)-y(4)));
residual(11) = y(5)-(params(5)*y(5)+x(1));
residual(12) = y(6)-(params(7)*y(6)+x(2));
end
