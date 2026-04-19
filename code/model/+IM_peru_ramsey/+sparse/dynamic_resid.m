function [residual, T_order, T] = dynamic_resid(y, x, params, steady_state, T_order, T)
if nargin < 6
    T_order = -1;
    T = NaN(1, 1);
end
[T_order, T] = IM_peru_ramsey.sparse.dynamic_resid_tt(y, x, params, steady_state, T_order, T);
residual = NaN(12, 1);
residual(1) = y(20)+y(21)-y(22)+T(1)*y(10)-params(2)*2*y(13);
residual(2) = (-y(20))-(1-params(2))*2*y(14);
residual(3) = y(20);
residual(4) = params(3)*params(4)*y(22)+params(1)*y(21)+params(9)*(-y(33));
residual(5) = y(23)+params(9)*y(35)*(-params(5))-y(20);
residual(6) = y(24)+y(22)*(-(params(3)*params(4)))+params(9)*y(36)*(-params(7));
residual(7) = y(22)*(-(params(3)*params(4)));
residual(8) = y(15)-(y(17)+y(14)-y(13));
residual(9) = params(1)*y(16)-(y(4)-y(13));
residual(10) = y(25)-(y(13)+params(3)*params(4)*(y(18)+y(19)-y(16)));
residual(11) = y(17)-(params(5)*y(5)+x(1));
residual(12) = y(18)-(params(7)*y(6)+x(2));
end
