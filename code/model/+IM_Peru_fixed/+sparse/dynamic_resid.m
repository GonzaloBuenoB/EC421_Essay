function [residual, T_order, T] = dynamic_resid(y, x, params, steady_state, T_order, T)
if nargin < 6
    T_order = -1;
    T = NaN(0, 1);
end
[T_order, T] = IM_Peru_fixed.sparse.dynamic_resid_tt(y, x, params, steady_state, T_order, T);
residual = NaN(9, 1);
    residual(1) = (0) - (y(14)+y(11)-y(10));
residual(2) = y(12);
    residual(3) = (params(1)*y(13)) - (y(4)-y(10));
    residual(4) = (y(19)) - (y(10)+params(3)*params(4)*(y(15)-y(13)));
residual(5) = y(18);
    residual(6) = (y(17)) - (y(19)-y(10));
    residual(7) = (y(14)) - (params(5)*y(5)+x(1));
    residual(8) = (y(15)) - (y(6)*params(7)+x(2));
residual(9) = y(16);
end
