function [residual, T_order, T] = static_resid(y, x, params, T_order, T)
if nargin < 5
    T_order = -1;
    T = NaN(0, 1);
end
[T_order, T] = IM_Peru_fixed.sparse.static_resid_tt(y, x, params, T_order, T);
residual = NaN(9, 1);
    residual(1) = (0) - (y(5)+y(2)-y(1));
residual(2) = y(3);
    residual(3) = (params(1)*y(4)) - (y(4)-y(1));
    residual(4) = (y(1)) - (y(1)+params(3)*params(4)*(y(6)-y(4)));
residual(5) = y(9);
residual(6) = y(8);
    residual(7) = (y(5)) - (y(5)*params(5)+x(1));
    residual(8) = (y(6)) - (y(6)*params(7)+x(2));
residual(9) = y(7);
end
