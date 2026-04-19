function [residual, T_order, T] = static_resid(y, x, params, T_order, T)
if nargin < 5
    T_order = -1;
    T = NaN(0, 1);
end
[T_order, T] = IM_peru_smoother.sparse.static_resid_tt(y, x, params, T_order, T);
residual = NaN(10, 1);
    residual(1) = (y(3)) - (y(6)-y(1));
residual(2) = y(4);
    residual(3) = (params(1)*y(5)) - (y(5)-y(1));
    residual(4) = (y(1)) - (y(1)+params(3)*params(4)*(y(7)+y(8)-y(5)));
    residual(5) = (y(10)) - (y(3));
residual(6) = y(2);
residual(7) = y(9);
    residual(8) = (y(6)) - (y(6)*params(5)+x(1));
    residual(9) = (y(7)) - (y(7)*params(7)+x(2));
    residual(10) = (y(8)) - (y(7)*(-params(9))+x(3));
end
