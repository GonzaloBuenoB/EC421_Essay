function [residual, T_order, T] = dynamic_resid(y, x, params, steady_state, T_order, T)
if nargin < 6
    T_order = -1;
    T = NaN(0, 1);
end
[T_order, T] = IM_Peru_1b.sparse.dynamic_resid_tt(y, x, params, steady_state, T_order, T);
residual = NaN(7, 1);
    residual(1) = (y(10)) - (y(12)-y(8));
    residual(2) = (params(1)*y(11)) - (y(4)-y(8));
    residual(3) = (y(15)) - (y(8)+params(3)*params(4)*(y(13)+y(14)-y(11)));
residual(4) = y(9);
    residual(5) = (y(12)) - (params(5)*y(5)+x(1));
    residual(6) = (y(13)) - (params(7)*y(6)+x(2));
    residual(7) = (y(14)) - ((-y(13)));
end
