function [residual, T_order, T] = dynamic_resid(y, x, params, steady_state, T_order, T)
if nargin < 6
    T_order = -1;
    T = NaN(0, 1);
end
[T_order, T] = IM_peru_smoother.sparse.dynamic_resid_tt(y, x, params, steady_state, T_order, T);
residual = NaN(10, 1);
    residual(1) = (y(13)) - (y(16)-y(11));
    residual(2) = (y(14)) - (y(13)-y(3));
    residual(3) = (params(1)*y(15)) - (y(5)-y(11));
    residual(4) = (y(21)) - (y(11)+params(3)*params(4)*(y(17)+y(18)-y(15)));
    residual(5) = (y(20)) - (y(23));
residual(6) = y(12);
    residual(7) = (y(19)) - (y(21)+y(23)-y(13)-y(11));
    residual(8) = (y(16)) - (params(5)*y(6)+x(1));
    residual(9) = (y(17)) - (params(7)*y(7)+x(2));
    residual(10) = (y(18)) - (y(17)*(-params(9))+x(3));
end
