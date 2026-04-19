function [residual, T_order, T] = static_resid(y, x, params, T_order, T)
if nargin < 5
    T_order = -1;
    T = NaN(0, 1);
end
[T_order, T] = IM_peru_ramsey.objective.sparse.static_resid_tt(y, x, params, T_order, T);
residual = NaN(1, 1);
residual(1) = (-(params(2)*y(1)^2+(1-params(2))*y(2)^2));
end
