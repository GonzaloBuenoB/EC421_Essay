function [g2_v, T_order, T] = static_g2(y, x, params, T_order, T)
if nargin < 5
    T_order = -1;
    T = NaN(0, 1);
end
[T_order, T] = IM_peru_ramsey.objective.sparse.static_g2_tt(y, x, params, T_order, T);
g2_v = NaN(2, 1);
g2_v(1)=(-(2*params(2)));
g2_v(2)=(-(2*(1-params(2))));
end
