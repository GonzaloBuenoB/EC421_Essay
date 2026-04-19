function [g3_v, T_order, T] = static_g3(y, x, params, T_order, T)
if nargin < 5
    T_order = -1;
    T = NaN(0, 1);
end
[T_order, T] = IM_peru_ramsey.objective.sparse.static_g3_tt(y, x, params, T_order, T);
g3_v = NaN(0, 1);
end
