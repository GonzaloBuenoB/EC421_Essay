function [T_order, T] = static_g3_tt(y, x, params, T_order, T)
if T_order >= 3
    return
end
[T_order, T] = IM_peru_ramsey.objective.sparse.static_g2_tt(y, x, params, T_order, T);
T_order = 3;
if size(T, 1) < 0
    T = [T; NaN(0 - size(T, 1), 1)];
end
end
