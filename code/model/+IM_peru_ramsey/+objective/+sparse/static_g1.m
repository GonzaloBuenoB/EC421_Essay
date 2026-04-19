function [g1, T_order, T] = static_g1(y, x, params, sparse_rowval, sparse_colval, sparse_colptr, T_order, T)
if nargin < 8
    T_order = -1;
    T = NaN(0, 1);
end
[T_order, T] = IM_peru_ramsey.objective.sparse.static_g1_tt(y, x, params, T_order, T);
g1_v = NaN(2, 1);
g1_v(1)=(-(params(2)*2*y(1)));
g1_v(2)=(-((1-params(2))*2*y(2)));
if ~isoctave && matlab_ver_less_than('9.8')
    sparse_rowval = double(sparse_rowval);
    sparse_colval = double(sparse_colval);
end
g1 = sparse(sparse_rowval, sparse_colval, g1_v, 1, 12);
end
