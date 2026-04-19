function [g1, T_order, T] = static_g1(y, x, params, sparse_rowval, sparse_colval, sparse_colptr, T_order, T)
if nargin < 8
    T_order = -1;
    T = NaN(1, 1);
end
[T_order, T] = IM_peru_ramsey.sparse.static_g1_tt(y, x, params, T_order, T);
g1_v = NaN(25, 1);
g1_v(1)=(-(2*params(2)));
g1_v(2)=1;
g1_v(3)=1;
g1_v(4)=(-(2*(1-params(2))));
g1_v(5)=(-1);
g1_v(6)=1;
g1_v(7)=params(1)-1;
g1_v(8)=params(3)*params(4);
g1_v(9)=(-1);
g1_v(10)=1-params(5);
g1_v(11)=(-(params(3)*params(4)));
g1_v(12)=1-params(7);
g1_v(13)=(-(params(3)*params(4)));
g1_v(14)=1;
g1_v(15)=(-1);
g1_v(16)=1;
g1_v(17)=(-1);
g1_v(18)=1;
g1_v(19)=params(1)-params(9);
g1_v(20)=T(1)-1;
g1_v(21)=params(3)*params(4);
g1_v(22)=(-(params(3)*params(4)));
g1_v(23)=(-(params(3)*params(4)));
g1_v(24)=1+params(9)*(-params(5));
g1_v(25)=1+params(9)*(-params(7));
if ~isoctave && matlab_ver_less_than('9.8')
    sparse_rowval = double(sparse_rowval);
    sparse_colval = double(sparse_colval);
end
g1 = sparse(sparse_rowval, sparse_colval, g1_v, 12, 12);
end
