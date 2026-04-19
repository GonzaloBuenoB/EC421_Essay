function [g1, T_order, T] = static_g1(y, x, params, sparse_rowval, sparse_colval, sparse_colptr, T_order, T)
if nargin < 8
    T_order = -1;
    T = NaN(2, 1);
end
[T_order, T] = IM_Peru_fixed_v2.sparse.static_g1_tt(y, x, params, T_order, T);
g1_v = NaN(25, 1);
g1_v(1)=1;
g1_v(2)=1;
g1_v(3)=1.00;
g1_v(4)=(-1);
g1_v(5)=1;
g1_v(6)=(-1.00);
g1_v(7)=1;
g1_v(8)=(-1);
g1_v(9)=T(2);
g1_v(10)=params(1)-1;
g1_v(11)=params(3)*params(4);
g1_v(12)=(-((y(3)-y(9))*(-(T(1)*(-(2*(y(6)+y(7)-y(4))))))));
g1_v(13)=(-1);
g1_v(14)=1-params(5);
g1_v(15)=(-1.00);
g1_v(16)=(-(params(3)*params(4)));
g1_v(17)=(-((y(3)-y(9))*(-(T(1)*2*(y(6)+y(7)-y(4))))));
g1_v(18)=1-params(7);
g1_v(19)=(-1);
g1_v(20)=(-(params(3)*params(4)));
g1_v(21)=(-((y(3)-y(9))*(-(T(1)*2*(y(6)+y(7)-y(4))))));
g1_v(22)=(-1);
g1_v(23)=1;
g1_v(24)=1;
g1_v(25)=(-T(2));
if ~isoctave && matlab_ver_less_than('9.8')
    sparse_rowval = double(sparse_rowval);
    sparse_colval = double(sparse_colval);
end
g1 = sparse(sparse_rowval, sparse_colval, g1_v, 9, 9);
end
