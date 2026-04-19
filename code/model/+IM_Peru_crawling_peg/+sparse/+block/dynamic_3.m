function [y, T, residual, g1] = dynamic_3(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
residual=NaN(3, 1);
  T(1)=2*params(2)*params(3)/(1-params(2))*params(3)*params(4)/(params(3)*params(4)+1+params(1))*(y(6)+y(7)-y(4))^2;
  T(2)=(-T(1));
  residual(1)=(y(11))-((y(12)-y(9))*T(2));
  residual(2)=(y(18))-(y(21));
  residual(3)=(y(12))-(y(14)+y(11)-y(10));
if nargout > 3
    g1_v = NaN(7, 1);
g1_v(1)=T(2);
g1_v(2)=1;
g1_v(3)=(-1);
g1_v(4)=1;
g1_v(5)=T(1);
g1_v(6)=1;
g1_v(7)=(-1);
    if ~isoctave && matlab_ver_less_than('9.8')
        sparse_rowval = double(sparse_rowval);
        sparse_colval = double(sparse_colval);
    end
    g1 = sparse(sparse_rowval, sparse_colval, g1_v, 3, 9);
end
end
