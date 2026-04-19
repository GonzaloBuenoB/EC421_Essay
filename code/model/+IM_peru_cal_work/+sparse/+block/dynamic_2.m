function [y, T, residual, g1] = dynamic_2(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
residual=NaN(2, 1);
  residual(1)=(params(1)*y(13))-(y(4)-y(10));
  residual(2)=(y(19))-(y(10)+params(3)*params(4)*(y(15)+y(16)-y(13)));
if nargout > 3
    g1_v = NaN(6, 1);
g1_v(1)=(-1);
g1_v(2)=params(1);
g1_v(3)=params(3)*params(4);
g1_v(4)=1;
g1_v(5)=(-1);
g1_v(6)=1;
    if ~isoctave && matlab_ver_less_than('9.8')
        sparse_rowval = double(sparse_rowval);
        sparse_colval = double(sparse_colval);
    end
    g1 = sparse(sparse_rowval, sparse_colval, g1_v, 2, 6);
end
end
