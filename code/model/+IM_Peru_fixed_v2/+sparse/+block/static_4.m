function [y, T, residual, g1] = static_4(y, x, params, sparse_rowval, sparse_colval, sparse_colptr, T)
residual=NaN(6, 1);
  residual(1)=(y(9))-(y(3));
  T(1)=2*params(2)*params(3)/(1-params(2))*params(3)*params(4)/(params(3)*params(4)+1+params(1));
  T(2)=T(1)*(y(6)+y(7)-y(4))^2;
  T(3)=(-T(2));
  residual(2)=(y(2))-((y(3)-y(9))*T(3));
  residual(3)=(y(3))-(y(7)+y(5)+y(2)-y(1));
  residual(4)=(params(1)*y(4))-(y(4)-y(1));
  residual(5)=(y(1))-(y(1)+params(3)*params(4)*(y(6)+y(7)-y(4)));
  residual(6)=((y(5)+y(2)-y(1))*(-1.00))-(y(7));
if nargout > 3
    g1_v = NaN(18, 1);
g1_v(1)=1;
g1_v(2)=T(3);
g1_v(3)=1;
g1_v(4)=(-1);
g1_v(5)=(-1.00);
g1_v(6)=(-1);
g1_v(7)=T(2);
g1_v(8)=1;
g1_v(9)=1;
g1_v(10)=1;
g1_v(11)=1.00;
g1_v(12)=(-((y(3)-y(9))*(-(T(1)*(-(2*(y(6)+y(7)-y(4))))))));
g1_v(13)=params(1)-1;
g1_v(14)=params(3)*params(4);
g1_v(15)=(-((y(3)-y(9))*(-(T(1)*2*(y(6)+y(7)-y(4))))));
g1_v(16)=(-1);
g1_v(17)=(-(params(3)*params(4)));
g1_v(18)=(-1);
    if ~isoctave && matlab_ver_less_than('9.8')
        sparse_rowval = double(sparse_rowval);
        sparse_colval = double(sparse_colval);
    end
    g1 = sparse(sparse_rowval, sparse_colval, g1_v, 6, 6);
end
end
