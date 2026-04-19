function [y, T, residual, g1] = dynamic_2(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
residual=NaN(6, 1);
  T(1)=2*params(2)*params(3)/(1-params(2))*params(3)*params(4)/(params(3)*params(4)+1+params(1));
  T(2)=T(1)*(y(6)+y(7)-y(4))^2;
  T(3)=(-T(2));
  residual(1)=(y(11))-((y(12)-y(9))*T(3));
  residual(2)=(y(18))-(y(21));
  residual(3)=((y(14)+y(11)-y(10))*(-1.00))-(y(16));
  residual(4)=(params(1)*y(13))-(y(4)-y(10));
  residual(5)=(y(19))-(y(10)+params(3)*params(4)*(y(16)+y(15)-y(13)));
  residual(6)=(y(12))-(y(14)+y(11)-y(10)+y(16));
if nargout > 3
    g1_v = NaN(21, 1);
g1_v(1)=T(3);
g1_v(2)=(-((y(12)-y(9))*(-(T(1)*2*(y(6)+y(7)-y(4))))));
g1_v(3)=(-((y(12)-y(9))*(-(T(1)*(-(2*(y(6)+y(7)-y(4))))))));
g1_v(4)=(-1);
g1_v(5)=1;
g1_v(6)=(-1.00);
g1_v(7)=(-1);
g1_v(8)=1;
g1_v(9)=(-1);
g1_v(10)=(-(params(3)*params(4)));
g1_v(11)=(-1);
g1_v(12)=params(1);
g1_v(13)=params(3)*params(4);
g1_v(14)=1.00;
g1_v(15)=1;
g1_v(16)=(-1);
g1_v(17)=1;
g1_v(18)=T(2);
g1_v(19)=1;
g1_v(20)=1;
g1_v(21)=(-1);
    if ~isoctave && matlab_ver_less_than('9.8')
        sparse_rowval = double(sparse_rowval);
        sparse_colval = double(sparse_colval);
    end
    g1 = sparse(sparse_rowval, sparse_colval, g1_v, 6, 18);
end
end
