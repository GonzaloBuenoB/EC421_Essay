function [y, T] = dynamic_1(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
  y(12)=0;
  y(16)=params(5)*y(6)+x(1);
  y(17)=params(7)*y(7)+x(2);
  y(18)=y(17)*(-params(9))+x(3);
end
