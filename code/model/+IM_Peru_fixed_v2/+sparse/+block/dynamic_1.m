function [y, T] = dynamic_1(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
  y(14)=params(5)*y(5)+x(1);
  y(15)=y(6)*params(7)+x(2);
end
