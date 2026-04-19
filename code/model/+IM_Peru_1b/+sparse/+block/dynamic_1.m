function [y, T] = dynamic_1(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
  y(9)=0;
  y(12)=params(5)*y(5)+x(1);
  y(13)=params(7)*y(6)+x(2);
  y(14)=(-y(13));
end
