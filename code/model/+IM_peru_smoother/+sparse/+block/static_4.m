function [y, T] = static_4(y, x, params, sparse_rowval, sparse_colval, sparse_colptr, T)
  y(8)=y(7)*(-params(9))+x(3);
end
