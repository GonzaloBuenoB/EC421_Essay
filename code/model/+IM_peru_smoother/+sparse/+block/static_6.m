function [y, T] = static_6(y, x, params, sparse_rowval, sparse_colval, sparse_colptr, T)
  y(3)=y(6)-y(1);
  y(10)=y(3);
end
