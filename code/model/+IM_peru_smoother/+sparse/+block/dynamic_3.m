function [y, T] = dynamic_3(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
  y(13)=y(16)-y(11);
  y(14)=y(13)-y(3);
end
