function g1m = ramsey_multipliers_static_g1(y, x, params, sparse_rowval, sparse_colval, sparse_colptr)
g1m_v=NaN(12,1);
g1m_v(1)=1;
g1m_v(2)=(-1);
g1m_v(3)=1;
g1m_v(4)=(-1);
g1m_v(5)=1;
g1m_v(6)=params(1)-params(9);
g1m_v(7)=params(9)^(-1)-1;
g1m_v(8)=params(3)*params(4);
g1m_v(9)=(-(params(3)*params(4)));
g1m_v(10)=(-(params(3)*params(4)));
g1m_v(11)=1+params(9)*(-params(5));
g1m_v(12)=1+params(9)*(-params(7));
if ~isoctave && matlab_ver_less_than('9.8')
    sparse_rowval = double(sparse_rowval);
    sparse_colval = double(sparse_colval);
end
g1m = sparse(sparse_rowval, sparse_colval, g1m_v, 7, 5);
end
