function [lhs, rhs] = static_resid(y, x, params)
T = NaN(0, 1);
lhs = NaN(1, 1);
rhs = NaN(1, 1);
lhs(1) = (-(params(2)*y(1)^2+(1-params(2))*y(2)^2));
rhs(1) = 0;
end
