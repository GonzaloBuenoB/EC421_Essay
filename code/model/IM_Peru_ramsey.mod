// IM_peru_ramsey.mod - Full Ramsey Optimal Policy
// Uses new Dynare 6.5 syntax
// ========================================

// ========================================
// 1. Variables
// ========================================
var
    z           // risk-sharing wedge
    x           // output gap
    e           // exchange rate
    b_star      // net foreign assets
    q_tilde     // natural RER
    n_star      // capital flow shock
    f_star;     // FX intervention (INSTRUMENT)

varexo
    eps_q       // ToT shock
    eps_n;      // capital flow shock

// ========================================
// 2. Parameters
// ========================================
parameters
    beta gamma omega_bar sigma_squared
    rho_q sigma_q rho_n sigma_n;

// ========================================
// 3. Calibration
// ========================================
beta = 0.995;
gamma = 0.27;
omega_bar = 0.5;
sigma_squared = 0.001;
rho_q = 0.85;
sigma_q = 0.03;
rho_n = 0.70;
sigma_n = 0.01;

// ========================================
// 4. Model Equations (Constraints)
// ========================================
model;
    // Exchange rate
    e = q_tilde + x - z;
    
    // NFA dynamics
    beta * b_star = b_star(-1) - z;
    
    // Risk-sharing constraint
    z(+1) = z + omega_bar * sigma_squared * (n_star + f_star - b_star);
    
    // ToT shock
    q_tilde = rho_q * q_tilde(-1) + eps_q;
    
    // Capital flow shock
    n_star = rho_n * n_star(-1) + eps_n;
    
    // NOTE: No policy rules!
    // f_star and x are chosen optimally by Ramsey planner
end;

// ========================================
// 5. Ramsey Setup (NEW SYNTAX)
// ========================================
// Define instruments (what planner chooses)
ramsey_model(instruments=(f_star, x), planner_discount=beta);

// Define planner's objective (minimize welfare loss)
// Dynare maximizes, so use negative
planner_objective -(gamma * z^2 + (1-gamma) * x^2);

// ========================================
// 6. Initial Values
// ========================================
initval;
    z = 0;
    x = 0;
    e = 0;
    b_star = 0;
    q_tilde = 0;
    n_star = 0;
    f_star = 0;
end;

// Compute Ramsey steady state
steady(solve_algo=4);

// ========================================
// 7. Shocks
// ========================================
shocks;
    var eps_q = sigma_q^2;
    var eps_n = sigma_n^2;
end;

// ========================================
// 8. Solve (NEW SYNTAX - separate commands)
// ========================================
// First solve the model
stoch_simul(order=1, irf=40, nograph);

// Then evaluate planner objective
evaluate_planner_objective;