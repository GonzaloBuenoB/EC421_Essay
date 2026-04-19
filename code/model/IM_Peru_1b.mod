// IM_peru_fp.mod - Itskhoki-Mukhin model for Peru with Fixed-Point
// ========================================

// ========================================
// 1. Variables
// ========================================
var
    z           // risk-sharing wedge
    x           // output gap
    e           // exchange rate
    b_star      // net foreign assets
    q_tilde     // natural RER (exogenous)
    n_star      // capital flow shock (exogenous)
    f_star;     // FX intervention (policy)

varexo
    eps_q       // ToT shock
    eps_n;      // capital flow shock

// ========================================
// 2. Parameters
// ========================================
parameters
    beta            // discount factor
    gamma           // openness
    omega_bar       // FX risk price
    sigma_squared   // ER volatility (updated by outer loop)
    rho_q           // ToT persistence
    sigma_q         // ToT shock volatility
    rho_n           // capital flow persistence
    sigma_n;        // capital flow shock volatility

// ========================================
// 3. Calibration
// ========================================
beta          = 0.995;
gamma         = 0.27;
omega_bar     = 100;//1/0.000525;
sigma_squared = 0.000525;   // Starting value; updated by outer fixed-point loop
rho_q         = 0.8368;
sigma_q       = 0.004747;
rho_n         = 0.9285;
sigma_n       = 0.1544;//0.05532; 

// ========================================
// 4. Model equations
// ========================================
model;
    // (i) Exchange rate determination
    e = q_tilde + x - z;
    
    // (ii) NFA dynamics
    beta * b_star = b_star(-1) - z;
    
    // (iii) Risk-sharing condition with financial friction
    z(+1) = z + omega_bar * sigma_squared * (n_star + f_star - b_star);
    
    // (iv) Output gap
    x = 0;
    
    // (v) Shocks (AR1 processes)
    q_tilde = rho_q * q_tilde(-1) + eps_q;
    n_star = rho_n * n_star(-1) + eps_n;
    
    // (vi) Policy rule: First-best
    f_star = -n_star;

end;

// ========================================
// 5. Initial values (steady state)
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

steady;

// ========================================
// 6. Shock variances
// ========================================
shocks;
    var eps_q = sigma_q^2;
    var eps_n = sigma_n^2;
end;

// ========================================
// 7. Simulation (called by outer loop)
// ========================================
check;
stoch_simul(
    order=2,
    irf=40,
    drop=100,
    nograph,
    nodisplay,
    nocorr,
    noprint
);