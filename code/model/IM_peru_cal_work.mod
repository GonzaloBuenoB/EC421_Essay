// IM_peru_crawling_peg.mod - Itskhoki-Mukhin model for Peru: Crawling Peg
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
    f_star      // FX intervention (constrained to zero)
    i
    g;          // E_t[e_{t+1}], auxiliary expectation variable

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
    sigma_squared   // ER volatility (updated by outer fixed-point loop)
    rho_q           // ToT persistence
    sigma_q         // ToT shock volatility
    rho_n           // capital flow persistence
    sigma_n         // capital flow shock volatility
    phi;            // FXI intervention intensity

// ========================================
// 3. Calibration
// ========================================
beta          = 0.995;
gamma         = 0.27;
omega_bar     = 0.5000000000;
sigma_squared = 0.000525;   // Starting value; updated by outer fixed-point loop
rho_q         = 0.8368;
sigma_q       = 0.038368;
rho_n         = 0.7000000000;
sigma_n       = 5.2500000000;       5.25
phi           = 0.9118762095;

// ========================================
// 4. Model equations
// ========================================
model;
    // Model-local delta: predetermined at t-1, used at t
    // Equivalent to delta_t-1 in Prop 4 notation
    #delta = (2*gamma*omega_bar/(1-gamma)) *
             (omega_bar*sigma_squared/(1+beta+omega_bar*sigma_squared)) *
             (n_star(-1) + f_star(-1) - b_star(-1))^2;

    // (i) Exchange rate determination
    e = q_tilde + x - z;

    // (ii) NFA dynamics
    beta * b_star = b_star(-1) - z;

    // (iii) Risk-sharing condition
    z(+1) = z + omega_bar * sigma_squared * (n_star + f_star - b_star);

    // (iv) Expectation auxiliary: g_t = E_t[e_{t+1}]
    g = e(+1);

    // (v) Output gap — crawling peg lean
    //     x_t = -delta_{t-1} * (e_t - E_{t-1}[e_t])
    x = -delta * (e - g(-1));
    i = e(+1) - e + z(+1) - z;

    // (vi) Shocks (AR1)
    q_tilde = rho_q * q_tilde(-1) + eps_q;
    n_star  = rho_n * n_star(-1)  + eps_n;

    // (vii) FXI: constrained to zero
    f_star = -phi* n_star;  // 0.845

end;

// ========================================
// 5. Steady state
// ========================================
initval;
    z       = 0;
    x       = 0;
    e       = 0;
    b_star  = 0;
    q_tilde = 0;
    n_star  = 0;
    f_star  = 0;
    i       = 0;
    g       = 0;
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
// 7. Simulation
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