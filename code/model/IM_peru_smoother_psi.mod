// IM_peru_smoother.mod
// ====================
// Crawling peg model with FXI policy error for Kalman smoother shock recovery.
// Uses order=1 (required by calib_smoother).
//
// Observables: q_tilde, psi, f_star  (3 obs <-> 3 shocks)
//   psi = Δe_{t+1} - (i_t - i*_t) = ex-post UIP deviation
//   This directly reflects the friction: psi ≈ -ω̄σ²(n* + f* - b*)
//
// Shocks:      eps_q (ToT), eps_n (capital flow), eps_f (FXI policy error)

// ========================================
// 1. Variables
// ========================================
var
    z           // risk-sharing wedge
    x           // output gap
    e           // exchange rate (log level)
    de          // first difference of e
    psi         // UIP residual: psi = de - i (ex-post UIP deviation)
    b_star      // net foreign assets
    q_tilde     // natural RER (exogenous, pass-through adjusted)
    n_star      // capital flow shock process (exogenous)
    f_star      // FX intervention (fraction of GDP, demeaned)
    i           // interest rate differential
    g;          // E_t[e_{t+1}], auxiliary expectation variable

varexo
    eps_q       // ToT shock
    eps_n       // capital flow shock
    eps_f;      // FXI policy error

// ========================================
// 2. Parameters
// ========================================
parameters
    beta
    gamma
    omega_bar
    sigma_squared
    rho_q
    sigma_q
    rho_n
    sigma_n
    phi
    sigma_f;

// ========================================
// 3. Calibration
// ========================================
beta          = 0.995;
gamma         = 0.27;
omega_bar     = 100;
sigma_squared = 0.000525;
rho_q         = 0.8368;
sigma_q       = 0.004747;
rho_n         = 0.9285;
sigma_n       = 0.1450;
phi           = 0.06703631*sqrt(1 - rho_n^2)/sigma_n;
sigma_f       = 0.006703631;

// ========================================
// 4. Model equations
// ========================================
model(linear);

    // (i) Exchange rate determination
    e = q_tilde + x - z;

    // (ii) First difference of e
    de = e - e(-1);

    // (iii) NFA dynamics
    beta * b_star = b_star(-1) - z;

    // (iv) Risk-sharing condition
    z(+1) = z + omega_bar * sigma_squared * (n_star + f_star - b_star);

    // (v) Expectation auxiliary
    g = e(+1);

    // (vi) Output gap: zero at first order
    x = 0;

    // (vii) Interest rate differential
    //       i = E_t[Δe_{t+1}] + E_t[Δz_{t+1}]
    i = e(+1) - e + z(+1) - z;

    // (viii) UIP residual (observable)
    //        psi_t = Δe_t - i_{t-1}
    //        In data: psi = Δe_{t+1} - (i_t - i*_t)
    //        In model: psi = de - i(-1)
    psi = (e(+1) - e) - i;

    // (ix) Shocks (AR1)
    q_tilde = rho_q * q_tilde(-1) + eps_q;
    n_star  = rho_n * n_star(-1)  + eps_n;

    // (x) FXI rule with policy error
    f_star = -phi * (n_star) + eps_f;

end;

// ========================================
// 5. Steady state
// ========================================
initval;
    z       = 0;
    x       = 0;
    e       = 0;
    de      = 0;
    psi     = 0;
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
    var eps_f = sigma_f^2;
end;

// ========================================
// 7. Check BK conditions
// ========================================
check;

// ========================================
// 8. Observables and Kalman smoother
// ========================================
// psi = UIP residual = Δe_{t+1} - (i_t - i*_t), demeaned quarterly
// q_tilde = HP-filtered ToT cycle * pass-through beta
// f_star = FXI / GDP, demeaned
varobs q_tilde psi f_star;

calib_smoother(datafile=peru_smoother_data);
