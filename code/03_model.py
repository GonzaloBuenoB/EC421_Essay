"""
Itskhoki-Mukhin (2023) Optimal Exchange Rate Policy
Linearized system (equations 11-13) applied to Peru

The system:
    (i)   e_t = q_t + x_t - z_t              # exchange rate
    (ii)  beta * b_t = b_{t-1} - z_t          # NFA dynamics
    (iii) E[dz_{t+1}] = w * s2 * (n_t + f_t - b_t)  # risk-sharing
    (iv)  s2 = var(de_{t+1})                  # ER volatility (endogenous)

Welfare loss:
    L = 0.5 * E sum beta^t [gamma * z_t^2 + (1-gamma) * x_t^2]
"""

import numpy as np
import matplotlib.pyplot as plt


class ITMModel:
    """Itskhoki-Mukhin small open economy model."""

    def __init__(self, params_file='code/params.json'):
        """
        Load parameters from JSON file.
        
        Expected keys in JSON:
            beta      : discount factor (quarterly)
            gamma     : openness / share of tradables
            omega_bar : price of FX risk (financial friction)
        """
        import json
        with open(params_file) as f:
            p = json.load(f)
        
        self.beta = p['beta']
        self.gamma = p['gamma']
        self.omega_bar = p['omega_bar']
        self.params = p  # store full dict for access to shock params etc.

    def exchange_rate(self, q, x, z):
        """Eq (i): e_t = q_t + x_t - z_t"""
        return q + x - z

    def nfa_dynamics(self, b_prev, z):
        """Eq (ii): beta * b_t = b_{t-1} - z_t  =>  b_t = (b_{t-1} - z_t) / beta"""
        return (b_prev - z) / self.beta

    def risk_sharing(self, sigma2, n, f, b):
        """Eq (iii): E[dz_{t+1}] = omega_bar * sigma2 * (n + f - b)"""
        return self.omega_bar * sigma2 * (n + f - b)

    def welfare_loss(self, z_path, x_path):
        """L = 0.5 * sum beta^t [gamma * z_t^2 + (1-gamma) * x_t^2]"""
        T = len(z_path)
        betas = self.beta ** np.arange(T)
        return 0.5 * np.sum(betas * (self.gamma * z_path**2 + (1 - self.gamma) * x_path**2))

    def solve_first_best(self, q_path, n_path):
        """
        Proposition 1: Both instruments unconstrained.
        Solution: x_t = 0, z_t = 0, f_t = -n_t, e_t = q_t
        
        This is the benchmark — BCRP perfectly offsets all financial shocks.
        """
        T = len(q_path)
        x = np.zeros(T)
        z = np.zeros(T)
        f = -n_path.copy()          # FXI fully offsets noise
        b = np.zeros(T)             # NFA stays at zero
        e = q_path.copy()           # exchange rate = natural RER
        sigma2 = np.var(np.diff(q_path))  # volatility = only fundamental vol

        return {
            'x': x, 'z': z, 'f': f, 'b': b, 'e': e,
            'sigma2': sigma2,
            'welfare_loss': self.welfare_loss(z, x)
        }

    def solve_no_intervention(self, q_path, n_path, tol=1e-8, max_iter=500):
        """
        No FXI case: f_t = 0 for all t.
        Must solve for {z_t, b_t, e_t, sigma2} with the fixed-point on sigma2.
        
        Monetary policy is inward-looking (discretionary): x_t = 0.
        So the only wedge is z_t from unhedged capital flows.
        """
        T = len(q_path)
        b = self.beta
        w = self.omega_bar
        f = np.zeros(T)
        x = np.zeros(T)  # discretionary monetary policy => x = 0

        # --- Fixed-point iteration on sigma2 ---
        sigma2 = np.var(np.diff(q_path))  # initial guess: fundamental vol

        for iteration in range(max_iter):
            z = np.zeros(T)
            b_star = np.zeros(T)

            for t in range(T - 1):
                # Risk-sharing condition gives E[dz_{t+1}]
                E_dz = w * sigma2 * (n_path[t] + f[t] - b_star[t])

                # z_{t+1} = z_t + E[dz_{t+1}] + surprise
                # For now, assume E[dz] is realized (no surprises in this deterministic pass)
                z[t + 1] = z[t] + E_dz

                # NFA dynamics
                b_star[t + 1] = (b_star[t] - z[t + 1]) / self.beta

            # Exchange rate path
            e = q_path + x - z

            # Compute implied volatility
            de = np.diff(e)
            sigma2_new = np.var(de) if len(de) > 0 else 0.0

            # Check convergence
            if abs(sigma2_new - sigma2) < tol:
                break
            sigma2 = sigma2_new

        return {
            'x': x, 'z': z, 'f': f, 'b': b_star, 'e': e,
            'sigma2': sigma2,
            'welfare_loss': self.welfare_loss(z, x),
            'iterations': iteration + 1
        }


# =============================================================
# EXERCISE: Compare first-best vs no-intervention for Peru
# =============================================================

if __name__ == "__main__":

    # --- Setup ---
    m = ITMModel()  # reads from code/params.json

    T = 40  # 40 quarters = 10 years
    np.random.seed(42)

    # Generate shock paths (AR(1) processes) from params
    rho_q = m.params['rho_q']
    sig_q = m.params['sigma_q']
    rho_n = m.params['rho_n']
    sig_n = m.params['sigma_n']

    q = np.zeros(T)
    n = np.zeros(T)
    for t in range(1, T):
        q[t] = rho_q * q[t-1] + sig_q * np.random.randn()
        n[t] = rho_n * n[t-1] + sig_n * np.random.randn()

    # --- Solve both cases ---
    fb = m.solve_first_best(q, n)
    ni = m.solve_no_intervention(q, n)

    # --- Print results ---
    print("=" * 50)
    print("ITSKHOKI-MUKHIN MODEL: PERU")
    print(f"Parameters: beta={m.beta}, gamma={m.gamma}, omega_bar={m.omega_bar}")
    print("=" * 50)
    print(f"\n{'':>25} {'First-Best':>12} {'No FXI':>12}")
    print(f"  {'Welfare loss':>23} {fb['welfare_loss']:12.6f} {ni['welfare_loss']:12.6f}")
    print(f"  {'ER volatility (σ²)':>23} {fb['sigma2']:12.6f} {ni['sigma2']:12.6f}")
    print(f"  {'S.D. of z (risk-share)':>23} {np.std(fb['z']):12.6f} {np.std(ni['z']):12.6f}")
    print(f"  {'S.D. of x (output gap)':>23} {np.std(fb['x']):12.6f} {np.std(ni['x']):12.6f}")
    print(f"  {'S.D. of e (exch rate)':>23} {np.std(fb['e']):12.6f} {np.std(ni['e']):12.6f}")
    print(f"  {'Mean |f*| (FXI size)':>23} {np.mean(np.abs(fb['f'])):12.6f} {np.mean(np.abs(ni['f'])):12.6f}")
    if 'iterations' in ni:
        print(f"  {'Fixed-point iterations':>23} {'---':>12} {ni['iterations']:>12}")

    # --- Plot ---
    fig, axes = plt.subplots(2, 3, figsize=(14, 8))
    quarters = np.arange(T)

    # Shocks
    axes[0, 0].plot(quarters, q, 'b-', label='q̃ (ToT)')
    axes[0, 0].plot(quarters, n, 'r--', label='n* (capital flows)')
    axes[0, 0].set_title('Exogenous Shocks')
    axes[0, 0].legend()
    axes[0, 0].axhline(0, color='k', lw=0.5)

    # Exchange rate
    axes[0, 1].plot(quarters, fb['e'], 'b-', label='First-best')
    axes[0, 1].plot(quarters, ni['e'], 'r--', label='No FXI')
    axes[0, 1].set_title('Exchange Rate (eₜ)')
    axes[0, 1].legend()
    axes[0, 1].axhline(0, color='k', lw=0.5)

    # Risk-sharing wedge
    axes[0, 2].plot(quarters, fb['z'], 'b-', label='First-best')
    axes[0, 2].plot(quarters, ni['z'], 'r--', label='No FXI')
    axes[0, 2].set_title('Risk-sharing Wedge (zₜ)')
    axes[0, 2].legend()
    axes[0, 2].axhline(0, color='k', lw=0.5)

    # FX intervention
    axes[1, 0].plot(quarters, fb['f'], 'b-', label='First-best')
    axes[1, 0].plot(quarters, ni['f'], 'r--', label='No FXI')
    axes[1, 0].set_title('FX Intervention (f*ₜ)')
    axes[1, 0].legend()
    axes[1, 0].axhline(0, color='k', lw=0.5)

    # NFA
    axes[1, 1].plot(quarters, fb['b'], 'b-', label='First-best')
    axes[1, 1].plot(quarters, ni['b'], 'r--', label='No FXI')
    axes[1, 1].set_title('Net Foreign Assets (b*ₜ)')
    axes[1, 1].legend()
    axes[1, 1].axhline(0, color='k', lw=0.5)

    # Output gap
    axes[1, 2].plot(quarters, fb['x'], 'b-', label='First-best')
    axes[1, 2].plot(quarters, ni['x'], 'r--', label='No FXI')
    axes[1, 2].set_title('Output Gap (xₜ)')
    axes[1, 2].legend()
    axes[1, 2].axhline(0, color='k', lw=0.5)

    for ax in axes.flat:
        ax.set_xlabel('Quarter')

    plt.suptitle('Itskhoki-Mukhin Model: First-Best vs No FXI (Peru calibration)', fontsize=13)
    plt.tight_layout()
    plt.savefig('code/model_comparison.png', dpi=150)
    plt.show()

    print("\nPlot saved to code/model_comparison.png")