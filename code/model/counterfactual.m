%% counterfactual_welfare.m
%  Simulates recovered shocks through different policy regimes
%  and computes conditional welfare losses for the 2020-2021 episode.
%
%  Requires: smoothed_shocks.mat (from extract_smoothed_shocks.m)
%  Requires: Regime .mod files with stoch_simul(order=1)

clear; close all;

%% 1. Load recovered shocks
shocks = load('smoothed_shocks.mat');
eps_q_full = shocks.eps_q;
eps_n_full = shocks.eps_n;
T_full = shocks.T;

fprintf('Loaded %d quarters of shocks\n', T_full);

%% 2. Define episode window
start_year = shocks.dates_start(1);
start_qtr  = shocks.dates_start(2);

% Find 2020Q1 index
target_start = (2020 - start_year)*4 + (1 - start_qtr) + 1;
target_end = min(T_full, target_start + 16 - 1);  % 16 quarters
T_sim = target_end - target_start + 1;

eps_q_episode = eps_q_full(target_start:target_end);
eps_n_episode = eps_n_full(target_start:target_end);

fprintf('Episode: %d quarters starting from index %d\n', T_sim, target_start);
fprintf('Simulating 2020Q1 to ~2023Q4\n');

%% 3. Define regimes
% NOTE: These .mod files need stoch_simul(order=1,...) for decision rules.
% If your files use order=2, create order=1 versions or temporarily change them.
regimes = {
    'IM_peru_1b',           'First-Best';
    'IM_peru_crawling_peg', 'Crawling Peg (BCRP)';
    'IM_peru_free',         'Free Float';
    'IM_peru_fixed',        'Fixed Peg';
    'IM_peru_fixed_v2',     'Fixed Peg v2';
};

n_regimes = size(regimes, 1);

%% 4. Simulate each regime
results = struct('name', {}, 'welfare_loss', {}, 'welfare_bps', {}, ...
    'z_path', {}, 'x_path', {}, 'e_path', {}, 'f_path', {}, ...
    'n_path', {}, 'b_path', {}, 'success', {});

for r = 1:n_regimes
    modfile = regimes{r, 1};
    regime_name = regimes{r, 2};
    
    fprintf('\n========================================\n');
    fprintf('Simulating: %s (%s)\n', regime_name, modfile);
    fprintf('========================================\n');
    
    % Run Dynare
    try
        evalin('base', sprintf('dynare %s noclearall', modfile));
    catch ME
        fprintf('ERROR running %s: %s\n', modfile, ME.message);
        results(r).name = regime_name;
        results(r).success = false;
        continue;
    end
    
    oo_ = evalin('base', 'oo_');
    M_  = evalin('base', 'M_');
    
    % Check if decision rules exist
    if ~isfield(oo_, 'dr') || ~isfield(oo_.dr, 'ghx')
        fprintf('WARNING: No decision rules for %s. Need stoch_simul(order=1).\n', modfile);
        results(r).name = regime_name;
        results(r).success = false;
        continue;
    end
    
    dr = oo_.dr;
    
    % Variable indices (declaration order)
    z_idx = find(strcmp(M_.endo_names, 'z'));
    x_idx = find(strcmp(M_.endo_names, 'x'));
    e_idx = find(strcmp(M_.endo_names, 'e'));
    n_idx = find(strcmp(M_.endo_names, 'n_star'));
    b_idx = find(strcmp(M_.endo_names, 'b_star'));
    
    % f_star may not exist in all regimes
    f_idx = find(strcmp(M_.endo_names, 'f_star'));
    if isempty(f_idx); f_idx = 0; end
    
    % Shock indices
    eq_idx = find(strcmp(M_.exo_names, 'eps_q'));
    en_idx = find(strcmp(M_.exo_names, 'eps_n'));
    
    if isempty(eq_idx) || isempty(en_idx)
        fprintf('WARNING: Shock names do not match for %s\n', modfile);
        fprintf('Available shocks: ');
        disp(M_.exo_names);
        results(r).name = regime_name;
        results(r).success = false;
        continue;
    end
    
    % Decision rule components
    ghx = dr.ghx;
    ghu = dr.ghu;
    order_var = dr.order_var;
    nstatic = M_.nstatic;
    npred   = M_.npred;
    ny = M_.endo_nbr;
    nex = M_.exo_nbr;
    
    state_idx_dr = (nstatic+1):(nstatic+npred);
    
    % Simulate
    y_sim = zeros(ny, T_sim);
    yhat_prev = zeros(npred, 1);
    
    for t = 1:T_sim
        eps_t = zeros(nex, 1);
        eps_t(eq_idx) = eps_q_episode(t);
        eps_t(en_idx) = eps_n_episode(t);
        
        y_dr = ghx * yhat_prev + ghu * eps_t;
        
        y_decl = zeros(ny, 1);
        y_decl(order_var) = y_dr;
        y_sim(:, t) = y_decl;
        
        yhat_prev = y_dr(state_idx_dr);
    end
    
    % Extract paths
    z_path = y_sim(z_idx, :);
    x_path = y_sim(x_idx, :);
    e_path = y_sim(e_idx, :);
    n_path = y_sim(n_idx, :);
    b_path = y_sim(b_idx, :);
    if f_idx > 0
        f_path = y_sim(f_idx, :);
    else
        f_path = zeros(1, T_sim);
    end
    
    % Welfare loss
    gamma = M_.params(find(strcmp(M_.param_names, 'gamma')));
    beta  = M_.params(find(strcmp(M_.param_names, 'beta')));
    
    beta_vec = beta.^(0:T_sim-1)';
    welfare_loss = 0.5 * sum(beta_vec .* (gamma * z_path'.^2 + (1-gamma) * x_path'.^2));
    welfare_bps = welfare_loss * 10000;
    
    % Store
    results(r).name = regime_name;
    results(r).welfare_loss = welfare_loss;
    results(r).welfare_bps = welfare_bps;
    results(r).z_path = z_path;
    results(r).x_path = x_path;
    results(r).e_path = e_path;
    results(r).f_path = f_path;
    results(r).n_path = n_path;
    results(r).b_path = b_path;
    results(r).success = true;
    
    fprintf('Welfare loss: %.6f (%.2f bps)\n', welfare_loss, welfare_bps);
end

%% 4b. BCRP Actual: use smoother paths + AR(1) n*
% Load smoother results
evalin('base', 'dynare IM_peru_smoother noclearall');
oo_sm = evalin('base', 'oo_');

% Smoother starts at different date — recompute index
sm_start_year = 2000;  % ADJUST to match your smoother
sm_start_qtr  = 2;     % ADJUST
sm_target_start = (2020 - sm_start_year)*4 + (1 - sm_start_qtr) + 1;
sm_target_end = sm_target_start + T_sim - 1;

z_actual = oo_sm.SmoothedVariables.z(sm_target_start:sm_target_end);
x_actual = zeros(size(z_actual));
e_actual = oo_sm.SmoothedVariables.e(sm_target_start:sm_target_end);
f_actual = oo_sm.SmoothedVariables.f_star(sm_target_start:sm_target_end);
b_actual = oo_sm.SmoothedVariables.b_star(sm_target_start:sm_target_end);

% Compute welfare from smoother's z path
gamma = 0.27; beta = 0.995;
beta_vec_act = beta.^(0:T_sim-1)';
W_actual = 0.5 * sum(beta_vec_act .* (gamma * z_actual(:).^2 + (1-gamma) * x_actual(:).^2));

% Simulate n* from eps_n (pure AR(1), consistent with counterfactuals)
rho_n = 0.9285;
n_star_sim = zeros(T_sim, 1);
for t = 1:T_sim
    if t == 1
        n_star_sim(t) = eps_n_episode(t);
    else
        n_star_sim(t) = rho_n * n_star_sim(t-1) + eps_n_episode(t);
    end
end

% Store in results
r_actual = length(results) + 1;
results(r_actual).name = 'BCRP Actual';
results(r_actual).welfare_loss = W_actual;
results(r_actual).welfare_bps = W_actual * 10000;
results(r_actual).z_path = z_actual(:)';
results(r_actual).x_path = x_actual(:)';
results(r_actual).e_path = e_actual(:)';
results(r_actual).f_path = f_actual(:)';
results(r_actual).n_path = n_star_sim(:)';
results(r_actual).b_path = b_actual(:)';
results(r_actual).success = true;

fprintf('\nBCRP Actual welfare loss: %.6f (%.2f bps)\n', W_actual, W_actual*10000);

%% 5. Summary table
fprintf('\n\n');
fprintf('================================================================\n');
fprintf('COUNTERFACTUAL WELFARE COMPARISON: 2020-2021 EPISODE\n');
fprintf('================================================================\n');
fprintf('%-25s %15s %15s\n', 'Regime', 'Welfare Loss', 'Basis Points');
fprintf('%s\n', repmat('-', 1, 55));
for r = 1:length(results)
    if results(r).success
        fprintf('%-25s %15.6f %15.2f\n', results(r).name, ...
            results(r).welfare_loss, results(r).welfare_bps);
    else
        fprintf('%-25s %15s %15s\n', results(r).name, 'FAILED', 'FAILED');
    end
end

% Relative to crawling peg
cp_idx = find(strcmp({results.name}, 'Crawling Peg (BCRP)'));
if ~isempty(cp_idx) && results(cp_idx).success
    fprintf('\n%-25s %15s\n', 'Regime', 'Relative to BCRP');
    fprintf('%s\n', repmat('-', 1, 40));
    for r = 1:length(results)
        if results(r).success
            rel = results(r).welfare_loss / results(cp_idx).welfare_loss;
            fprintf('%-25s %15.2fx\n', results(r).name, rel);
        end
    end
end

%% 6. Plot counterfactual paths
dates_episode = datetime(2020, 1, 1) + calquarters(0:T_sim-1);

sty  = {'b-', 'k-', 'r--', 'm:', 'g-.', 'k--'};
lw   = [2.5, 2.5, 1.5, 1.5, 1.5, 2.0];

figure('Position', [100 100 1400 800]);

% Exchange rate
subplot(2,3,1); hold on; box on; grid on;
for r = 1:length(results)
    if results(r).success
        plot(dates_episode, results(r).e_path(:), sty{r}, 'LineWidth', lw(r));
    end
end
title('Exchange Rate e'); ylabel('deviation from SS');
xline(datetime(2021,1,1), '--k');

% Risk-sharing wedge z
subplot(2,3,2); hold on; box on; grid on;
for r = 1:length(results)
    if results(r).success
        plot(dates_episode, results(r).z_path(:), sty{r}, 'LineWidth', lw(r));
    end
end
title('Risk-sharing Wedge z'); ylabel('deviation from SS');
xline(datetime(2021,1,1), '--k');

% Output gap x
subplot(2,3,3); hold on; box on; grid on;
for r = 1:length(results)
    if results(r).success
        plot(dates_episode, results(r).x_path(:), sty{r}, 'LineWidth', lw(r));
    end
end
title('Output Gap x'); ylabel('deviation from SS');
xline(datetime(2021,1,1), '--k');

% FXI f*
subplot(2,3,4); hold on; box on; grid on;
for r = 1:length(results)
    if results(r).success
        plot(dates_episode, results(r).f_path(:), sty{r}, 'LineWidth', lw(r));
    end
end
title('FX Intervention f^*'); ylabel('fraction of GDP');
xline(datetime(2021,1,1), '--k');

% n*
subplot(2,3,5); hold on; box on; grid on;
for r = 1:length(results)
    if results(r).success
        plot(dates_episode, results(r).n_path(:), sty{r}, 'LineWidth', lw(r));
    end
end
title('Capital Flow n^*'); ylabel('deviation from SS');
xline(datetime(2021,1,1), '--k');

% Cumulative welfare loss
subplot(2,3,6); hold on; box on; grid on;
for r = 1:length(results)
    if results(r).success
        gamma = 0.27; beta = 0.995;
        cum_loss = cumsum(0.5 * beta.^(0:T_sim-1)' .* ...
            (gamma * results(r).z_path(:).^2 + (1-gamma) * results(r).x_path(:).^2));
        plot(dates_episode, cum_loss * 10000, sty{r}, 'LineWidth', lw(r));
    end
end
title('Cumulative Welfare Loss (bps)'); ylabel('bps');
xline(datetime(2021,1,1), '--k');

% Legend
leg_names = {};
for r = 1:length(results)
    if results(r).success
        leg_names{end+1} = results(r).name;
    end
end
legend(leg_names, 'Location', 'best', 'FontSize', 8);

sgtitle('Counterfactual Analysis: 2020-2021 Crisis Episode', 'FontSize', 14);
saveas(gcf, 'counterfactual_paths.png');
fprintf('\nSaved counterfactual_paths.png\n');

save('counterfactual_results.mat', 'results');
fprintf('Saved counterfactual_results.mat\n');