% calibrate_IM_peru.m
%
% Calibrates omega_bar to match Var(e) = sigma_e_target.
% phi is pinned analytically from Std(f*) target.
%
% Approach: for each trial omega_bar, overwrite the parameter lines in
% a working copy of the .mod file, then call solve_IM_fixed_point on it.
% This reuses your existing working infrastructure exactly.

%% ── 0. Targets ───────────────────────────────────────────────────────────
sigma_e_target = 0.000525;
fxi_std_target = 6.703631;

%% ── 1. Fixed parameters (from AR(1) on BCRP data) ────────────────────────
rho_n   = 0.70;
sigma_n = 5.25;

%% ── 2. Analytical phi ────────────────────────────────────────────────────
phi = fxi_std_target * sqrt(1 - rho_n^2) / sigma_n;
fprintf('Analytical phi = %.6f\n\n', phi);

%% ── 3. Source .mod file ──────────────────────────────────────────────────
src_mod  = 'IM_peru_crawling_peg.mod';   % your original
work_mod = 'IM_peru_cal_work';           % working copy (no .mod extension for dynare call)

%% ── 4. Helper: write working .mod with trial omega_bar ───────────────────
    % Reads src_mod, replaces the parameter lines, writes work_mod.mod
write_mod = @(omega_bar_val) write_cal_mod( ...
    src_mod, [work_mod '.mod'], omega_bar_val, phi, rho_n, sigma_n);

%% ── 5. 1D grid over omega_bar ────────────────────────────────────────────
omega_vals = linspace(0.5, 3.0, 20);   % Dima: omega ~ 1

fprintf('%-10s  %12s  %12s  %10s\n', 'omega_bar', 'Var(e)', 'Std(f*)', 'loss');
fprintf('%s\n', repmat('-',1,50));

best_loss  = Inf;
best_omega = NaN;

for ov = omega_vals
    [loss, ve, sf] = eval_omega(ov, write_mod, work_mod, ...
                                sigma_e_target, fxi_std_target);
    marker = '';
    if ~isnan(ve) && loss < best_loss; marker = '  *'; end
    fprintf('%-10.4f  %12.6f  %12.4f  %10.5f%s\n', ov, ve, sf, loss, marker);
    if loss < best_loss
        best_loss  = loss;
        best_omega = ov;
    end
end

fprintf('\nBest grid point: omega_bar = %.4f  (loss = %.6f)\n\n', ...
    best_omega, best_loss);

%% ── 6. Polish with fminsearch ────────────────────────────────────────────
obj  = @(o) eval_omega(o, write_mod, work_mod, sigma_e_target, fxi_std_target);
opts = optimset('Display','iter','TolX',1e-5,'TolFun',1e-7,'MaxFunEvals',100);
[omega_opt, loss_opt] = fminsearch(obj, best_omega, opts);
[~, ve_opt, sf_opt]   = eval_omega(omega_opt, write_mod, work_mod, ...
                                   sigma_e_target, fxi_std_target);

fprintf('\n========================================\n');
fprintf('CALIBRATION RESULTS\n');
fprintf('========================================\n');
fprintf('omega_bar = %.6f\n', omega_opt);
fprintf('phi       = %.6f  (analytical)\n', phi);
fprintf('rho_n     = %.6f  (fixed)\n', rho_n);
fprintf('sigma_n   = %.6f  (fixed)\n', sigma_n);
fprintf('\n%-20s %12s %12s\n', 'Moment', 'Target', 'Model');
fprintf('%-20s %12.6f %12.6f\n', 'Var(e)',  sigma_e_target, ve_opt);
fprintf('%-20s %12.4f %12.4f\n', 'Std(f*)', fxi_std_target, sf_opt);
fprintf('\nLoss = %.8f\n', loss_opt);

%% ── 7. Single evaluation: runs solve_IM_fixed_point on work_mod ──────────
function [loss, var_e, std_f] = eval_omega(omega_bar, write_mod, work_mod, ...
                                            sigma_e_target, fxi_std_target)
    var_e = NaN; std_f = NaN;

    if omega_bar < 0.5 || omega_bar > 5   % hard bounds matching grid range
        loss = 1e6; return
    end

    % Write .mod file with this omega_bar
    write_mod(omega_bar);

    % Run fixed-point (your existing function)
    try
        results = solve_IM_fixed_point(work_mod, sprintf('cal_omega%.4f', omega_bar));
    catch
        loss = 1e6; return
    end

    if ~results.converged
        loss = 1e6; return
    end

    oo_ = results.oo_;
    M_  = results.M_;

    e_idx = find(strcmp(M_.endo_names, 'e'));
    f_idx = find(strcmp(M_.endo_names, 'f_star'));

    var_e = oo_.var(e_idx, e_idx);
    std_f = sqrt(oo_.var(f_idx, f_idx));

    loss = ((var_e - sigma_e_target) / sigma_e_target)^2;
end

%% ── 8. Write calibration .mod file ──────────────────────────────────────
function write_cal_mod(src, dst, omega_bar, phi, rho_n, sigma_n)
    % Read source
    fid = fopen(src, 'r');
    txt = fread(fid, '*char')';
    fclose(fid);

    % Replace parameter lines using regex
    % Matches:  param_name   = <number>;
    txt = regexprep(txt, 'omega_bar\s*=\s*[^;]+;', ...
        sprintf('omega_bar     = %.10f;', omega_bar));
    txt = regexprep(txt, 'phi\s*=\s*[^;]+;', ...
        sprintf('phi           = %.10f;', phi));
    txt = regexprep(txt, 'rho_n\s*=\s*[^;]+;', ...
        sprintf('rho_n         = %.10f;', rho_n));
    txt = regexprep(txt, 'sigma_n\s*=\s*[^;]+;', ...
        sprintf('sigma_n       = %.10f;', sigma_n));

    % Write destination
    fid = fopen(dst, 'w');
    fprintf(fid, '%s', txt);
    fclose(fid);
end