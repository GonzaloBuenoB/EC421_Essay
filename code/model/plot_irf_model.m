% plot_irfs_fixed_v2.m
% Single regime IRF plots: Fixed Peg v2
% Two figures (one per shock), 2x3 panel layout each.

close all;

%% ── 0. Solve regime ─────────────────────────────────────────────────────
fprintf('\n========================================\n');
fprintf('Solving I-M Model: Fixed Exchange Rate v2\n');
fprintf('========================================\n\n');
evalin('base', 'dynare IM_peru_fixed_v2 noclearall');
results_fx2.oo_ = evalin('base', 'oo_');
results_fx2.M_  = evalin('base', 'M_');

irf_fx2 = results_fx2.oo_.irfs;

%% ── 1. Settings ──────────────────────────────────────────────────────────
H    = 20;
T    = 0:H;
sty  = {'g-.'};
lw   = [1.5];
legs = {'Fixed ER v2'};
grey = [0.6 0.6 0.6];

shock_labels = {'\epsilon_q  (Terms of Trade shock)', ...
                '\epsilon_n  (Capital Flow shock)'};
shocks  = {'eps_q', 'eps_n'};
fnames  = {'IRF_ToT_shock_fixed_v2', 'IRF_CapFlow_shock_fixed_v2'};

%% ── 2. Loop over shocks ──────────────────────────────────────────────────
for s = 1:2
    sk = shocks{s};

    if s == 1; shk_vn = 'q_tilde'; shk_lbl = '\tilde{q}  (ToT)';
    else;       shk_vn = 'n_star';  shk_lbl = 'n^*  (cap. flow)';
    end

    vars = {'e', 'f_star', 'z', 'x', 'b_star', shk_vn};
    irfs = {irf_fx2};
    data = struct();
    for vi = 1:numel(vars)
        vn = vars{vi};
        fn = [vn '_' sk];
        for ri = 1:length(irfs)
            rk = sprintf('r%d', ri);
            if isfield(irfs{ri}, fn)
                data.([vn rk]) = [0, irfs{ri}.(fn)(1:H)];
            else
                data.([vn rk]) = zeros(1, H+1);
            end
        end
    end

    fig = figure('Position', [50 50 1400 800]);

    subplot(2,3,1); hold on; box on; grid on
    for ri = 1:length(irfs)
        plot(T, data.([shk_vn sprintf('r%d',ri)]), sty{ri}, 'LineWidth', lw(ri))
    end
    yline(0, 'Color', grey, 'LineWidth', 0.8)
    title(['Shock variable  ' shk_lbl], 'FontWeight', 'bold')
    ylabel('deviation from SS'); xlabel('Quarters after shock')
    legend(legs, 'Location', 'best', 'FontSize', 8)
    xlim([0 H])

    subplot(2,3,2); hold on; box on; grid on
    for ri = 1:length(irfs)
        plot(T, data.(sprintf('er%d',ri)), sty{ri}, 'LineWidth', lw(ri))
    end
    yline(0, 'Color', grey, 'LineWidth', 0.8)
    title('Exchange Rate  e', 'FontWeight', 'bold')
    ylabel('log-dev. from SS'); xlabel('Quarters after shock')
    xlim([0 H])

    subplot(2,3,3); hold on; box on; grid on
    for ri = 1:length(irfs)
        plot(T, data.(sprintf('f_starr%d',ri)), sty{ri}, 'LineWidth', lw(ri))
    end
    yline(0, 'Color', grey, 'LineWidth', 0.8)
    title('FX Intervention  f^*', 'FontWeight', 'bold')
    ylabel('+ buy / \minus sell FX'); xlabel('Quarters after shock')
    xlim([0 H])

    subplot(2,3,4); hold on; box on; grid on
    for ri = 1:length(irfs)
        plot(T, data.(sprintf('xr%d',ri)), sty{ri}, 'LineWidth', lw(ri))
    end
    yline(0, 'Color', grey, 'LineWidth', 0.8)
    title('Output Gap  x', 'FontWeight', 'bold')
    ylabel('deviation from SS'); xlabel('Quarters after shock')
    xlim([0 H])

    subplot(2,3,5); hold on; box on; grid on
    for ri = 1:length(irfs)
        plot(T, data.(sprintf('zr%d',ri)), sty{ri}, 'LineWidth', lw(ri))
    end
    yline(0, 'Color', grey, 'LineWidth', 0.8)
    title('Risk-sharing Wedge  z', 'FontWeight', 'bold')
    ylabel('deviation from SS'); xlabel('Quarters after shock')
    xlim([0 H])

    subplot(2,3,6); hold on; box on; grid on
    for ri = 1:length(irfs)
        plot(T, data.(sprintf('b_starr%d',ri)), sty{ri}, 'LineWidth', lw(ri))
    end
    yline(0, 'Color', grey, 'LineWidth', 0.8)
    title('Private NFA  b^*', 'FontWeight', 'bold')
    ylabel('deviation from SS'); xlabel('Quarters after shock')
    xlim([0 H])

    sgtitle(['Impulse Responses: Fixed Peg v2 — ' shock_labels{s}], 'FontSize', 13, 'FontWeight', 'bold')

    exportgraphics(fig, ['../../results/' fnames{s} '.pdf'], ...
                   'ContentType', 'vector', 'Resolution', 300)
    fprintf('Saved %s.pdf\n', fnames{s})
end