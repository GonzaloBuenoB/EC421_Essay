% plot_irfs_v2.m
% Two figures (one per shock), 2x3 panel layout each.
% Row 1: Shock variable | Exchange rate | FX intervention
% Row 2: Output gap x   | Risk-sharing wedge z | Private NFA b*

close all;

%% ── 0. Solve all regimes ─────────────────────────────────────────────────
results_fb = solve_IM_fixed_point('IM_Peru_1b',           'First-Best');
results_sb = solve_IM_fixed_point('IM_Peru_crawling_peg', 'Crawling Peg');
results_fl = solve_IM_fixed_point('IM_Peru_free',         'Free Float');

fprintf('\n========================================\n');
fprintf('Solving I-M Model: Fixed Exchange Rate\n');
fprintf('========================================\n\n');
evalin('base', 'dynare IM_Peru_fixed noclearall');
results_fx.oo_ = evalin('base', 'oo_');
results_fx.M_  = evalin('base', 'M_');

fprintf('\n========================================\n');
fprintf('Solving I-M Model: Fixed Exchange Rate v2\n');
fprintf('========================================\n\n');
evalin('base', 'dynare IM_Peru_fixed_v2 noclearall');
results_fx2.oo_ = evalin('base', 'oo_');
results_fx2.M_  = evalin('base', 'M_');

irf_fb  = results_fb.oo_.irfs;
irf_sb  = results_sb.oo_.irfs;
irf_fl  = results_fl.oo_.irfs;
irf_fx  = results_fx.oo_.irfs;
irf_fx2 = results_fx2.oo_.irfs;

%% ── 1. Settings ──────────────────────────────────────────────────────────
H    = 20;
T    = 0:H;
sty  = {'b-', 'k-', 'r--', 'm:', 'g-.'};
lw   = [2.5, 2.5, 1.5, 1.5, 1.5];
legs = {'First-Best', 'BCRP (Crawling Peg)', 'Free Float', 'Fixed ER (Mon. Pol.)', 'Fixed ER (FXI)'};
grey = [0.6 0.6 0.6];

shock_labels = {'\epsilon_q  (Terms of Trade shock)', ...
                '\epsilon_n  (Capital Flow shock)'};
shocks  = {'eps_q', 'eps_n'};
fnames  = {'IRF_ToT_shock', 'IRF_CapFlow_shock'};

%% ── 2. Loop over shocks ──────────────────────────────────────────────────
set(0, 'DefaultAxesFontSize', 14);
set(0, 'DefaultTextFontSize', 14);

for s = 1:2
    sk = shocks{s};

    % Shock variable name
    if s == 1; shk_vn = 'q_tilde'; shk_lbl = '$\tilde{q}$ \textbf{(ToT)}';
    else;       shk_vn = 'n_star';  shk_lbl = '$n^*$ \textbf{(cap. flow)}';
    end

    % -- Safe field extraction --
    vars = {'e', 'f_star', 'z', 'x', 'b_star', shk_vn};
    irfs = {irf_fb, irf_sb, irf_fl, irf_fx, irf_fx2};
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
    % First-best output gap is exactly zero by construction
    data.xr1 = zeros(1, H+1);

    % -- Figure --
    fig = figure('Position', [50 50 1500 900]);

    % ── Helper: plot one subplot ─────────────────────────────────────────
    panels = {
        {shk_vn, ['\textbf{Shock variable } ' shk_lbl], 'deviation from SS', true}
        {'e',     '\textbf{Exchange Rate } $e$',          'log-dev. from SS',  false}
        {'f_star','\textbf{FX Intervention } $f^*$',      '+ buy / - sell FX', false}
        {'x',     '\textbf{Output Gap } $x$',             'deviation from SS', false}
        {'z',     '\textbf{Risk-sharing Wedge } $z$',     'deviation from SS', false}
        {'b_star','\textbf{Private NFA } $b^*$',          'deviation from SS', false}
    };

    for p = 1:6
        subplot(2,3,p); hold on; box on; grid on

        vn = panels{p}{1};
        for ri = 1:length(irfs)
            plot(T, data.([vn sprintf('r%d',ri)]), sty{ri}, 'LineWidth', lw(ri))
        end
        yline(0, 'Color', grey, 'LineWidth', 0.8)

        title(panels{p}{2}, 'Interpreter', 'latex', 'FontSize', 16)
        ylabel(panels{p}{3}, 'FontSize', 14)
        xlabel('Quarters after shock', 'FontSize', 14)
        xlim([0 H])

        % Prevent scientific notation clashing with titles
        ax = gca;
        ax.YAxis.Exponent = 0;
        ytickformat('%.4f')

        % Legend only on first panel
        if panels{p}{4}
            legend(legs, 'Location', 'best', 'FontSize', 10)
        end
    end

    % ── Shared title & export ─────────────────────────────────────────────
    sgtitle(['Impulse Responses: ' shock_labels{s}], 'FontSize', 16, 'FontWeight', 'bold')

    % Add spacing so titles don't clash with tick labels
    set(fig, 'Units', 'normalized');
    ha = findobj(fig, 'Type', 'axes');
    for i = 1:length(ha)
        pos = get(ha(i), 'Position');
        pos(4) = pos(4) * 0.88;
        set(ha(i), 'Position', pos);
    end

    exportgraphics(fig, ['../../paper/figures/' fnames{s} '.pdf'], ...
                   'ContentType', 'vector', 'Resolution', 300)
    fprintf('Saved %s.pdf\n', fnames{s})
end

% Reset defaults
set(0, 'DefaultAxesFontSize', 10);
set(0, 'DefaultTextFontSize', 10);