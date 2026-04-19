% solve_ramsey_fixed_point.m - UPDATED FOR DYNARE 6.5

function results = solve_ramsey_fixed_point(modfile, regime_name)
    
    fprintf('\n════════════════════════════════════════════════════\n');
    fprintf('  RAMSEY OPTIMAL POLICY: %s\n', regime_name);
    fprintf('  File: %s.mod\n', modfile);
    fprintf('════════════════════════════════════════════════════\n\n');
    
    % Initial parameters
    sigma_squared = 0.001;
    tolerance = 1e-6;
    max_iter = 30;
    damping = 0.5;
    
    for iter = 1:max_iter
        fprintf('[Iter %2d] σ² = %.6f → ', iter, sigma_squared);
        
        % Set parameter
        assignin('base', 'sigma_squared', sigma_squared);
        
        % Run Ramsey
        try
            evalin('base', sprintf('dynare %s noclearall', modfile));
        catch ME
            fprintf('FAILED: %s\n', ME.message);
            if iter == 1
                error('Ramsey failed on first iteration');
            end
            % Try to continue with damped update
            sigma_squared = sigma_squared * 0.9;
            continue;
        end
        
        % Get results
        oo_ = evalin('base', 'oo_');
        M_ = evalin('base', 'M_');
        
        % Check if variance exists
        if ~isfield(oo_, 'var')
            fprintf('No variance computed\n');
            continue;
        end
        
        % Get implied volatility
        e_idx = find(strcmp(M_.endo_names, 'e'));
        sigma_squared_new = oo_.var(e_idx, e_idx);
        
        fprintf('%.6f (Δ=%.2e)\n', sigma_squared_new, abs(sigma_squared_new - sigma_squared));
        
        % Convergence check
        if abs(sigma_squared_new - sigma_squared) < tolerance
            fprintf('\n✓ CONVERGED in %d iterations!\n\n', iter);
            
            % Extract results
            z_idx = find(strcmp(M_.endo_names, 'z'));
            x_idx = find(strcmp(M_.endo_names, 'x'));
            f_idx = find(strcmp(M_.endo_names, 'f_star'));
            
            results.regime = regime_name;
            results.converged = true;
            results.iterations = iter;
            results.sigma_squared = sigma_squared;
            results.welfare_loss = 0.5 * (0.27 * oo_.var(z_idx,z_idx) + 0.73 * oo_.var(x_idx,x_idx));
            results.std_z = sqrt(oo_.var(z_idx, z_idx));
            results.std_x = sqrt(oo_.var(x_idx, x_idx));
            results.std_e = sqrt(sigma_squared);
            results.std_f = sqrt(oo_.var(f_idx, f_idx));
            results.oo_ = oo_;
            
            % Display
            fprintf('┌────────────────────────────────────────────────┐\n');
            fprintf('│  RAMSEY OPTIMAL POLICY                         │\n');
            fprintf('├────────────────────────────────────────────────┤\n');
            fprintf('│  Welfare Loss    = %23.6f │\n', results.welfare_loss);
            fprintf('│  Std(z)          = %23.4f │\n', results.std_z);
            fprintf('│  Std(x)          = %23.4f │\n', results.std_x);
            fprintf('│  Std(e)          = %23.4f │\n', results.std_e);
            fprintf('│  Std(f*)         = %23.4f │\n', results.std_f);
            fprintf('│  σ²              = %23.6f │\n', sigma_squared);
            fprintf('└────────────────────────────────────────────────┘\n\n');
            
            if isfield(oo_, 'planner_objective_value')
                %fprintf('Planner objective value: %.6f\n\n', oo_.planner_objective_value);
            end
            
            return;
        end
        
        % Update
        sigma_squared = damping * sigma_squared_new + (1-damping) * sigma_squared;
        sigma_squared = max(sigma_squared, 1e-6);
        sigma_squared = min(sigma_squared, 0.1);
    end
    
    warning('No convergence after %d iterations', max_iter);
    results.converged = false;
end