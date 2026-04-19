% solve_IM_fixed_point.m - FIXED VERSION

function results = solve_IM_fixed_point(modfile, regime)
    
    fprintf('\n========================================\n');
    fprintf('Solving I-M Model: %s regime\n', regime);
    fprintf('========================================\n\n');
    
    % Fixed-point iteration parameters

    % Run Dynare once to get initial parameters
    fprintf('Loading initial parameters from %s.mod...\n', modfile);
    evalin('base', sprintf('dynare %s noclearall', modfile));
    
    % Extract parameters from M_ structure
    M_ = evalin('base', 'M_');
    
    % Find sigma_squared parameter index
    sigma_idx = find(strcmp(M_.param_names, 'sigma_squared'));

    sigma_squared = M_.params(sigma_idx);
    tolerance = 1e-7;
    max_iter = 50;
    damping = 0.4;
    
    sigma_squared_history = zeros(max_iter, 1);
    
    for iter = 1:max_iter
        fprintf('--- Iteration %d ---\n', iter);
        fprintf('Current sigma_squared = %.8f\n', sigma_squared);
        
        % Set parameter in base workspace
        assignin('base', 'sigma_squared', sigma_squared);
        
        % Run Dynare in base workspace
        evalin('base', ['dynare ', char(modfile), ' noclearall']);
        
        % CRITICAL FIX: Check if oo_ exists in base workspace
        if evalin('base', 'exist(''oo_'', ''var'')') == 0
            error('Dynare did not create oo_ structure!');
        end
        
        % Get oo_ and M_ from base workspace
        oo_ = evalin('base', 'oo_');
        M_ = evalin('base', 'M_');
        
        % Debug: Check what oo_ contains
        if iter == 1
            fprintf('\n=== DEBUG: Checking oo_ structure ===\n');
            fprintf('oo_ is empty: %d\n', isempty(oo_));
            fprintf('oo_ fields:\n');
            if ~isempty(oo_)
                disp(fieldnames(oo_));
            else
                error('oo_ is empty! Dynare did not solve properly.');
            end
            
            % Check if var field exists
            if isfield(oo_, 'var')
                fprintf('oo_.var exists, size: %s\n', mat2str(size(oo_.var)));
            else
                fprintf('WARNING: oo_.var does not exist!\n');
            end
            fprintf('=====================================\n\n');
        end
        
        % Find variable indices
        e_idx = find(strcmp(M_.endo_names, 'e'));
        z_idx = find(strcmp(M_.endo_names, 'z'));
        x_idx = find(strcmp(M_.endo_names, 'x'));
        f_idx = find(strcmp(M_.endo_names, 'f_star'));
        
        % Get variance from oo_.var (theoretical moments)
        if isfield(oo_, 'var') && ~isempty(oo_.var)
            % sigma_squared_new = oo_.var(e_idx, e_idx);
            var_e = oo_.var(e_idx, e_idx);
            rho_e = oo_.autocorr{1}(e_idx, e_idx);
            sigma_squared_new = 2 * var_e * (1 - rho_e);
        else
            error('oo_.var not computed! Check stoch_simul options in .mod file');
        end
        
        fprintf('Implied sigma_squared = %.8f\n', sigma_squared_new);
        fprintf('Difference = %.8f\n', abs(sigma_squared_new - sigma_squared));
        
        sigma_squared_history(iter) = sigma_squared;
        
        % Check convergence
        if abs(sigma_squared_new - sigma_squared) < tolerance
            fprintf('\n✓ CONVERGED in %d iterations!\n', iter);
            fprintf('Final sigma_squared = %.8f\n\n', sigma_squared);
            
            % Store results
            results.converged = true;
            results.iterations = iter;
            results.sigma_squared = sigma_squared;
            results.sigma_squared_history = sigma_squared_history(1:iter);
            results.oo_ = oo_;
            results.M_ = M_;
            
            % Compute welfare
            var_z   = oo_.var(z_idx, z_idx);
            var_x   = oo_.var(x_idx, x_idx);
            var_fxi = oo_.var(f_idx, f_idx);
            gamma = 0.27;
            results.welfare_loss = 0.5 * (gamma * var_z + (1-gamma) * var_x);
            
            % Get phi
            phi_idx = find(strcmp(M_.param_names, 'phi'));
            phi_val = M_.params(phi_idx);
            
            fprintf('=== FINAL RESULTS ===\n');
            fprintf('Regime: %s\n', regime);
            fprintf('Welfare Loss = %.6f\n', results.welfare_loss);
            fprintf('Std(z) = %.4f\n', sqrt(var_z));
            fprintf('Std(x) = %.4f\n', sqrt(var_x));
            fprintf('Std(fxi) = %.4f\n', sqrt(var_fxi));
            fprintf('Std(e) = %.4f\n', sqrt(sigma_squared));
            fprintf('sigma^2 (fixed point)  = %.6f\n', sigma_squared);
            fprintf('Var(e) level from model = %.6f\n', oo_.var(e_idx, e_idx));
            fprintf('Var(Δe) from model     = %.6f\n', 2 * oo_.var(e_idx, e_idx) * (1 - oo_.autocorr{1}(e_idx, e_idx)));
            fprintf('phi    = %.6f\n', phi_val);
            
            return;
        end
        
        % Update with damping
        sigma_squared = damping * sigma_squared_new + (1-damping) * sigma_squared;
        fprintf('\n');
    end
    
    warning('Did not converge after %d iterations', max_iter);
    results.converged = false;
    results.iterations = max_iter;
end