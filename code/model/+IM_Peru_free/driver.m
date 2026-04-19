%
% Status : main Dynare file
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

tic0 = tic;
% Define global variables.
global M_ options_ oo_ estim_params_ bayestopt_ dataset_ dataset_info estimation_info
options_ = [];
M_.fname = 'IM_Peru_free';
M_.dynare_version = '6.5';
oo_.dynare_version = '6.5';
options_.dynare_version = '6.5';
%
% Some global variables initialization
%
global_initialization;
M_.exo_names = cell(2,1);
M_.exo_names_tex = cell(2,1);
M_.exo_names_long = cell(2,1);
M_.exo_names(1) = {'eps_q'};
M_.exo_names_tex(1) = {'eps\_q'};
M_.exo_names_long(1) = {'eps_q'};
M_.exo_names(2) = {'eps_n'};
M_.exo_names_tex(2) = {'eps\_n'};
M_.exo_names_long(2) = {'eps_n'};
M_.endo_names = cell(9,1);
M_.endo_names_tex = cell(9,1);
M_.endo_names_long = cell(9,1);
M_.endo_names(1) = {'z'};
M_.endo_names_tex(1) = {'z'};
M_.endo_names_long(1) = {'z'};
M_.endo_names(2) = {'x'};
M_.endo_names_tex(2) = {'x'};
M_.endo_names_long(2) = {'x'};
M_.endo_names(3) = {'e'};
M_.endo_names_tex(3) = {'e'};
M_.endo_names_long(3) = {'e'};
M_.endo_names(4) = {'b_star'};
M_.endo_names_tex(4) = {'b\_star'};
M_.endo_names_long(4) = {'b_star'};
M_.endo_names(5) = {'q_tilde'};
M_.endo_names_tex(5) = {'q\_tilde'};
M_.endo_names_long(5) = {'q_tilde'};
M_.endo_names(6) = {'n_star'};
M_.endo_names_tex(6) = {'n\_star'};
M_.endo_names_long(6) = {'n_star'};
M_.endo_names(7) = {'f_star'};
M_.endo_names_tex(7) = {'f\_star'};
M_.endo_names_long(7) = {'f_star'};
M_.endo_names(8) = {'i'};
M_.endo_names_tex(8) = {'i'};
M_.endo_names_long(8) = {'i'};
M_.endo_names(9) = {'g'};
M_.endo_names_tex(9) = {'g'};
M_.endo_names_long(9) = {'g'};
M_.endo_partitions = struct();
M_.param_names = cell(8,1);
M_.param_names_tex = cell(8,1);
M_.param_names_long = cell(8,1);
M_.param_names(1) = {'beta'};
M_.param_names_tex(1) = {'beta'};
M_.param_names_long(1) = {'beta'};
M_.param_names(2) = {'gamma'};
M_.param_names_tex(2) = {'gamma'};
M_.param_names_long(2) = {'gamma'};
M_.param_names(3) = {'omega_bar'};
M_.param_names_tex(3) = {'omega\_bar'};
M_.param_names_long(3) = {'omega_bar'};
M_.param_names(4) = {'sigma_squared'};
M_.param_names_tex(4) = {'sigma\_squared'};
M_.param_names_long(4) = {'sigma_squared'};
M_.param_names(5) = {'rho_q'};
M_.param_names_tex(5) = {'rho\_q'};
M_.param_names_long(5) = {'rho_q'};
M_.param_names(6) = {'sigma_q'};
M_.param_names_tex(6) = {'sigma\_q'};
M_.param_names_long(6) = {'sigma_q'};
M_.param_names(7) = {'rho_n'};
M_.param_names_tex(7) = {'rho\_n'};
M_.param_names_long(7) = {'rho_n'};
M_.param_names(8) = {'sigma_n'};
M_.param_names_tex(8) = {'sigma\_n'};
M_.param_names_long(8) = {'sigma_n'};
M_.param_partitions = struct();
M_.exo_det_nbr = 0;
M_.exo_nbr = 2;
M_.endo_nbr = 9;
M_.param_nbr = 8;
M_.orig_endo_nbr = 9;
M_.aux_vars = [];
M_.Sigma_e = zeros(2, 2);
M_.Correlation_matrix = eye(2, 2);
M_.H = 0;
M_.Correlation_matrix_ME = 1;
M_.sigma_e_is_diagonal = true;
M_.det_shocks = [];
M_.surprise_shocks = [];
M_.learnt_shocks = [];
M_.learnt_endval = [];
M_.heteroskedastic_shocks.Qvalue_orig = [];
M_.heteroskedastic_shocks.Qscale_orig = [];
M_.matched_irfs = {};
M_.matched_irfs_weights = {};
options_.linear = false;
options_.block = false;
options_.bytecode = false;
options_.use_dll = false;
options_.ramsey_policy = false;
options_.discretionary_policy = false;
M_.eq_nbr = 9;
M_.ramsey_orig_eq_nbr = 0;
M_.ramsey_orig_endo_nbr = 0;
M_.set_auxiliary_variables = exist(['./+' M_.fname '/set_auxiliary_variables.m'], 'file') == 2;
M_.epilogue_names = {};
M_.epilogue_var_list_ = {};
M_.orig_maximum_endo_lag = 1;
M_.orig_maximum_endo_lead = 1;
M_.orig_maximum_exo_lag = 0;
M_.orig_maximum_exo_lead = 0;
M_.orig_maximum_exo_det_lag = 0;
M_.orig_maximum_exo_det_lead = 0;
M_.orig_maximum_lag = 1;
M_.orig_maximum_lead = 1;
M_.orig_maximum_lag_with_diffs_expanded = 1;
M_.lead_lag_incidence = [
 0 5 14;
 0 6 0;
 0 7 15;
 1 8 0;
 2 9 0;
 3 10 0;
 0 11 0;
 0 12 0;
 4 13 0;]';
M_.nstatic = 3;
M_.nfwrd   = 2;
M_.npred   = 4;
M_.nboth   = 0;
M_.nsfwrd   = 2;
M_.nspred   = 4;
M_.ndynamic   = 6;
M_.dynamic_tmp_nbr = [2; 0; 0; 0; ];
M_.equations_tags = {
  1 , 'name' , 'e' ;
  2 , 'name' , '2' ;
  3 , 'name' , 'z' ;
  4 , 'name' , 'g' ;
  5 , 'name' , 'x' ;
  6 , 'name' , 'i' ;
  7 , 'name' , 'q_tilde' ;
  8 , 'name' , 'n_star' ;
  9 , 'name' , 'f_star' ;
};
M_.mapping.z.eqidx = [1 2 3 6 ];
M_.mapping.x.eqidx = [1 5 ];
M_.mapping.e.eqidx = [1 4 5 6 ];
M_.mapping.b_star.eqidx = [2 3 5 ];
M_.mapping.q_tilde.eqidx = [1 7 ];
M_.mapping.n_star.eqidx = [3 5 8 ];
M_.mapping.f_star.eqidx = [9 ];
M_.mapping.i.eqidx = [6 ];
M_.mapping.g.eqidx = [4 5 ];
M_.mapping.eps_q.eqidx = [7 ];
M_.mapping.eps_n.eqidx = [8 ];
M_.static_and_dynamic_models_differ = false;
M_.has_external_function = false;
M_.block_structure.time_recursive = false;
M_.block_structure.block(1).Simulation_Type = 1;
M_.block_structure.block(1).endo_nbr = 3;
M_.block_structure.block(1).mfs = 3;
M_.block_structure.block(1).equation = [ 7 8 9];
M_.block_structure.block(1).variable = [ 5 6 7];
M_.block_structure.block(1).is_linear = true;
M_.block_structure.block(1).NNZDerivatives = 5;
M_.block_structure.block(1).bytecode_jacob_cols_to_sparse = [1 2 4 5 6 ];
M_.block_structure.block(2).Simulation_Type = 8;
M_.block_structure.block(2).endo_nbr = 2;
M_.block_structure.block(2).mfs = 2;
M_.block_structure.block(2).equation = [ 2 3];
M_.block_structure.block(2).variable = [ 4 1];
M_.block_structure.block(2).is_linear = true;
M_.block_structure.block(2).NNZDerivatives = 6;
M_.block_structure.block(2).bytecode_jacob_cols_to_sparse = [1 3 4 6 ];
M_.block_structure.block(3).Simulation_Type = 8;
M_.block_structure.block(3).endo_nbr = 3;
M_.block_structure.block(3).mfs = 3;
M_.block_structure.block(3).equation = [ 5 4 1];
M_.block_structure.block(3).variable = [ 2 9 3];
M_.block_structure.block(3).is_linear = true;
M_.block_structure.block(3).NNZDerivatives = 7;
M_.block_structure.block(3).bytecode_jacob_cols_to_sparse = [2 4 5 6 9 ];
M_.block_structure.block(4).Simulation_Type = 1;
M_.block_structure.block(4).endo_nbr = 1;
M_.block_structure.block(4).mfs = 1;
M_.block_structure.block(4).equation = [ 6];
M_.block_structure.block(4).variable = [ 8];
M_.block_structure.block(4).is_linear = true;
M_.block_structure.block(4).NNZDerivatives = 1;
M_.block_structure.block(4).bytecode_jacob_cols_to_sparse = [2 ];
M_.block_structure.block(1).g1_sparse_rowval = int32([]);
M_.block_structure.block(1).g1_sparse_colval = int32([]);
M_.block_structure.block(1).g1_sparse_colptr = int32([]);
M_.block_structure.block(2).g1_sparse_rowval = int32([1 1 2 1 2 2 ]);
M_.block_structure.block(2).g1_sparse_colval = int32([1 3 3 4 4 6 ]);
M_.block_structure.block(2).g1_sparse_colptr = int32([1 2 2 4 6 6 7 ]);
M_.block_structure.block(3).g1_sparse_rowval = int32([1 1 3 2 1 3 2 ]);
M_.block_structure.block(3).g1_sparse_colval = int32([2 4 4 5 6 6 9 ]);
M_.block_structure.block(3).g1_sparse_colptr = int32([1 1 2 2 4 5 7 7 7 8 ]);
M_.block_structure.block(4).g1_sparse_rowval = int32([]);
M_.block_structure.block(4).g1_sparse_colval = int32([]);
M_.block_structure.block(4).g1_sparse_colptr = int32([]);
M_.block_structure.variable_reordered = [ 5 6 7 4 1 2 9 3 8];
M_.block_structure.equation_reordered = [ 7 8 9 2 3 5 4 1 6];
M_.block_structure.incidence(1).lead_lag = -1;
M_.block_structure.incidence(1).sparse_IM = [
 2 4;
 5 4;
 5 6;
 5 9;
 7 5;
 8 6;
];
M_.block_structure.incidence(2).lead_lag = 0;
M_.block_structure.incidence(2).sparse_IM = [
 1 1;
 1 2;
 1 3;
 1 5;
 2 1;
 2 4;
 3 1;
 3 4;
 3 6;
 4 9;
 5 2;
 5 3;
 6 1;
 6 3;
 6 8;
 7 5;
 8 6;
 9 7;
];
M_.block_structure.incidence(3).lead_lag = 1;
M_.block_structure.incidence(3).sparse_IM = [
 3 1;
 4 3;
 6 1;
 6 3;
];
M_.block_structure.dyn_tmp_nbr = 2;
M_.state_var = [5 6 4 9 ];
M_.maximum_lag = 1;
M_.maximum_lead = 1;
M_.maximum_endo_lag = 1;
M_.maximum_endo_lead = 1;
oo_.steady_state = zeros(9, 1);
M_.maximum_exo_lag = 0;
M_.maximum_exo_lead = 0;
oo_.exo_steady_state = zeros(2, 1);
M_.params = NaN(8, 1);
M_.endo_trends = struct('deflator', cell(9, 1), 'log_deflator', cell(9, 1), 'growth_factor', cell(9, 1), 'log_growth_factor', cell(9, 1));
M_.NNZDerivatives = [30; -1; -1; ];
M_.dynamic_g1_sparse_rowval = int32([2 5 7 5 8 5 1 2 3 6 1 5 1 5 6 2 3 1 7 3 8 9 6 4 3 6 4 6 7 8 ]);
M_.dynamic_g1_sparse_colval = int32([4 4 5 6 6 9 10 10 10 10 11 11 12 12 12 13 13 14 14 15 15 16 17 18 19 19 21 21 28 29 ]);
M_.dynamic_g1_sparse_colptr = int32([1 1 1 1 3 4 6 6 6 7 11 13 16 18 20 22 23 24 25 27 27 29 29 29 29 29 29 29 30 31 ]);
M_.lhs = {
'e'; 
'beta*b_star'; 
'z(1)'; 
'g'; 
'x'; 
'i'; 
'q_tilde'; 
'n_star'; 
'f_star'; 
};
M_.static_tmp_nbr = [2; 0; 0; 0; ];
M_.block_structure_stat.block(1).Simulation_Type = 1;
M_.block_structure_stat.block(1).endo_nbr = 1;
M_.block_structure_stat.block(1).mfs = 1;
M_.block_structure_stat.block(1).equation = [ 6];
M_.block_structure_stat.block(1).variable = [ 8];
M_.block_structure_stat.block(2).Simulation_Type = 3;
M_.block_structure_stat.block(2).endo_nbr = 1;
M_.block_structure_stat.block(2).mfs = 1;
M_.block_structure_stat.block(2).equation = [ 7];
M_.block_structure_stat.block(2).variable = [ 5];
M_.block_structure_stat.block(3).Simulation_Type = 3;
M_.block_structure_stat.block(3).endo_nbr = 1;
M_.block_structure_stat.block(3).mfs = 1;
M_.block_structure_stat.block(3).equation = [ 8];
M_.block_structure_stat.block(3).variable = [ 6];
M_.block_structure_stat.block(4).Simulation_Type = 1;
M_.block_structure_stat.block(4).endo_nbr = 1;
M_.block_structure_stat.block(4).mfs = 1;
M_.block_structure_stat.block(4).equation = [ 9];
M_.block_structure_stat.block(4).variable = [ 7];
M_.block_structure_stat.block(5).Simulation_Type = 6;
M_.block_structure_stat.block(5).endo_nbr = 2;
M_.block_structure_stat.block(5).mfs = 2;
M_.block_structure_stat.block(5).equation = [ 2 3];
M_.block_structure_stat.block(5).variable = [ 1 4];
M_.block_structure_stat.block(6).Simulation_Type = 6;
M_.block_structure_stat.block(6).endo_nbr = 3;
M_.block_structure_stat.block(6).mfs = 3;
M_.block_structure_stat.block(6).equation = [ 5 1 4];
M_.block_structure_stat.block(6).variable = [ 2 3 9];
M_.block_structure_stat.variable_reordered = [ 8 5 6 7 1 4 2 3 9];
M_.block_structure_stat.equation_reordered = [ 6 7 8 9 2 3 5 1 4];
M_.block_structure_stat.incidence.sparse_IM = [
 1 1;
 1 2;
 1 3;
 1 5;
 2 1;
 2 4;
 3 4;
 3 6;
 4 3;
 4 9;
 5 2;
 5 3;
 5 4;
 5 6;
 5 9;
 6 8;
 7 5;
 8 6;
 9 7;
];
M_.block_structure_stat.tmp_nbr = 1;
M_.block_structure_stat.block(1).g1_sparse_rowval = int32([]);
M_.block_structure_stat.block(1).g1_sparse_colval = int32([]);
M_.block_structure_stat.block(1).g1_sparse_colptr = int32([]);
M_.block_structure_stat.block(2).g1_sparse_rowval = int32([1 ]);
M_.block_structure_stat.block(2).g1_sparse_colval = int32([1 ]);
M_.block_structure_stat.block(2).g1_sparse_colptr = int32([1 2 ]);
M_.block_structure_stat.block(3).g1_sparse_rowval = int32([1 ]);
M_.block_structure_stat.block(3).g1_sparse_colval = int32([1 ]);
M_.block_structure_stat.block(3).g1_sparse_colptr = int32([1 2 ]);
M_.block_structure_stat.block(4).g1_sparse_rowval = int32([]);
M_.block_structure_stat.block(4).g1_sparse_colval = int32([]);
M_.block_structure_stat.block(4).g1_sparse_colptr = int32([]);
M_.block_structure_stat.block(5).g1_sparse_rowval = int32([1 1 2 ]);
M_.block_structure_stat.block(5).g1_sparse_colval = int32([1 2 2 ]);
M_.block_structure_stat.block(5).g1_sparse_colptr = int32([1 2 4 ]);
M_.block_structure_stat.block(6).g1_sparse_rowval = int32([1 2 1 2 3 1 3 ]);
M_.block_structure_stat.block(6).g1_sparse_colval = int32([1 1 2 2 2 3 3 ]);
M_.block_structure_stat.block(6).g1_sparse_colptr = int32([1 3 6 8 ]);
M_.static_g1_sparse_rowval = int32([1 2 1 5 1 4 5 2 3 5 1 7 3 5 8 9 6 4 5 ]);
M_.static_g1_sparse_colval = int32([1 1 2 2 3 3 3 4 4 4 5 5 6 6 6 7 8 9 9 ]);
M_.static_g1_sparse_colptr = int32([1 3 5 8 11 13 16 17 18 20 ]);
M_.params(1) = 0.995;
beta = M_.params(1);
M_.params(2) = 0.27;
gamma = M_.params(2);
M_.params(3) = 100;
omega_bar = M_.params(3);
M_.params(4) = 0.000525;
sigma_squared = M_.params(4);
M_.params(5) = 0.8368;
rho_q = M_.params(5);
M_.params(6) = 0.004747;
sigma_q = M_.params(6);
M_.params(7) = 0.9285;
rho_n = M_.params(7);
M_.params(8) = 0.1544;
sigma_n = M_.params(8);
%
% INITVAL instructions
%
options_.initval_file = false;
oo_.steady_state(1) = 0;
oo_.steady_state(2) = 0;
oo_.steady_state(3) = 0;
oo_.steady_state(4) = 0;
oo_.steady_state(5) = 0;
oo_.steady_state(6) = 0;
oo_.steady_state(7) = 0;
oo_.steady_state(8) = 0;
oo_.steady_state(9) = 0;
if M_.exo_nbr > 0
	oo_.exo_simul = ones(M_.maximum_lag,1)*oo_.exo_steady_state';
end
if M_.exo_det_nbr > 0
	oo_.exo_det_simul = ones(M_.maximum_lag,1)*oo_.exo_det_steady_state';
end
steady;
%
% SHOCKS instructions
%
M_.exo_det_length = 0;
M_.Sigma_e(1, 1) = M_.params(6)^2;
M_.Sigma_e(2, 2) = M_.params(8)^2;
oo_.dr.eigval = check(M_,options_,oo_);
options_.drop = 100;
options_.irf = 40;
options_.nocorr = true;
options_.nodisplay = true;
options_.nograph = true;
options_.noprint = true;
options_.order = 1;
var_list_ = {};
[info, oo_, options_, M_] = stoch_simul(M_, options_, oo_, var_list_);


oo_.time = toc(tic0);
disp(['Total computing time : ' dynsec2hms(oo_.time) ]);
if ~exist([M_.dname filesep 'Output'],'dir')
    mkdir(M_.dname,'Output');
end
save([M_.dname filesep 'Output' filesep 'IM_Peru_free_results.mat'], 'oo_', 'M_', 'options_');
if exist('estim_params_', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'IM_Peru_free_results.mat'], 'estim_params_', '-append');
end
if exist('bayestopt_', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'IM_Peru_free_results.mat'], 'bayestopt_', '-append');
end
if exist('dataset_', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'IM_Peru_free_results.mat'], 'dataset_', '-append');
end
if exist('estimation_info', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'IM_Peru_free_results.mat'], 'estimation_info', '-append');
end
if exist('dataset_info', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'IM_Peru_free_results.mat'], 'dataset_info', '-append');
end
if exist('oo_recursive_', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'IM_Peru_free_results.mat'], 'oo_recursive_', '-append');
end
if exist('options_mom_', 'var') == 1
  save([M_.dname filesep 'Output' filesep 'IM_Peru_free_results.mat'], 'options_mom_', '-append');
end
if ~isempty(lastwarn)
  disp('Note: warning(s) encountered in MATLAB/Octave code')
end
