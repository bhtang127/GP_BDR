# Gaussian Process Distribution Regression

Experiment codes for Gaussian process distribution regression paper. 

`GPDR_Matern.R` holds the model codes for our method.

`SimulationDP.R` is the main file for the simulation analysis, except results for `BDR` method in [Law2018](#1).

`NHANES_experiment_CV.R` is the main file for the real data analysis on NHANES activity data, except the results for method in [Law2018](#1).

`BDR/BDR_Simulation.R` is the main file for the simulation analysis for `BDR` method in [Law2018](#1).

`BDR/BDR_NHANES_Experiments.R` is the main file for NHANES activity data analysis for the method in [Law2018](#1).

`plot_simulation.R` is the code visualizing the simulation results.

`NHANES.RData` is the data used for NHANES data analysis. It can be replicated using the `vignettes/NHANES_accelerometry_introduction.Rmd` file in R package [rnhanesdata](https://github.com/andrew-leroux/rnhanesdata). `NHANES.RData` holds variables `Act_Analysis, data_analysis, Flags_Analysis` produced by that file.


## References
<a id="1">[1]</a> 
Law, H. C. L., Sutherland, D. J., Sejdinovic, D., & Flaxman, S. (2018, March). Bayesian approaches to distribution regression. In International Conference on Artificial Intelligence and Statistics (pp. 1167-1176). PMLR.
