# fwebinfr devel 

* Add `fw_range_plot()` to plot interaction strength and stability ranges.
* Add `fw_ode_plot()` to visualize ODE integration.
* Add an implementation of the niche model (see `fw_niche_model()`).
* Add `fw_problem()` to create objects of class `fw_problem`.
* Add `fw_model()` to create `fw_model` objects (only one model currently available).
* Export `fw_predict_A()` and `fw_predict_B()`.
* Add vignettes "Get started with fwebinfr" and "Use fwebinfr to infer interactions with 5 species".
* `fw_infer()` gains arguments `sdB` to dealt with biomass variation and `...`
to pass further argument to `limSolve::xsample()`.


# fwebinfr 0.0.1

* `fw_infer()` performs LIM for a system described by a generalized Lotka-Voltera equation set.
* `fw_gener()` generates parameter values.
* `fw_list_experim()` list available numerical experiments.
