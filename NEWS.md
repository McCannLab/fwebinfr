# fwebinfr devel 

* Add an implementation of the niche model (see `fw_niche_model()`).
* Add `fw_create()` to create objects of class `fwmod`.
* Add `fw_model()` to select model (only one currently available).
* Export `fw_get_A_predicted()` and `fw_get_B_predicted()`.
* Add vignette "Get started with fwebinfr".
* `fw_infer()` gains arguments `sdB` to dealt with biomass variation and `...`
to pass further argument to `limSolve::xsample()`.


# fwebinfr 0.0.1

* `fw_infer()` performs LIM for a system described by a generalized Lotka-Voltera equation set.
* `fw_gener()` generates parameter values.
* `fw_list_experim()` list available numerical experiments.
