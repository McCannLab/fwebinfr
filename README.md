# fwebinfr
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Food web inference.


# Installation

Requires R>=4.2

```{R}
install.packages("remotes")
remotes::install_deps()
```

# How to

```R
val <- fw_example_2species()
pars <- c(
    val, 
    burninlength = 5000, 
    iter = 5000, 
    type = "mirror"
)
res <- pars |> 
    do.call(what = "fw_infer")
val$B
get_B_from_res(res[1, 1:3], val$A, val$R)
```


One example that comes with figures can be run once the package loaded:

```R
get_fig_exp_1("pres/pres_2023-12-18")
```


```R
val <- fw_gen_parms_exper2()
pars <- c(
    val, 
    burninlength = 5000, 
    iter = 5000, 
    type = "mirror"
)
res <- pars |> do.call(what = "fw_infer")  
apply(res[, 1:10], 2, log10) |> 
    boxplot()
val$B  
get_B_from_res(res[1, 1:10], val$A, val$R)
#----- 3 species
val <- fw_gen_3sp_01()
quick_ode_plot(
  mod_lv_fr1, rep(10, 3), seq(1, 50, 0.1), pars = val
)
pars <- c(
    val, 
    burninlength = 5000, 
    iter = 5000, 
    type = "mirror"
)
res <- pars |> do.call(what = "fw_infer")  
apply(res[, 1:5], 2, log10) |> boxplot()
val$B  
get_B_from_res(res[1, 1:5], val$A, val$R)
val_res <- val
val_res$A <- get_A_from_res(res[1, 1:5], val$A)
quick_ode_plot(
  mod_lv_fr1, rep(10, 3), seq(1, 50, 0.1), pars = val_res
)
val_res$A <- get_A_from_res(res[1, 1:5], val$A)
quick_ode_plot(
  mod_lv_fr1, rep(10, 3), seq(1, 50, 0.1), pars = val_res
)
```


# Data 

> Citation: GLFC (Great Lakes Fishery Commission). 2022. Commercial fish production in the Great Lakes 1867–2020 [online database]. Great Lakes Fishery Commission, Ann Arbor, Michigan. Available: www.glfc.org/great-lakes-databases.php.


# References

> Gellner G, McCann K, Hastings A. 2023. Stable diverse food webs become more common when interactions are more biologically constrained. Proceedings of the National Academy of Sciences 120:e2212061120. DOI: 10.1073/pnas.2212061120.
