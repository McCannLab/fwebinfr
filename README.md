# fwebinfr

Food web inference.


# Installation

Requires R>=4.2

```{R}
install.packages("remotes")
remotes::install_deps()
```

# How to

One example that comes with figures can be run once the package loaded:

```R
get_fig_exp_1("pres/pres_2023-12-18")
```

```R
pars <- c(
    fw_gen_5sp_2chains_01(FALSE), 
    burninlength = 5000, 
    iter = 5000, 
    type = "mirror"
)
res <- pars |> do.call(what = "fw_infer"); apply(res[, 1:8], 2, log10)
boxplot(res)
```
ok

# Data 

> Citation: GLFC (Great Lakes Fishery Commission). 2022. Commercial fish production in the Great Lakes 1867–2020 [online database]. Great Lakes Fishery Commission, Ann Arbor, Michigan. Available: www.glfc.org/great-lakes-databases.php.


# References

> Gellner G, McCann K, Hastings A. 2023. Stable diverse food webs become more common when interactions are more biologically constrained. Proceedings of the National Academy of Sciences 120:e2212061120. DOI: 10.1073/pnas.2212061120.
