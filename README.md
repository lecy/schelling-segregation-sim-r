# schelling-segregation-sim-r

Simulation of Thomas Shelling's Housing Segregation Model in R

> Schelling, T. C. (1971). Dynamic models of segregation. Journal of mathematical sociology, 1(2), 143-186.

```r
library(ggplot2)
library(dplyr)

lecy <- "https://raw.githubusercontent.com/lecy/"
repo <- "schelling-segregation-sim-r/refs/heads/main/"
code <- "seg-mod-sim.R"
source( paste0( lecy, repo, code ) )

sim <- 
  run_simulation(
    width = 50, 
    height = 50, 
    empty_ratio = 0.5, 
    similarity_threshold = 0.7, 
    n_iterations = 1000  )

# SHOW RESULTS 
i <- 2
plot_grid( sim[[i]], iteration=i ) 

# ANIMATE
par( ask=TRUE )
for( i in 1:length(sim) )
{ plot_grid( sim[[i]], iteration=i ) }
```

![image](https://github.com/user-attachments/assets/57d2e98a-e151-45b8-a146-839046c8349f)

