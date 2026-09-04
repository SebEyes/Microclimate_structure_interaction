#---------------------------------
# 0. Package Loading
#---------------------------------
rm(list = ls())

if(!requireNamespace("pacman", quietly = TRUE)){
    install.packages("pacman")
}

pacman::p_load(
    tidyverse,
    reshape2,
    tidyplots,
    patchwork,
    mgcv,
    nlme,
    lme4,
    lmerTest,
    broom.mixed,
    car,
    e1071,
    glmmTMB,
    see
)