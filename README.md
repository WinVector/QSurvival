# QSurvival
Quasi observation based survival package for R.

This package supplies an explicit quasi observation implementation of survival models.

One can use this to reproduce Cox's discrete time logistic hazard model, or build a model with constant base hazard rate, or directly control how time enters the model.  You can also change what machine learning algorithm is used make the hazard predictions.  This is a teaching package showing some of the issues in survival modeling.   The best way to approach this package is through the included vignettes (including showing the use of the mgcv package for survival modeling).

To install:

  
    # install.packages('devtools')
    devtools::install_github('WinVector/QSurvival', build_vignettes=TRUE)
