# ssahealthriskfactors

This repository contains the replication files for my master's thesis at the Department of Statistics, University of Munich.

*Identification of Health Risk Factors in Developing Countries using Intrinsic Model Selection Approaches*


**Abstract**

In low- and middle-income countries, nationally representative household surveys such as the Demographic and Health Surveys provide a wealth of primary data on health, nutrition, and socio-economic outcomes. For epidemiological studies, the survey data is often drawn upon to identify health risk factors, both at the individual and geographical levels. In practice, the functional form of the risk factors is not known beforehand. For instance, an effect could be linear or non-linear, if included at all. Furthermore, the increased availability of remotely sensed data provides a new data source that can be integrated into the analyses of health conditions but is not necessarily informative. The increased dimensionality of such analyses demands methods of variable selection and model choice, both to remain interpretable and generalise well to future observations. In this thesis, I employ component-wise boosting to identify risk factors of two prevalent health conditions in sub-Saharan Africa. The approach is applied in two case studies, where risk factors of individual-level outcomes of chronic childhood malnutrition and environmental correlates of the geographic prevalence of malaria are modelled. The flexible estimation of linear, non-linear and spatial effects is found to be central in the understanding of both outcomes, even improving on other non-parametric models in terms of predictive capacity. When estimating malaria risk, component-wise boosting allows for response distributions that account for excess variability at the cluster level while being superior in interpretability compared to competing approaches proposed in the literature on predictive disease mapping.


**Replication**

To replicate the analyses done here, you first need to request access to the microdata from the Demographic and Health Survey Program (DHS) [here](https://dhsprogram.com/). Then set the following environment variables in your R project: 

```
email="yourmail@mail.com"
project="yourprojectname"
```
To set up the API of the `rdhs` package run `src/configs/rdhs.R`. The files to download all required data are in the folder `src/data/`. For the malaria risk mapping of Mali, Data from the [Google Earth Engine](https://earthengine.google.com/) has to be retrieved. See also [here](https://developers.google.com/earth-engine/tutorials/community/intro-to-python-api) for an introduction. The Python environment is documented in `requirements.txt` and can be used to set up a local virtual environment. Finally, the data exported from the Google Earth Engine has to be manually copied into the folder `data/raw/earthengine`. 

Each analysis has separate files to prepare the microdata and remote sensing data, those have to be executed first. After all `src/analysis/*country*/*` files were run, the figures can be created with the files in `src/figures`. 


**References**

(Only main references, see also report.)

P. Bühlmann and T. Hothorn, ‘Boosting Algorithms: Regularization, Prediction and Model Fitting’, Statist. Sci., vol. 22, no. 4, Nov. 2007, doi: 10.1214/07-STS242.

N. Fenske, T. Kneib, and T. Hothorn, ‘Identifying Risk Factors for Severe Childhood Malnutrition by Boosting Additive Quantile Regression’, Journal of the American Statistical Association, vol. 106, no. 494, pp. 494–510, Jun. 2011, doi: 10.1198/jasa.2011.ap09272.

T. Kneib, T. Hothorn, and G. Tutz, ‘Variable Selection and Model Choice in Geoadditive Regression Models’, Biometrics, vol. 65, no. 2, pp. 626–634, Jun. 2009, doi: 10.1111/j.1541-0420.2008.01112.x.

J. Thomas, A. Mayr, B. Bischl, M. Schmid, A. Smith, and B. Hofner, ‘Gradient boosting for distributional regression: faster tuning and improved variable selection via noncyclical updates’, Stat Comput, vol. 28, no. 3, pp. 673–687, May 2018, doi: 10.1007/s11222-017-9754-6.

T. Q. Dong and J. Wakefield, ‘Modeling and presentation of vaccination coverage estimates using data from household surveys’, Vaccine, vol. 39, no. 18, pp. 2584–2594, Apr. 2021, doi: 10.1016/j.vaccine.2021.03.007.

Institut National de la Statistique (INSTAT) and ICF, ‘Enquête Démographique et de Santé à Madagascar (EDSMD-V) 2021’, Antananarivo, Madagascar et Rockville, Maryland, USA, 2022. [Online]. Available: https://www.dhsprogram.com/pubs/pdf/FR376/FR376.pdf

Institut National de la Statistique (INSTAT), Programme National de Lutte contre le Paludisme (PNLP), and The DHS Program, ‘Enquête sur les Indicateurs du Paludisme au Mali 2021’, Bamako, Mali et Rockville, Maryland, USA, 2022. [Online]. Available: https://www.dhsprogram.com/pubs/pdf/FR376/FR376.pdf
