Intelligent forecasting is the process of predicting future events and trends based on the analysis of historical and current data using artificial intelligence and machine learning methods. It is applied in various fields to improve decision-making: in finance, supply chains, healthcare, and so on.

A number of methods [1] for improving the accuracy of intelligent forecasting are described in many scientific papers: shift correction methods, data fusion, data assimilation, sequential methods, 3D-Var, ensemble forecasting, Monte Carlo simulation, and multi-model ensembles.

Therefore, this study aims to improve forecasting performance through the optimal selection of a multi-layer ensemble structure. Ensemble structures effectively combine different models, thereby reducing the risk of underfitting and overfitting. There are three main ensemble learning techniques: bagging, boosting, and stacking. Bagging uses aggregation and bootstrapping for models with low bias and high variance. Boosting trains models in stages, correcting the errors of the previous model to reduce bias and improve accuracy. Stacking combines the predictions of weak models into a meta-model to improve performance, reducing bias or variance depending on the type of meta-model.

To create multi-layer heterogeneous ensemble regressors, six potentially effective regression models were selected: based on multiple linear regression (MLR), decision tree (DT-R), random forest (RF-R), support vector machine (SVR), k-nearest neighbors (KNN-R), and artificial neural network (ANN-R).

In the first stage of modelling, a classical approach was used, combining base models into ensembles with a multi-layer structure. Under-trained models with low variance and high bias, as well as a relatively low R² value, were added to the first stacking layer. The results from the first layer are fed as input to the second stacking layer, where overtrained models with high variance and low bias are applied. The results of the classical approach are shown in Table 1.

Table 1 – Results of the classical approach to forming a two-layer ensemble structure
| Model | R² | RMSE | MAE | MAPE (%) |
|---|---:|---:|---:|---:|
| DT-R | 0.88 | 0.0026 | 0.0021 | 1.04 |
| KNN-R | 0.88 | 0.0026 | 0.0020 | 1.03 |
| **Stacking** | **0.92** | **0.0021** | **0.0016** | **0.80** |
| MLR | 0.93 | 0.0020 | 0.0016 | 0.79 |
| RF-R | 0.96 | 0.0016 | 0.0011 | 0.54 |
| SVR | 0.94 | 0.0019 | 0.0014 | 0.69 |
| ANN-R | 0.91 | 0.0022 | 0.0017 | 0.86 |
| **Bagging** | **0.95** | **0.0017** | **0.0012** | **0.61** |

Consequently, the random forest-based regression model outperformed the ensemble structure. This is due to the specific nature of the energy consumption forecasting task and the characteristics of the data. It was therefore decided to select an experimental combination of ensemble structures with the aim of improving the quality of the forecast values.

For the experimental combination of models, an approach was used involving under-trained models at the first ‘bagging’ level and over-trained models at the second ‘stacking’ level. This decision is based on the hypothesis that under-trained models, owing to their lower specificity in data fitting, can more effectively capture general trends and reduce the risk of overfitting. At the first level, they are applied in the bagging layer, which improves the ensemble’s robustness by combining the results of several weakly correlated models. At the second level, the overfitted base models and the bagging results are integrated using stacking. This ensures more accurate forecasting, as stacking utilizes the results of the initial forecasting to further train the models, enabling them to deepen their understanding of the relationships within the data. The results of the experimental variant are shown in Table 2.

Table 2 – Results of the experimental variant for forming a two-layer ensemble structure
| Model | R² | RMSE | MAE | MAPE (%) |
|---|---:|---:|---:|---:|
| DT-R | 0.88 | 0.0026 | 0.0021 | 1.04 |
| KNN-R | 0.88 | 0.0026 | 0.0020 | 1.03 |
| **Bagging** | **0.89** | **0.0025** | **0.0019** | **0.96** |
| MLR | 0.93 | 0.0020 | 0.0016 | 0.79 |
| RF-R | 0.96 | 0.0016 | 0.0011 | 0.54 |
| SVR | 0.94 | 0.0019 | 0.0014 | 0.69 |
| ANN-R | 0.91 | 0.0022 | 0.0017 | 0.86 |
| **Stacking** | **0.96** | **0.0015** | **0.0010** | **0.51** |

Thus, the results of the experimental variant identified—involving a combination of basic regression models within a two-level ensemble structure—indicate a significant improvement in the quality of forecasting.

Scientific research can be found in "Матеріали всеукраїнської науково-практичної конференції молодих вчених, аспірантів і студентів": https://dspace.chmnu.edu.ua/jspui/handle/123456789/2631.
