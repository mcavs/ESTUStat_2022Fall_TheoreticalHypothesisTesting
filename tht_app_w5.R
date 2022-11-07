# In a refrigerator manufacturing company, the breakdown times 
# of a component were tested and the following observation 
# values are collected on a decade scale.

# The production engineer wants to test whether the average 
# breakdown time of this component is 5 years.

times <- c(0.364, 0.137, 0.234, 0.381, 0.126, 0.355, 0.683, 0.285,
           0.030, 0.077, 0.797, 0.292, 0.141, 0.258, 1.012, 0.687,
           0.197, 0.096, 0.075, 1.817, 0.516, 0.652, 0.268, 0.403,
           0.485, 0.406, 0.351, 0.668, 1.023, 0.031, 0.076, 0.122,
           0.271, 0.402, 0.132, 0.263, 0.320, 1.930, 0.032, 0.492,
           0.265, 0.502, 0.220, 0.511, 0.377, 0.001, 0.029, 0.286,
           1.431, 0.642)
 
t.test(data2, mu = 0.5)
wilcox.test(data2, mu = 0.5)