# Lab 2

- Data preprocessing included cleaning NaN, empty, non-numeric values from the dataframe. Then, it removed outliers from the dataframe based on the 1.5 IQR rule.
- For Models 1-3, Price had a tall y-axis with data point in a low band around the x-axis. Based on this, I determined that it needed to be transformed to log(Price).
- Model 1 was BEDS & BATH. Model 2 was BEDS & PROPERTYSQFT. Model 3 was BATH & PROPERTYSQFT.
- In each model, the most significant contributor to log(Price) was given by the largest t-stat printed by summary(lin.modX)