# diabetes-classification
Machine Learning application that predicts the log of miles per gallon compsution of a car. The dataset used and some description can be found at https://archive.ics.uci.edu/ml/datasets/auto+mpg.

The application returns the prediction via a POST request, that needs to be structured like this:
```
{
    "Intercept": 1,
    "age": 13,
    "weight": 3504,
    "Japan": 0,
    "Europe": 0
}
```  
More samples can be set via the POST request.

The application uses Docker and can be runned via the command (the image is published in my Docker Hub account):  
`docker run -p5000:5000 marcosaugusto47/auto-mpg-app`