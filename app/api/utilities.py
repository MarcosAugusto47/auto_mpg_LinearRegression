import numpy as np
import pickle

with open('models/model3.pickle', 'rb') as f:
    model = pickle.load(f)

def predict_mpgl(new_data):
    # Predict diabetes
    predictions = model.predict(new_data)

    #pred_to_label = {0: 'Negative', 1: 'Positive'}

    # Make a list of predictions
    data = []
    for t, pred in zip(new_data, predictions):
        data.append({'prediction': pred[0]})

    return data

if __name__=="__main__":
    # Sample to classify should be in a list.
    new_sample = np.array([1, 13, 3504, 0, 0])
    new_sample = [new_sample]
    predictions = predict_mpgl(new_sample)
    print(predictions)