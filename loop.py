import re
import os

import pandas as pd

txt_path = 'path'
print("Files and directories in the specified path:")

dates = []
blob_column = []
sent_column = []
        
for filename in os.listdir(txt_path):
    f = os.path.join(txt_path, filename)
    if os.path.isfile(f):
        setting_data = open(f, 'r')
        lines = setting_data.readlines()
        limited_n_ints = ''

        for i in lines:
            limited_n_ints = limited_n_ints + i

        p = re.compile("[-+]?([0-9]*\. [0-9]+|[0-9]+)")
        new_sen = p.sub("",limited_n_ints)
        #print (new_sen) 
        
        cl_path = project_dir/'models'/'classifier_model'/'finbert-sentiment'
        model = AutoModelForSequenceClassification.from_pretrained(cl_path, cache_dir=None, num_labels=3)
        
        # getting the results from the BERT language model 
        result = predict(new_sen, model)
        
        # preparing data and then gauging sentiment with less sophisticated NLP algorithm (allegedly)
        blob = TextBlob(new_sen)
        result['textblob_prediction'] = [sentence.sentiment.polarity for sentence in blob.sentences]

        #print(f'Average sentiment is %.2f.' % (result.sentiment_score.mean()))
        #print(f'Blob average sentiment score is %.2f.' % (result.textblob_prediction.mean())

        dates.append(filename)
        blob_column.append(result.textblob_prediction.mean())
        sent_column.append(result.sentiment_score.mean())

df = pd.DataFrame(list(zip(dates, sent_column, blob_column), columns = ['DATE', 'Bert Score', 'Blob Column']))

df.head()




        


