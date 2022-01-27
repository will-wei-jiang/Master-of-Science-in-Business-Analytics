# Read the data from S3 backet
import pandas as pd
df = pd.read_csv('s3://671happy-yulingai/final_review.csv',error_bad_lines=False,encoding='iso-8859-1')
 
# filter and clean the data
df = df.iloc[:, [0,1,2,3,4,7,8]]

# remove the useless part of movie reviews from html
df.review = df.review.apply(lambda x: x.split('<div class="text show-more__control">')[1])
print(df.head())


# create a table in AWS Dynamodb and assign the hash key and range key
import time
import boto3
from decimal import Decimal
import json


client = boto3.client('dynamodb', region_name='us-east-1')

try:
    resp = client.create_table(
        TableName="movie_review",
        # Declare your Primary Key in the KeySchema argument
        KeySchema=[
            {
                "AttributeName": "tconst",
                "KeyType": "HASH"
            },
            {
                "AttributeName": "review_id",
                "KeyType": "RANGE"
            }
        ],
        # Any attributes used in KeySchema or Indexes must be declared in AttributeDefinitions
        AttributeDefinitions=[
            {
                "AttributeName": "tconst",
                "AttributeType": "S"
            },
            {
                "AttributeName": "review_id",
                "AttributeType": "N"
            }
        ],
        # ProvisionedThroughput controls the amount of data you can read or write to DynamoDB per second.
        # You can control read and write capacity independently.
        ProvisionedThroughput={
            "ReadCapacityUnits": 1,
            "WriteCapacityUnits": 1
        }
    )
    print("Table created successfully!")
except Exception as e:
    print("Error creating table:")
    print(e)
 

import time
time.sleep(10)

# upload the data into the Dynamodb
columns = df.columns
dynamodb = boto3.resource('dynamodb', region_name='us-east-1')
table = dynamodb.Table('movie_review')
for index, row in df.iterrows():
    chunk = dict(zip(columns, row))
    ddb_data = json.loads(json.dumps(chunk), parse_float=Decimal)
    table.put_item(Item=ddb_data)


# another method to upload data
#import awswrangler as wr

#def float_to_decimal(num):
#    return Decimal(str(num))

#def pandas_to_dynamodb(df):
#    df = df.fillna(0)
    # convert any floats to decimals
#    for i in df.columns:
#        datatype = df[i].dtype
#        if datatype == 'float64':
#            df[i] = df[i].apply(float_to_decimal)
    # write to dynamodb
#    wr.dynamodb.put_df(df=df, table_name='movie_review')

#pandas_to_dynamodb(df)