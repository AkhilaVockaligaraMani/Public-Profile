# Programming Assignment 03 - Perceptron
# Mani, Akhila Vockaligara [221475]

#Importing Libraries
import csv
import pandas as pd
import numpy as np

#Activation Function
def activation(wtx):
    if wtx > 0:
        return 1
    else:
        return 0

#Read the dataset and Split into X and Y
sheet_name = input('Enter the sheet name here: ')
try:
	dataset = pd.read_table(sheet_name, header = None)
except:
	print("\nFile with name {} doesn't exist!\n".format(sheet_name))
	exit()

out_file_name = input('Enter the output file name here: ')
if '.tsv' not in out_file_name:
    print("\nFile with name {} not supported! Only .tsv as output\n".format(sheet_name))
    exit()

x = dataset.iloc[:, 1:].values
y = dataset.iloc[:, 0].values

if str(x[:, -1][0]) == 'nan':
    x = x[:, :-1]

#Insert Bias into X
x = np.insert(x, 0, np.ones(len(x)), axis = 1)
learning_rate0 = 1

#Initialize Weights
weights = np.array([0, 0, 0])

#Map A --> 1 and B --> 0
for c in range(len(y)):
    if y[c] == 'A':
        y[c] = 1
    else:
        y[c] = 0
        
#Main loop
count = 0
thr_pass = 1
tsv_row_items = []
tsv_row_items2 = []
            
while(count < 101 and thr_pass <= 2):
    
    #Calculating y_hat
    y_hat = []
    for i in range(len(x)):
        y_hat.append(activation(sum([w * x for w, x in list(zip(weights, x[i]))])))
        
    #Printing out misclassified rate
    if thr_pass == 1:
        tsv_row_items.append(np.sum(y != y_hat))
    elif thr_pass == 2:
        tsv_row_items2.append(np.sum(y != y_hat))
    
    #Calculating gradient and updating weights
    errors = [(y - y_hat) for y_hat, y in zip(y_hat, y)]
    temp = []
    for wght in range(len(weights)):
        gradient = sum(errors * x[:, wght])
        if thr_pass == 1:
            temp.append(weights[wght] + learning_rate0 * gradient)
        elif thr_pass == 2:
            temp.append(weights[wght] + learning_rate0/(count+1) * gradient)
    weights = temp
    
    count+=1
    
    #Iterating through two cases
    if count > 100 and thr_pass == 1:
        weights = np.array([0, 0, 0])
        count = 0
        thr_pass += 1
        
    if count > 100 and thr_pass == 2:
        with open(out_file_name, 'wt') as out_file:
            tsv_writer = csv.writer(out_file, delimiter='\t')
            tsv_writer.writerow(tsv_row_items)
            tsv_writer.writerow(tsv_row_items2)