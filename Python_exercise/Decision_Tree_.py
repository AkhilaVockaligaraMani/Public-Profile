
#program to implement a decision tree based on ID3 algorithm

#import required libraries

import numpy as np
import pandas as pd
import sys, getopt
import xml.etree.cElementTree as ET


#This will calculate entropy of any attribute required and will be called accordingly

def entropy_of_class(reqd_col, labels):
    number_of_classes = len(labels)
    prob = reqd_col / sum(reqd_col)
    entropy = -(prob) * np.log(prob)/np.log(number_of_classes)    
    total_entropy = entropy.sum()
    return total_entropy

'''This will give the attribute at any level by calculating entropy using entropy function and also with respect to class
and will give information gain'''

def best_attr(dataframe, labels):
    Target_list = pd.DataFrame(dataframe['Output'].value_counts(dropna=False)) #Give the output class list
    entopy_of_target = entropy_of_class(Target_list['Output'], labels) #Take out entropy with respect to output classes 

    if entopy_of_target == 0:
        return Target_list.index[0], entopy_of_target #Make a leaf node/ Complete decision tree

    column_name = list(dataframe.drop('Output', axis=1).columns) #Drop the last last column to find best attribute
    attr_gains_list = []
    
    for attribute in column_name:
        group_by_label = dataframe.groupby([attribute, 'Output']).size().to_frame(name='occurences').reset_index()
        '''The above pandas function will take the whole attribute and count occurences of each class/label with respect to the output class and give it as a dataframe '''
        attribute_ent = 0
        for val_attr in group_by_label[attribute].unique(): #Select each label
            temp = group_by_label[group_by_label[attribute] == val_attr] #We take individual class w.r.t final label
            s = temp['occurences'].sum() / dataframe.shape[0] #And divide each of them with total number of values
            ''' This above will essentially give out N(each class)/total N '''
            ent_val = entropy_of_class(temp['occurences'], labels) * s
            attribute_ent = attribute_ent + ent_val
        gain_of_attribute = entopy_of_target - attribute_ent #formula for information gain
        attr_gains_list.append(gain_of_attribute) #Take all gains
    max_gain = column_name[np.argmax(attr_gains_list)] # Take out the column name with max info. gain and return along with entropy
    return max_gain, entopy_of_target


def algo_ID3(dataframe, labels, node):
    current_node, ent = best_attr(dataframe, labels) #Will give the current node and its entropy
    for attr_val in dataframe[current_node].unique(): #Will take each individual level
        new_dataframe = dataframe[dataframe[current_node] == attr_val].drop([current_node], axis=1) #Drop that node attribute
        child_node, entopy_target = best_attr(new_dataframe, labels) #Calculate the child best attribute of that parent node
        if child_node in labels:
            ET.SubElement(node, 'node', {'entropy': str(entopy_target), 'feature': current_node, 'value': attr_val}).text = str(child_node)
            '''If the child node is leaf node i.e. the entropy is 0 and all labels are of same class, we take it as a leaf      node and stop the tree then and there and will move towards new label of that node/attribute'''
            continue
        else:
            next_node = ET.SubElement(node, 'node', {'entropy': str(entopy_target), 'feature': current_node,  'value': attr_val}) 
            ''' If there is still uncertainity and entropy we will continue to grow tree of that particular node until the above condition is achieved '''
       
        algo_ID3(new_dataframe, labels, next_node)  #We will call this function recursively 


def initialize():
    dataset = pd.read_csv(input_file, delimiter=',', header=None)
    dataset = dataset.rename(lambda x: 'att' + str(x), axis=1) #For naming as required
    dataset = dataset.rename({dataset.columns[-1]: 'Output'}, axis=1) #Renaming last as output

    labels = dataset['Output'].unique().tolist() # Take all the labels occuring in the output
    Target_list = pd.DataFrame(dataset['Output'].value_counts(dropna=False)) #counting occurences of each label
    ent_tar = entropy_of_class(Target_list['Output'], labels) #Calculating root entropy and....

    root = ET.Element('tree', {'entropy': str(ent_tar)}) #... Building a tree on it

    algo_ID3(dataset, labels, root)
    tree = ET.ElementTree(root)
    tree.write(output_file) #Write that tree in a given file name


#For passing at command line execution
if __name__ == "__main__":
     input_file = None
     output_file = None
    
    
     opts = getopt.getopt(sys.argv[1:], '', ["data=", "output="])
     opts = opts[0]
     for opt, arg in opts:
         if opt == '--data':
             input_file = arg
         elif opt == '--output':
             output_file = arg
    
     if input_file == None or output_file == None :
         print('Error in inputfilename or outputfilename')
         exit()
     initialize()
  