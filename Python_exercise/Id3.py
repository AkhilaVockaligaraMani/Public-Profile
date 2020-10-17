import csv
import xml.etree.cElementTree as xml
import math

Column_Dict = {}
row_count = 0
inputFile = None
x_axis     = []
number_col = 0
information_gain = 0.0
node_gain = []
outputFilename = ""
#outputFile= "C:\\Users\\shara\\Desktop\\DKE\\Machine learning\\Programming assignment 2\\decisiontree\\1.xml"

		

class Column:

    __count = 0
    
    
    def __init__(self, row_number = 0):
        self._row_number = []
        self.__info_gain = 0.0
        self.__count = self.__count + 1
        self._row_number.append(row_number)
		
		
    def update(self, row_number):
        self.__count = self.__count + 1
        self._row_number.append(row_number)
		
    def getCount(self):
        return self.__count
		
    def getRowNumber(self):
        return self._row_number
		


		
def prepare_dict(data, col_index, row_index):


    if col_index in Column_Dict:
        inner_dict = Column_Dict.get(col_index)
        if data in inner_dict:
            obj = inner_dict.get(data)
            obj.update(row_index)
			
        else:
            obj = Column(row_index)
            inner_dict.update( { data : obj } )
			
    else:
        obj = Column(row_index)
        Column_Dict.update( { col_index : { data : obj } } )
            
            
  
def prepare_column_data():
    
    col = 0
    while col < number_col:
        row = 0
        while row < row_count:
            temp = x_axis[row][col]
            prepare_dict(temp, col, row)
            row = row + 1
        col = col + 1
            			

def read_input_file():
    with open(inputfile, 'r') as csvfile:
        sheet = csv.reader(csvfile)
        global row_count
        for row in sheet:
            global number_col
            number_col = len(row)
            x_axis.append(row)
            row_count = row_count + 1
        

def find_information_gain(col_index):

    inner_dict = {}
    inner_dict = Column_Dict.get(col_index)
    sum = 0
    for key, value in inner_dict.items():
        count = value.getCount()
        p_var = count/float(row_count)
        sum += -  p_var * math.log( p_var , len(inner_dict))
    global information_gain
    information_gain = sum
        
        

def find(rows):

    inner_dict = Column_Dict.get(number_col-1)
    sum = 0.0
    for key, value in inner_dict.items():
        r1 = set(value.getRowNumber())
        r2 = set(rows)
        count = len(r2.intersection(r1))
        if len(rows) > 0:
            p_var = count/float(len(rows))
            if p_var > 0:
                sum += -  p_var * math.log( p_var , len(inner_dict))
    return sum
        
    
def entropy(index1, index2 ):

    final_sum = 0.0
    index1_dict = Column_Dict.get(index1)
    temp_list = [None, 0, [], 0.0, 0.0]
    for key1, value1 in index1_dict.items():  
        count = value1.getCount()
        temp_list[0] = key1
        temp_list[1] = count		
        row_number = value1.getRowNumber()
        index2_dict = Column_Dict.get(index2)
        for key2, value2 in index2_dict.items():
            s1 = set(value1.getRowNumber())
            s2 = set(value2.getRowNumber())
            temp_list[2].append(len(s1.intersection(s2)))
        sum = 0
        for i in range(len(temp_list[2])):
            if  temp_list[2][i] == 0:
                None
            else:
                p_var = temp_list[2][i]/float(count)
                sum += - p_var * math.log(p_var ,len(Column_Dict.get(number_col-1)))
        temp_list[3] = sum 
        temp_list[4] = ( ( count/float(row_count) * temp_list[3] ) )
        final_sum = final_sum + temp_list[4]
        temp_list = [None, 0, [], 0.0, 0.0]
    return information_gain - final_sum
        
                	
					
def get_prediction(main_key, main_row_number=[]):

    dict = Column_Dict.get(number_col-1)
    for key, value in dict.items():
        r1 = set(value.getRowNumber())
        inter = len(set(main_row_number).intersection(r1))
        if len(main_row_number) != 0:
       	    count = inter/float(len(main_row_number))
        else:
            count = 0

        if count == 0:
            None
        if count == 1:
            
            return 1, key
        if count > 0:
            
            return 0, "not found"
    
    return 0, "not found"
            

			
			
def find_gain_factor(parent, main_key, main_column, main_row_number=[], comp_attr = [] ):

    entropy_dict = { }
    flag, key = get_prediction(main_key, main_row_number)
    if flag == 1:
        child = xml.SubElement(parent, "node", value=main_key, feature = "att" + str(main_column) ,entropy=str(0.0))
        child.text = key
        return parent
        
    else:
        for index in comp_attr:
            in_dict = Column_Dict.get(index)
            final_sum = 0.0
            for key1, value1 in in_dict.items():		
                count = len(set(main_row_number).intersection(set(value1.getRowNumber())))
                temp_list = [key1, count, [], 0.0, 0.0]
                for key2, value2 in Column_Dict.get(number_col-1).items():
                    r1 = value1.getRowNumber()            
                    r2 = value2.getRowNumber()            
                    s1 = set(main_row_number).intersection(set(r1)).intersection(set(r2))
                    temp_list[2].append(len(s1))
                sum = 0
                for i in range(len(temp_list[2])):
                    if  temp_list[2][i] == 0:
                        None
                    else:
                        p_var = temp_list[2][i]/float(count)
                        sum += - p_var * math.log(p_var ,len(Column_Dict.get(number_col-1)))
                temp_list[3] = sum 
                temp_list[4] = ( ( count/float(len(main_row_number)) * temp_list[3] ) )
                final_sum = final_sum + temp_list[4]
                temp_list = [None, 0, [], 0.0, 0.0]
            ent = find(main_row_number) - final_sum
            entropy_dict.update( { index : ent } )

        max_index = -9999
        max_entropy = -999999
        for key_dict, value_dict in entropy_dict.items():
            if value_dict > max_entropy:
                max_index = key_dict
                max_entropy = value_dict

        if entropy_dict:			
          
            f1=find(main_row_number)
            child = xml.SubElement(parent, "node", value=main_key, feature = "att" + str(main_column) ,entropy=str(f1))   
            for key_var, value_var in Column_Dict.get(max_index).items():
                sub_list = list(set(main_row_number).intersection(set(value_var.getRowNumber())))
                sub_comp_attr = []
                for j in comp_attr:
                    if j == max_index:
                        None
                    else:
                        sub_comp_attr.append(j)
                if len(sub_list) > 0:  
                    find_gain_factor(child, key_var, max_index, sub_list, sub_comp_attr )
        return parent

            
							
if __name__ == '__main__':
    inputfile = input('Enter the input file name: ')
    outputFilename = input('Enter the output file name: ')
    outputFile=open(outputFilename,"wb")
    read_input_file()
    prepare_column_data()
    find_information_gain(number_col-1)
    i = 0
    while i < number_col -1:
	    node_gain.append( entropy(i, number_col-1) )
	    i = i + 1 

    col = node_gain.index(max(node_gain))
    comp_attr = [ ]	
    i = 0
    while i < number_col-1:
        comp_attr.append(i)
        i = i + 1
		
    comp_attr.remove(col)
    tree = xml.Element("tree", entropy = str(information_gain))
    for key, value in Column_Dict.get(col).items():
        fulltree = find_gain_factor (tree, key, col,  value.getRowNumber(), comp_attr )
    fulltree = xml.ElementTree(fulltree)
    fulltree.write(outputFile)
