import sys
import io
import json

with open('ring.bs','r') as f:
    #read in file and filter out empty lines
    ring_bs = filter(lambda x: x!='', f.read().split('
'))

atoms_struct = {'species':[],'coords':[]}
#species_struct = {'name':[],'r':[],'colour':[]}
#store these two structures as a list of dicts in .json
species_list = []
bonds_list =[]

for line in ring_bs:
    split_line = line.split()
    if split_line[0] =='atom':
        atoms_struct['species'].append(split_line[1])
        x,y,z = map(float, [split_line[2],split_line[3], split_line[4]])
        atoms_struct['coords'].append([x,y,z]) 
    elif split_line[0] == 'spec':
        species_struct={}
        species_struct['name'] = split_line[1]
        species_struct['r'] = float(split_line[2])
        species_struct['colour'] = split_line[3]
        print species_struct
        species_list.append(species_struct)
    elif split_line[0] == 'bonds':
        bonds_struct = {}
        #bonds_struct = {'name1':[],'name2':[],'min_length':[],
        #                'max_length':[],'radius':[],'colour':[]}
        bonds_struct['name1'] = split_line[1]
        bonds_struct['name2'] = split_line[2]
        bonds_struct['min_length'] = float(split_line[3])
        bonds_struct['max_length'] = float(split_line[4])
        bonds_struct['radius'] = float(split_line[5])
        bonds_struct['colour'] = split_line[6]
        print bonds_struct
        bonds_list.append(bonds_struct)
    else:
        pass

#Takes a .mv file and builds a json object
#(initially a python dict) that, for each frame
#has a key for a dict of meta values and a key
#for the coords stored as a list of lists of three floats.

with open('ring.mv') as f:
    ring_mv = f.read()

frames = filter(lambda x: x!='', ring_mv.split('
'))
frame_json = {'meta':[],'coords':[]}
for i in range(0,len(frames),2):
    tmp_dict = {}
    meta_data_list = []
    for x in frames[i][6:].split("="):
        for y in x.split():
            meta_data_list.append(y)
 #   print meta_data_list
    for j in range(0, len(meta_data_list),2):
        k = meta_data_list[j]
        v = meta_data_list[j+1]
        tmp_dict[k]=v 
#    print tmp_dict    
    frame_json['meta'].append(tmp_dict)
    tmp_frame = frames[i+1].split()
    frame_json['coords'].append([map(float, [x for x in tmp_frame[j:j+3]]) 
                                 for j in range(0,len(tmp_frame),3)])
#


xbs_struct = {'atoms':atoms_struct,'species':species_list,'bonds':bonds_list,
              'frames':frame_json}

with open('ringmv.json','w') as f:
    json.dump(xbs_struct,f)
