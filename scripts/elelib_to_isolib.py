#!/usr/bin/env python
import linecache
from optparse import OptionParser

def create_elelib(inp, outp):
    output=file(outp, 'w')
    line_index=1
    line=linecache.getline(inp, line_index)

    while line !='':

        line_list=line.split()
        if len(line_list)==5:
             num_isos=int(line_list[4])
            
             for x in range(0,5):
                 output.write(line_list[x]+' ')
             output.write('\n')

             for y in range(1,num_isos+1):
                 output.write('\t\t\t')
                 for z in range(0,2):
                     output.write(linecache.getline(inp,line_index+y).split()[z]+' ')
                 output.write('\n')                 
             output.write('\n')

             for iso_index in range (1,num_isos+1):
                 iso_line=linecache.getline(inp,line_index+iso_index).split()
                 output.write(line_list[0]+':'+iso_line[0]+' '+line_list[1]+' '+\
                     line_list[2]+' '+line_list[3]+' '+ '1\n\t\t\t'+iso_line[0]+'\t100'+'\n')
                 output.write('\n')
        line_index=line_index+1
        line=linecache.getline(inp, line_index)
def main( arguments = None ):

    parser = OptionParser()

    parser.add_option('-o', dest='outp', default='isolib', help='specify\
        output file name')
    (opts, args) = parser.parse_args( arguments )
    create_elelib(args[0],opts.outp)

if __name__ == '__main__':
    main()
 
             
    
    
