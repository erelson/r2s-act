#!/usr/bin/env python
import linecache
from optparse import OptionParser
import sys
from itaps import iMesh, iBase
import math

def find_meshtal_type(meshtal):
    neutron_index=-1
    photon_index =-1
    count=1
    meshtal_type=''
    while neutron_index == -1 and photon_index == -1 :
        line=linecache.getline(meshtal, count)
        neutron_index=line.find('neutron')
        photon_index=line.find('photon')
        count=count+1
        if count > 100 :
            print >>sys.stderr, 'Type of meshtal not detected in \
                                                  first 100 lines'
            sys.exit(1)
    if neutron_index != -1 :
        print 'Parsing neutron meshtal file.'
        meshtal_type = 'n'
    if photon_index != -1 :
        print 'Parsing photon meshtal file.'
        meshtal_type = 'p'
    return meshtal_type

def find_first_line(meshtal):
    table_heading ='   Energy         '
    n=1
    line=linecache.getline(meshtal, 1)
    while line[0:18] != table_heading :
        n=n+1
        line=linecache.getline(meshtal, n)
    m=n+1
    print '\tSkipping Header:', n, 'lines'  
    return m #first line of data

def meshtal_to_array(meshtal, m):
    n=m
    line=linecache.getline(meshtal, n)
    array=[]
    totals=[]
    totals_bool=False
    while line != '':
        column_count=0
        char_count=0
        column_data=[0]*6
        while column_count <= 5:    
            if line[char_count] == ' ':
                char_count = char_count +1
            else :
                min_char = char_count
                while line[char_count] != ' ' and char_count<len(line)-1:
                    char_count=char_count +1
                max_char=char_count
                if line[min_char:max_char] != 'Total':
                    column_data[column_count]=float(line[min_char:max_char])
                if line[min_char:max_char] == 'Total':
                    column_data[column_count]=line[min_char:max_char]
                column_count = column_count+1
        array.append(column_data)
        if line[:11] == '   Total   ':
            totals_bool= True
        n=n+1
        line=linecache.getline(meshtal, n)
    if totals_bool == True :
        print '\tTotals energy group found'
    if totals_bool == False :
        print '\tTotals energy group not found'
    return array, totals_bool

def count_mesh_points(array):
    j=1
    while abs(array[j][0]/array[0][0] - 1) < 0.00001:
        j=j+1
    print '\tMesh points found:', j
    return j

def calc_totals(array, j):
    print '\tCalculating total fluxes'
    k=len(array)/j #number of energy groups
    totals_array=[]
    for mesh_point_index in range(0,j): #j
        totals=[0]*6
        totals[0]='Total'
        for y in (1, 2, 3):
            totals[y]=array[mesh_point_index][y]
        flux_sum=0.0
        abs_err_sqr_sum=0.0
        for a in range(0, k):
            flux_sum=flux_sum + array[mesh_point_index+j*a][4]
            abs_err_sqr_sum=abs_err_sqr_sum+(array[mesh_point_index+j*a][4]\
                                          *array[mesh_point_index+j*a][5])**2
        totals[4]=flux_sum
        if flux_sum !=0:
            #calculating relative error=absolute/value
            totals[5]=math.sqrt(abs_err_sqr_sum)/flux_sum 
        else:
            totals[5]=0.0
        totals_array.append(totals)
    print totals_array[0]
    print totals_array[1]
    print totals_array[2]
    print totals_array[3]
    for x in range(0, len(totals_array)):
        array.append(totals_array[x])
    return array

def count_energy_bins(array,j):
    k=1
    for n in range(0,len(array)-j-1):
        if abs(array[n][0]/array[n+1][0] -1)> 0.00001 :
            k=k+1
    print '\tEnergy bins found:', k, '+ Total'
    k=k+1 #need add 1 to account because the function does not 
          #detect (and count) the total group
    return k #total energy bins, include the total energy bin

def count_data_points(array):
    l=len(array)
    print '\tTotal data points found (including totals energy bin):', l
    return l

def count_delinations(array) :
    #Finding number or Z deliniations
    z=1
    while abs(array[0][3]-array[z][3])>0.001 and array[z] != 'Total':
        z=z+1
    #Finding number of Y delinations
    y=1
    while abs(array[0][2]-array[z*y][2])>0.001 and array[z] != 'Total':
        y=y+1
    #Finding number of X deliniations
    x=1
    while abs(array[0][1]-array[z*y*x][1])>0.001 and array[z*y*x] != 'Total':
        x=x+1
    print '\tSpacial Deliniations: ('+str(x)+', '+str(y)+', '+str(z)+')'
    return(x, y, z)

def check_meshtal_data(l, j, k, x, y, z) :
    if l != (j*k) :
        print >>sys.stderr, 'Number of data points does not equal \
                                           meshpoints*energy groups'
        sys.exit(1)
    if x*y*z != j :
        print >>sys.stderr, 'Number of mesh points does not equal x*y*z'
        sys.exit(1)

def print_fluxin(array, j, k, norm, output_file, backwardbool):
    k_no_totals = k-1 #Do not want to print data 'Totals' energy bin
    output=file(output_file, 'w')
    for t in range(0, j):
        pointoutput=''
        if backwardbool==False :
            a=0
            b=k_no_totals
            c=1
        else :
            a=k_no_totals-1
            b=-1
            c=-1
        for s in range(a, b, c):
           pointoutput+=str((array[s*j + t][4])*norm)+' '
           
           if (s+1)%8 == 0 & s != (k_no_totals-1):
               pointoutput+='\n'
        output.write(pointoutput + '\n\n')
    print 'ALARA flux input file,', output_file , ', created sucessfully'

def tag_fluxes_preexisting(array, j, k, norm, meshtal_type, mesh_input, mesh_output):
    if meshtal_type == 'n':
        print 'Tagging user supplied mesh with neutron fluxes'
    if meshtal_type == 'p':
        print 'Tagging user supplied mesh with photon fluxes'
    mesh = iMesh.Mesh()
    mesh.load(mesh_input)
    voxels = mesh.getEntities(iBase.Type.region)
    column_flux=[]
    column_error=[]
    count=0
    for group_ID in range(1,k+1):#need to add one for total fluxes
        if group_ID != k:
            tag_flux=mesh.createTag\
            ('{0}_group_{1:03d}'.format(meshtal_type, group_ID),1,float)
            tag_error=mesh.createTag\
            ('{0}_group_{1:03d}_error'.format(meshtal_type, group_ID),1,float)
        if group_ID ==k:
            tag_flux=mesh.createTag(meshtal_type+'_group_total',1,float)
            tag_error=mesh.createTag(meshtal_type+'_group_total_error',1,float)
        for x in range(0,j):
            array_index=x+j*count            
            column_flux.append(array[array_index][4]*norm)
            column_error.append(array[array_index][5])
        count=count+1
        tag_flux[voxels]=column_flux
        tag_error[voxels]=column_error
        column_flux=[]
        column_error=[]
    mesh.save(mesh_output)
    print 'User supplied mesh successfully tagged'

def get_mesh_boundaries(meshtal):
    boundary_index=-1
    count=1
    while boundary_index == -1 :
        line=linecache.getline(meshtal, count)
        boundary_index=line.find('Tally bin')
        count=count+1
        if count > 100 :
            print >>sys.stderr,\
            'Tally bin boundaries not found in first 100 lines'
            sys.exit(1)

    boundaries_array=[]
    for x in range (0,3):
        column_data=[]
        line=linecache.getline(meshtal, count+x)
        char_count=0
        column_count=0
        while line[char_count] != '' and char_count < len(line)-1:
            if line[char_count] == ' ':
                char_count = char_count +1
            else :
                min_char = char_count
                while line[char_count] != ' ' and char_count<len(line)-1:
                    char_count=char_count +1
                max_char=char_count
                if column_count > 1:
                    column_data.append(float(line[min_char:max_char]))
                column_count += 1    
        boundaries_array.append(column_data)        
    return boundaries_array

def create_mesh(a, mesh_name):#a is the name of the boundaries_array
    mesh=iMesh.Mesh()

    #creating a list of vertices from array of mesh boundaries
    #with z values changing fastest, then x,then y

    vertex_list=[]
    x_div=len(a[0])
    y_div=len(a[1])
    z_div=len(a[2])
    verts=[0]*len(a[0])*len(a[1])*len(a[2])
    count=0;

    for x in range(0, x_div):
        for y in range(0, y_div):
            for z in range(0, z_div):
                verts[count]= mesh.createVtx([ a[0][x], a[1][y], a[2][z] ])
                count += 1

    #grouping together vertices in cannonical order to create cubes
 
    for x in range(0, x_div-1):
        for y in range(0,y_div-1):
            for z in range(0, z_div-1):
                vert_set = [verts[z + y*z_div + x*z_div*y_div],\
                            verts[z + y*z_div + (x+1)*z_div*y_div],\
                            verts[z + y*z_div + (x+1)*z_div*y_div + z_div],\
                            verts[z + y*z_div + x*z_div*y_div + z_div],\
                            verts[z + y*z_div + x*z_div*y_div +1 ],\
                            verts[z + y*z_div + (x+1)*z_div*y_div + 1 ],\
                            verts[z + y*z_div + (x+1)*z_div*y_div + z_div +1],\
                            verts[z + y*z_div + x*z_div*y_div + z_div + 1]]
                cube, status = mesh.createEnt(iMesh.Topology.hexahedron, vert_set)
    mesh.save(mesh_name)                            

def main( arguments = None ):

    parser = OptionParser(usage='FluxParse.py <meshtal_file> <normalization_factor> [options]')

    parser.add_option('-b', action='store_true', dest='backward_bool',\
        default=False, help='Print to ALARA fluxin in fluxes in \
        decreasing energy')\

    parser.add_option('-o', dest='fluxin_name', default='ALARAflux.in',\
        help='Name of ALARA fluxin output file, default=%default')

    parser.add_option('-m',  dest='mesh_input', default= 'False',\
        help = 'Tag meshes onto user preexisting mesh, supply file name')

    parser.add_option('-p', dest='mesh_output', default='fluxparse_out.h5m',\
        help = 'Name of mesh output file, default=%default')

    parser.add_option('-s', action='store_true', dest='supress_mesh',\
        default='False', help='Supress creation of mesh file, only fluxin\
        file is created')

    (opts, args) = parser.parse_args( arguments )

    meshtal_type=find_meshtal_type(args[0])
    m=find_first_line(args[0])
    array, totals_bool=meshtal_to_array(args[0],m)    
    j=count_mesh_points(array)

    if totals_bool== False :
        array=calc_totals(array, j)
    
    k=count_energy_bins(array,j)
    l=count_data_points(array)
    x,y,z=count_delinations(array)
    check_meshtal_data(l, j, k, x, y, z)
    norm=float(args[1])
    print_fluxin(array, j, k, norm, opts.fluxin_name, opts.backward_bool)

    #mesh tagging/creation
   
    if opts.mesh_input == 'False' and opts.supress_mesh == 'False' :
        boundaries_array = get_mesh_boundaries(args[0])
        create_mesh(boundaries_array, opts.mesh_output)
        tag_fluxes_preexisting(array, j, k, norm, meshtal_type, opts.mesh_output, opts.mesh_output)
    if opts.mesh_input != 'False':
        tag_fluxes_preexisting(array, j, k, norm, meshtal_type, opts.mesh_input, opts.mesh_output)

if __name__ == '__main__':
    main()
