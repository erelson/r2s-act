#! /usr/bin/env python

# This script is passed a path for a source.F90 file (e.g. from the parent
#  directory).  It searches the file for 'subroutine voxel_sample' at the start
#  of a line, and then comments that line and subsequent lines until a line is
#  found that begins with 'end subroutine source'. This hack is used for crude
# For benchmarking purposes... we pull the position sampling code out
#  of source.F90 and store it in a new file. This is done for both voxel and
#  uniform sampling.

import sys
import os

def main():
    if not os.path.isfile(sys.argv[1]):
        print "File", sys.argv[1], "not found."
        sys.exit(1)
    if sys.argv[1] == "source.F90":
        print "File", sys.argv[1], "has same name as script output; " \
                "script will not overwrite the original."

    fr = open(sys.argv[1], 'r')
    fw = open("sampling.F90", 'w')

    #fw.write("module mod_alias_table\n\n")

    keep = 0
    for line in fr:
        # Fun fact: range specifications exceeding list sizes are not a problem
        #if keep == 0 and line.split('(')[0:1] == ["subroutine voxel_sample "]:
        if keep == 0 and line.strip() == "subroutine voxel_sample":
            keep = 1
        if keep == 1:
            fw.write(line)
            #if line.split()[0:3] == ["end","subroutine","voxel_sample"]:
            if line.split()[0:3] == ["end","subroutine","uniform_sample"]:
                keep = 2
            # Record how long the heap sort took."
            elif line.strip() == "call heap_sort(bins, len)":
                fw.write("        call CPU_TIME(th)\n")

        elif keep == 2:
            break
        else:
            pass

    fr.close()

    fw.close()

    if keep != 2:
        print "ERROR: subroutine subroutine voxel_sample was not found. keep={0}".format(keep)
    else:
        print "Successfully found voxel_sample and related subroutines'\n"

    return


if __name__ == '__main__':
    if len(sys.argv) == 2:
        main()
    else:
        print "Fail.", sys.argv
        sys.exit(1)
