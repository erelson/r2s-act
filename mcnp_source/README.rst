=====================================
Custom MCNP source routine Readme
=====================================

This folder contains modified version(s) of custom source.F90 files and a link to the custom-compiled dag-mcnp executables that use these custom source.F90 files (link is only applicable for user in cnergg group on Wisconsin's CAE).

As of Oct. 30, 2012, the most up to date/capable *source.F90* replacement is ``source_gamma_refactor.F90``.


files
#############

source.F90 files
----------------

The different source.F90 versions:

:``source_gamma_refactor.F90``: Derived from ``vendor/source_gamma_meshtal2.F90``; Implements alias table sampling of voxels, as well as alias table sampling of photon energies. Also implements biasing. Uses *gammas* file that can include bias values and custom energy bins.

From KIT (Germany): (originals are in mcnp_source/vendor folder)

:``vendor/source_gamma_meshtal1.F90``: Not used (uses RDUM and IDUm cards in MCNP; have not tried to get this working)
:``vendor/source_gamma_meshtal2.F90``: Original was modified from 24 energy groups to 42 groups and to use dynamic array allocation for the phtn source information. The file it looks for is called *gammas*


Custom executables
------------------

You must be in the svn-dagmc group to access the modified executables. If on CAE, ideally use the links in the r2s-act/mcnp_source folder. The actual executables are managed by Eric Relson (5-21-2012).

Custom-compiled dag-mcnp/mcnp5 executable links:

:``mcnp5p_voxel``: compiled with ``source_gamma_voxel_alias.F90``

Compiling
----------

On a CAE system, one should be able to use the mcnp5p links for running problems.

Alternately, to use one of these in a custom compile of MCNP/DAG-MCNP, one can link the files in the repository to the ``DAG-MCNP/5.1.51/trunk/Source/src`` folder like this:

``ln -s *path to this folder*/source_gamma_meshtal2.F90 source.F90``

(or you could copy the file to that folder, but it won't get updated when changes happen in the repository)

To *compile* MCNP/DAG-MCNP:

We call the build scripts from within the /5.1.51/trunk/Source folder.
Since we are using some custom code, the .o files are in the way... use 'clean' (But this also gives an error so we build twice...); do the following two commands:

1:

```
>> ../scripts/build_dagmc clean
<< error messages...
```

2:

``
>> ../scripts/build_dagmc
<< Success!
``

You can now call the mcnp5 executable that was created in ``Source/src``.

If you make further modifications to the same source.F90, you can usually recompile directly with the second command above (clean is not needed).

You can make it easy to call my using an alias command in your .bashrc file, e.g.
alias mcnp5p='$HOME/DAG-MCNP/5.1.51/trunk/Source/src/mcnp5'
----

