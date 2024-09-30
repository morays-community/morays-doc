Getting Started
===============


.. toctree::
   :maxdepth: 2

In this tutorial, we will deploy a CROCO hybrid modeling experiment.

.. warning :: WORK IN PROGRESS

Prerequisites to the turorial:
    - ...



Introduction
------------



1. Morays environment
---------------------

Every Morays experiments with CROCO require a couple of shared libraries. We quickly summarize the steps to install them.

Compile OASIS_v5.0
~~~~~~~~~~~~~~~~~~

OASIS is the coupling library on which both NEMO and Eophis rely to perform field exchanges. OASIS_v5.0 is the minimal required version and must be dynamically compiled. See `OASIS documentation <https://oasis.cerfacs.fr/en/documentation/>`_ for more details.

.. code-block:: bash

    # Clone OASIS_v5.0
    cd ~/
    git clone https://gitlab.com/cerfacs/oasis3-mct.git
    cd ~/oasis3-mct
    git checkout OASIS-MCT_5.0
    cd util/make_dir
    
Edit your own ``make.<YOUR_MACHINE>`` file. Be sure to have the following flags defined for dynamic compilation:

.. code-block:: bash

     DYNOPT = -fPIC
     LDDYNOPT = -shared -lnetcdff -lnetcdf


.. code-block:: bash
    
    # Link your architecture file for compilation
    echo "include ~/oasis3-mct/util/make_dir/make.<YOUR_MACHINE>"  >  make.inc
     
    # Compile dynamic libraries
    make -f TopMakefileOasis3 pyoasis
    

.. note :: In the following, we will compile NEMO with OASIS. Just keep in mind where OASIS_v5.0 dynamic libraries are stored. Let's assume for the tutorial that they are at this location:

    .. code-block :: bash
    
        ls ~/oasis3-mct/BLD/lib/
        libmct.so   libmpeu.so   liboasis.cbind.so   libpsmile.MPI1.so   libscrip.so



Compile XIOS
~~~~~~~~~~~~

XIOS is used by NEMO to write results. It must be compiled with the abovementioned OASIS libraries. See `XIOS documentation <https://forge.ipsl.jussieu.fr/ioserver/wiki/documentation>`_ for more details about compilation of XIOS with OASIS.


.. code-block:: bash

    # Clone XIOS
    cd ~/
    svn co http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS2/trunk xios_oasis_5.0
    cd ~/xios_oasis_5.0/

Edit your ``arch-<YOUR_MACHINE>.path`` file to include the OASIS libraries directories and bindings:

.. code-block:: bash

    # edit arch files
    vi archs/arch-<YOUR_MACHINE>.path
        # ...
        OASIS_INCDIR="-I~/oasis3-mct/BLD/include/ -I~/oasis3-mct/BLD/build-shared/lib/cbindings/"
        OASIS_LIBDIR="-L~/oasis3-mct/BLD/lib"
        OASIS_LIB="-loasis.cbind -lpsmile.MPI1 -lscrip -lmct -lmpeu"

.. code-block:: bash

    # Compile
    ./make_xios --full --prod --arch <YOUR_MACHINE> --use_oasis oasis3_mct

    # library should be here
    ls lib/
    libxios.a


2. Experiment environment
-------------------------

Now that we have set up the common environment for all Morays experiments, we need to install the dependencies related to the specific experiment of interest. Let's find it in the `Morays repositories <https://github.com/orgs/morays-community/repositories>`_. Those are named with a simple convention: ``<OCEAN_CODE>-<EXPERIMENT>``. The corresponding repository for the tutorial is then ``CROCO-...``. We clone the tutorial branch:



3. CONFIG - CROCO case
----------------------

Morays patch
~~~~~~~~~~~~

Experiment patch
~~~~~~~~~~~~~~~~


4. RUN - CROCO settings
-----------------------


5. INFERENCES - Python material
-------------------------------


6. Running the experiment
-------------------------


7. POST-PROCESS and RES
-----------------------


Going further
-------------

From now on, you have an useable deployed Morays experiment for CROCO. Do not hesitate to check out and deploy other test cases to get inspired.

Here are the locations where you can play with:


Next chapters of this documentation provide more details on how to configure the CROCO external communication module, and to create a Morays experiment for CROCO from scratch.

