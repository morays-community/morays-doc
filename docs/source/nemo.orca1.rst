Off-the-shelf NEMO climatological ORCA1
=======================================


.. toctree::
   :maxdepth: 2


This section summarizes guidelines to deploy and run a climatological 1º global circulation NEMO configuration (ORCA1) in different modelling contexts.

Initial conditions of the configuration are based on the `Gouretski, 2019 <https://www.wdc-climate.de/ui/entry?acronym=WAGHC_V1.0>`_ Argo-WOCE climatology, and the atmospheric forcing are those from `COREv2 normal year <https://data1.gfdl.noaa.gov/nomads/forms/core/COREv2/CNYF_v2.html>`_ . More details about the scientific context of the configuration can be found in this `paper <https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2024MS004824>`_.



Prerequisites:
    - Operating NEMO environment with XIOS (see `NEMO doc <https://sites.nemo-ocean.io/user-guide/install.html>`_ for help)
or:
    - Installed Apptainer
    - Morays container (see below)
    
    
**Apptainer on Linux**

.. code-block :: bash
    
    # For Ubuntu
    sudo apt update && sudo apt install -y software-properties-common
    sudo add-apt-repository -y ppa:apptainer/ppa
    sudo apt update && sudo apt install -y apptainer
    
    # For amd64 Debian
    sudo apt update
    sudo apt install -y wget
    cd /tmp
    wget https://github.com/apptainer/apptainer/releases/download/v1.4.3/apptainer_1.4.3_amd64.deb
    sudo apt install -y ./apptainer_1.4.3_amd64.deb
    
.. warning :: For other Linux distributions, please refer to this `guide <https://github.com/apptainer/apptainer/blob/main/INSTALL.md>`_.
 

**Apptainer on macOS**

.. code-block:: bash
    
    # Apptainer is available on macOS via LIMA (LInux virtual MAchines)
    brew install qemu lima  # Install with brew
    port install qemu lima  # Install with macports

    # Create Linux VM with Apptainer
    limactl start template://apptainer
    limactl shell apptainer
    # IMPORTANT: type 'cd' to go in VM home, 'pwd' should return '/home/<your_name>.linux'
    cd
    
    # NB1: copy files from VM to host
    limactl cp apptainer:/path/to/file  /host/destination
    

**Run Morays container**

.. code-block:: bash

    # Get your hardware architecture
    uname -m
    #  aarch64 --> arm64
    #  x86_64  --> amd64
    ARCH=arm64

    # Download image
    wget https://github.com/morays-community/morays-doc/releases/download/containers/morays_env_${ARCH}.sif

    # Run container: should print " >>>> Welcome in Morays environment ! <<<< "
    apptainer run --writable-tmpfs --bind $(pwd):/home/jdoe/to_host morays_env_${ARCH}.sif

        
        
1. Deploy climatological ORCA1
------------------------------

Get a copy of NEMO. For now, the material described in this documentation is compatible with NEMO version 5.0.1 only:

.. code-block:: bash

    # Clone NEMO
    mkdir -p ~/to_host
    cd ~/to_host
    git clone --branch 5.0.1 https://forge.nemo-ocean.eu/nemo/nemo.git ~/to_host/nemo_5.0.1


Clone ORCA1 repository in the NEMO config directory:
    
.. code-block:: bash

    # Clone ORCA1
    git clone https://github.com/alexis-barge/ORCA1.git ~/to_host/nemo_5.0.1/cfgs/ORCA1


Install and build (will download inputs and forcing files):
    
.. code-block:: bash

    # Build ORCA1
    cd ~/to_host/nemo_5.0.1/cfgs/ORCA1
    ./deploy.sh


.. warning :: You might need to adapt the template arch file ``arch-ORCA1_GCC.fcm`` if you are not running the Morays container.



2. Run standard ORCA1 configuration
-----------------------------------

Once installed, the configuration is ready to be executed classically:


.. code-block:: bash

    cd ~/to_host/nemo_v5.0.1/cfgs/ORCA1/EXP00/RUN_DEFAULT
    mpirun -np 4 ./nemo.exe
    
    
 
3. Python-wrapped ORCA1
-----------------------
 
The configuration is compatible with the NEMO Python wrapper `Aronnax <https://github.com/alexis-barge/Aronnax>`_ to be steered from a Python environment. Install Aronnax:

.. code-block:: bash

    # Clone Aronnax
    cd ~/to_host
    git clone --branch v0.5.x https://github.com/alexis-barge/Aronnax.git Aronnax
    
    # Compile Aronnax
    cd ~/to_host/Aronnax
    NEMO_BLD=~/to_host/nemo_v5.0.1/cfgs/ORCA1   pip install .
    
    
.. warning ::

    If you are not running the Morays container, XIOS must be compiled with ``-fPIC`` flag first (or equivalent depending on your compiler). For instance:
    
    .. code-block :: bash

        cat XIOS/arch/arch-XIOS.fcm
            %CCOMPILER      mpicc -fPIC
            %FCOMPILER      mpif90 -fPIC
 
    or if you are using XIOS3 or later:
    
    .. code-block :: bash
    
        ./make_xios --full --prod --arch <YOUR_MACHINE> --dynamic
    
    
To simulate one year, run the following commands in a Python script or interpreter:

.. code-block:: python

    import Aronnax

    solver = Aronnax.NEMO(config="ORCA1_RUN", nproc=4)
    solver.start()
    solver.advance(years=1)
    solver.finish(write_restart=True)
 
 
.. note :: More informations about use of Aronnax can be found in its dedicted documentation.

 
 
4. Hybrid ML-ORCA1
------------------

To set up a hybrid Python-ORCA1 configuration, follow the steps described in `this tutorial <https://morays-doc.readthedocs.io/en/latest/nemo.tuto.html>`_, starting from section 2.


Also you can get inspired by using this base to deploy the `ORCA1_AirSeaFluxes <https://github.com/morays-community/NEMO-ORCA1_AirSeaFluxes>`_ test case. As a reminder, the procedure to deploy a Morays experiment is described in this `section <https://morays-doc.readthedocs.io/en/latest/nemo.getting_started.html>`_.
