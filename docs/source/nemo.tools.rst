Advanced NEMO-Eophis coupling
=============================


.. toctree::
   :maxdepth: 2

Previous sections gave complete minimal guidelines to reproduce or create an hybrid ocean modeling experiment with NEMO. In this section, we present additional tools to make hybrid modeler's life easier.



pyDCM
-----

NEMO experiments (hybrid or not) often need to perform long simulation times, which requires to  run the simulation on several segments. Each segment starts on the results of the previous one. Hopefully, they are many of useful tools to automate the segments succession and save the intermediate results and restarts. One of this tool is named Drakkar Config Manager (DCM).

However, DCM has not been designed to manage hybrid modeling environment with Python and Eophis material. `pyDCM <https://github.com/alexis-barge/pyDCM/tree/4.2.1_pyoasis?tab=readme-ov-file>`_ is an updated version of the DCM in which those missing features have been added. We invite you to read the pyDCM documentation for more details on how to use it.

Some of the Morays use cases have been conducted with pyDCM (``NEMO-Subgrid_Density`` for instance).

Additional material required by pyDCM are stored in experiments repositories in accordance with Morays template (see **Morays collections** section).




Configure NEMO with Eophis
--------------------------

.. warning :: Work in progress




Hybrid CPU/GPU execution
------------------------

.. warning :: Work in progress
