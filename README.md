# smart-morey

**Populate ocean with artificially intelligent snakes** 

This repository contains sources for the development of interfaces between NEMO and python-written Machine Learning features via OASIS. 

The long term objectives are:
  * create an API into NEMO to easily implement models that needs data/inferences exchange with ML components
  * build a python code that manages the data treatment in accordance with inferences asked by NEMO models or users

**MAIN ROADMAP**
  * I: ~~perform dummy exchanges between NEMO and python code with OASIS~~
  * II: send data from NEMO to python, successfully call a ML component, send back the inference and write it in an ouptut file
  * III: ...

## Nemo Sources

Sanding box to modify NEMO sources. Copy them in the `MY_SRC` directory of a NEMO_v4.2.1 config to use them.

**Main modifications (under assumption of testing) : 18/09/2023**
  * OASIS coupling module `cpl_oasis.F90` was managed by SBC module since it is only used for exchanges with atmosphere or SAS module.
      - OASIS environnement is now totally managed by NEMO main routines in `nemogcm.F90`
      - Coupling module can now be called by any other module to define variables to exchange, send and receive them on demand

      [ ADD A DRAWING ]

  * Properties of variables and exchanges were stored in meta-arrays `ssnd` and `srcv` in coupling module
      - Dimension added to the array to sort meta-data between calling modules
        
  * `infmod.F90` : module dedicated to Inference Models, can call coupling module -- /!\ HARD-CODED DUMMY MODELS FOR NOW /!\
  * `inffld.F90` : memory management for inference model fields

## Inferences Models

Python sources for the Machine Learning features API.

**Does for now**
  * initialize OASIS coupling environnement
  * receive and send data
