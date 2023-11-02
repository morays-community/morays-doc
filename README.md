# Morays

**<span style="color:red">M</span>imicking <span style="color:red">O</span>cean <span style="color:red">R</span>elevance with <span style="color:red">A</span>rtificiall<span style="color:red">Y</span> intelligent <span style="color:red">S</span>nakes**

Morays is a project aiming the development of a [NEMO](https://www.nemo-ocean.eu/)  interface with Python written Machine Learning libraries and the supply of material for a demonstrating coupled simulation.

## Overview

**Development Strategy**

- [Eophis](https://github.com/alexis-barge/eophis/) is a python package built on [OASIS](https://oasis.cerfacs.fr/en/) that facilitates the deployment of inference models (i.e. models based on machine learning components) for coupled runs with Earth-System simulation codes. The last OASIS version allows communication between non-homogeneously written codes. This tool is particularly suitable for our purposes since an OASIS interface already exists in NEMO.

- The first main objective is to update the NEMO OASIS interface in order to be compatible with the creation of a module dedicated to Eophis communication.

- The second main objective is to create a NEMO API to easily implement ocean models that need inferences.

**Demonstration Case**
<span style="color:red">Work In Progress</span>
- Morays test case simulates a complete year of an eORCA025 config with daily exchanges of Sea Temperature with Eophis in which equation of state is computed with different models. Results are sent back to NEMO and written in an ouput file with XIOS.

## Docs
Detailed informations about use of Morays and implementation of models with NEMO inference module interface.
<span style="color:red">Work In Progress</span>

## Nemo Setup

### src

Modified NEMO sources. 

- Copy them in the `MY_SRC` directory of a NEMO_v4.2.1 config to run demonstration case. <u>NEMO must be then compiled with deployed OASIS Python API</u> (see this [guide](https://github.com/alexis-barge/morays/blob/main/python_cpl/pyOASIS_NEMO.md)).

#### Last modifications (under assumption of testing) : 28/10/2023
  * Architecture: OASIS coupling module `cpl_oasis.F90` was initially managed by SBC module
      - OASIS environnement is now totally managed by NEMO main routines in `nemogcm.F90`
      - Coupling module is independent and can be called by any other module to define coupling variables, send and receive them on demand
      - Now possible to perform exchange of 3D fields (OASIS_V5 needed)

  * Properties of coupling variables are stored in meta-arrays `ssnd` and `srcv` in coupling module
      - Dimension added to the array to sort meta-data between calling modules

  * New modules:        
      - `infmod.F90` : module dedicated to inference models management 
         /!\ HARD-CODED DUMMY MODELS FOR NOW /!\
      - `inffld.F90` : memory management for inference models needed fields

<table>
<tr>
<th> Before </th>
<th> After </th>
</tr>
<tr>
<td>
<img width="510" alt="archi_nemo_old" src="https://github.com/alexis-barge/smart-morey/assets/138531178/d68820ef-10b2-459c-afaf-603f2dc4add8">
</td>
<td>
<img width="466" alt="archi_nemo_new" src="https://github.com/alexis-barge/smart-morey/assets/138531178/8e2ac17a-2168-4aa0-9bc9-e666cd66dc5c">
</td>
</tr>
</table>

### config
NEMO simulation material (namelists, XIOS files, launch scripts...) for demonstration case. Copy them in the `EXP00` directory of the NEMO config.
<span style="color:red">Work In Progress</span>

## Inferences

Inference models and coupling material for demonstration case. Copy them in the `EXP00` directory of the NEMO config.

- **main.py** : Eophis script that deploys models hereunder during coupling

- **models.py** : inference models collection <span style="color:red">
Work In Progress</span>
     - `add_100` --- trivially add 100 to a field
     - `Std_Stanley` --- analytic computation of [*Stanley et al.*(2020)](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2020MS002185) model
     - `GTF_LinReg_Stanley` --- *Stanley et al.* with LinReg [GeoTrainFlow](https://github.com/anastasiaGor/geoTrainFlow) inferences
     - `GTF_FCNN_Stanley` --- *Stanley et al.* with FCNN GeoTrainFlow inferences
     -`GTF_CNN_Stanley` --- *Stanley et al.* with CNN GeoTrainFlow model



