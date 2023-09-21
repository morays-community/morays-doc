# Smart Moray

#### ***Populate ocean with artificially intelligent snakes*** 

This repository contains sources for the development of interfaces between NEMO and Python written Machine Learning libraries via OASIS. 

**Long term objectives:**
  - Create an API into NEMO to easily implement models that needs data/inferences exchange with ML components
  - Build a python code that manages the data treatment in accordance with inferences asked by NEMO models or users

**Coupling Strategy**

OASIS is a Fortran coupling library that performs field exchanges between two coupled executables. Last release provided C and Python APIs, which enables coupling between non-homogeneously written codes. 
Communication of data between NEMO and Python is done with OASIS. Here is a [guide](https://github.com/alexis-barge/smart-morey/blob/main/python_cpl/pyOASIS_NEMO.md) to install and use the OASIS Python API with NEMO.

## Nemo Sources: *nemo_src*

Sanding box for NEMO sources. Copy them in the `MY_SRC` directory of a NEMO_v4.2.1 config.

**Main modifications (under assumption of testing) : 20/09/2023**
  * Architecture: OASIS coupling module `cpl_oasis.F90` was initially managed by SBC module
      - OASIS environnement is now totally managed by NEMO main routines in `nemogcm.F90`
      - Coupling module is independent and can be called by any other module to define coupling variables, send and receive them on demand
      - Now possible to perform exchange of 3D fields (OASIS_v5 needed)

  * Properties of coupling variables are stored in meta-arrays `ssnd` and `srcv` in coupling module
      - Dimension added to the array to sort meta-data between calling modules

  * New modules:        
      - `infmod.F90` : module dedicated to Inference Models management 
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

## Python Coupling: *python_cpl*

Python sources for Machine Learning treatment and coupling materials.

**Sources do:**
  * initialize OASIS coupling environnement
  * receive 2D/3D data from ocean, send back dummy fields
