# Morays

_**M**imicking **O**cean **R**elevance with **A**rtificiall**Y** **I**ntelligent **S**nakes_

Morays is a project that illustrates how to deploy ML-based components in ocean models with Eophis.

It also temporarily stores NEMO developement sources before a merging in an official release.

## Overview

- [Eophis](https://github.com/alexis-barge/eophis/) is a python package built on [OASIS](https://oasis.cerfacs.fr/en/) that facilitates the deployment of inference models (i.e. models based on machine learning components) for coupled runs with Earth-System simulation codes. The last OASIS version allows communication between non-homogeneously written codes. This tool is particularly suitable for our purposes since an OASIS interface already exists in several ocean models.

- Ocean models taken into consideration in the Morays project are:
    - [NEMO](https://www.nemo-ocean.eu/)
    - [CROCO](https://www.croco-ocean.org/) (not started yet)


## Experiments collections

Collaborative collections of Morays experiments are available here:

- [morays-NEMO](https://github.com/morays-NEMO) for NEMO

- [morays-CROCO](https://github.com/morays-CROCO) for CROCO (not started yet)


## Docs
Detailed informations about software environement requirements and steps to set up a coupled ML-ocean model experiment.

...WORK IN PROGRESS...

## Nemo Sources

Modified NEMO sources:

- Copy them in the `MY_SRC` directory of a NEMO config
- NEMO must be then compiled with deployed OASIS Python API (see this [guide](https://github.com/alexis-barge/morays/blob/main/docs/pyOASIS_NEMO.md)).

#### Last modifications (under assumption of testing):
  * Architecture: OASIS coupling module `cpl_oasis.F90` was initially managed by SBC module
      - OASIS environnement is now totally managed by NEMO main routines in `nemogcm.F90`
      - Coupling module is independent and can be called by any other module to define coupling variables, send and receive them on demand
      - Now possible to perform exchange of 3D fields (OASIS_V5 needed)

  * Properties of coupling variables are stored in meta-arrays `ssnd` and `srcv` in coupling module
      - Dimension added to the array to sort meta-data between calling modules

  * New modules:        
      - `infmod.F90` : module dedicated to inference models management 
         /!\ HARD-CODED MODELS FOR NOW /!\
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
