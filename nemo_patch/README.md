# Nemo Patches

- Modified NEMO sources for:
	- [v4.2.0](https://forge.nemo-ocean.eu/nemo/nemo/-/releases/4.2.0)
	- [v4.2.1](https://forge.nemo-ocean.eu/nemo/nemo/-/releases/4.2.1)
	- [v4.2.2](https://forge.nemo-ocean.eu/nemo/nemo/-/releases/4.2.2)

- Copy them in the `MY_SRC` directory of a NEMO config
- NEMO must be then compiled with *key_oasis3* CCP key (see this [guide](https://morays-doc.readthedocs.io/en/latest/nemo.html)).


### Last modifications (under assumption of testing):
  * Architecture: OASIS coupling module `cpl_oasis.F90` was initially managed by SBC module
      - OASIS environnement is now totally managed by NEMO main routines in `nemogcm.F90`
      - Coupling module is independent and can be called by any other module to define coupling variables, send and receive them on demand
      - Now possible to perform exchange of 3D fields (OASIS_V5 needed)

  * Properties of coupling variables are stored in meta-arrays `ssnd` and `srcv` in coupling module
      - Dimension added to the array to sort meta-data between calling modules

  * New modules:        
      - `infmod.F90` : module dedicated to inference models management   /!\ Models must be hard-coded for now /!\
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
