Morays
======

**M** imicking **O** cean **R** elevance with **A** rtificiall **Y** intelligent **S** nakes

`Morays`_ is an effort for sharing Machine Learning-based closures for hybrid physics / AI ocean modeling. The project is intended to be ocean model agnostic with commonly agreed templates.

It also serves as a platform for collaborative collections of examples and use cases for reproducible hybrid ocean modeling experiments.

Codes taken into consideration in this work are:
    - `NEMO`_
    - `CROCO`_ (not started yet)


Strategy
--------

`Eophis`_ is a library that facilitates the deployment of Python scripts through `OASIS`_ for coupled runs with Fortran/C geoscientific codes. This tool is particularly suitable for our purposes since an OASIS interface already exists in several ocean models.

In this context, a Morays experiment is an ocean simulation in which the physical model sends fields towards an external Python script that contains ML components. The results infered by the ML model are sent back to the ocean and retroactively used for the solution.

.. note:: This documentation provides detailed guidelines and tools to set up an hybrid ocean experiment with Eophis. Newcomers are welcomed to start with this `tutorial`_.


.. _Morays: https://github.com/morays-community
.. _Eophis: https://github.com/meom-group/eophis/
.. _OASIS: https://oasis.cerfacs.fr/en/
.. _NEMO: https://www.nemo-ocean.eu/
.. _CROCO: https://www.croco-ocean.org/
.. _tutorial: https://morays-doc.readthedocs.io/en/latest/getting_started.html
