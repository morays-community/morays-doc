NEMO4 for Morays
================

.. toctree::
   :maxdepth: 2

Let's recall that a Morays experiment is an ocean simulation that exchanges fields with an external Python script through OASIS. The strategy is to deploy the OASIS interface in the Python side and configure the global coupling environment with help of the Eophis library.

From this point, we consider that all the Python material is already well configured. See `Eophis documentation <https://eophis.readthedocs.io/en/latest/>`_ or this `tutorial <https://eophis.readthedocs.io/en/latest/tutorial.html>`_ for more details. What remains now is to configure the NEMO Python communication module in accordance with Eophis.


.. note ::
    - **Developer guide** describes how to build a NEMO4 communication module from scratch.

    - **User guide** describes a provided ready-to-use communication module -- **recommended**.



Developer guide
---------------

We describe here how to use the OASIS interface within NEMO4 to build a whole new module dedicated to external communication, here Python.


Modified OASIS interface
~~~~~~~~~~~~~~~~~~~~~~~~

Current NEMO4 releases do include an OASIS module. However, it is restricted for coupling NEMO surface boundary condition only. Modifications of the NEMO4 sources are required to make it usable for other coupling purposes.


Morays provides `patches <https://github.com/morays-community/Patches-NEMO/>`_ with the abovementionned modifications of the NEMO code. More details about modifications are available `here <https://github.com/morays-community/Patches-NEMO/blob/main/README.md>`_. As a user, you just need to copy them in the ``MY_SRC`` directory of a NEMO4 config.



Register a new module
~~~~~~~~~~~~~~~~~~~~~

Flexibility of OASIS in NEMO4 relies on the fact that coupling properties of the fields to exchange are stored in meta-arrays ``ssnd`` and ``srcv`` defined in ``cpl_oasis3.F90``. A dimension has simply been added to those arrays to sort meta-data between all the other modules that are using the OASIS interface.

The first thing to do is to register your module ID in the ``cpl_oasis3.F90`` global parameters:

.. code-block :: Fortran

    vi cpl_oasis3.F90
    ##   [...]
    91   INTEGER, PUBLIC, PARAMETER ::   ntypmax=2    ! Maximum number of coupling types
    92   INTEGER, PUBLIC, PARAMETER ::   ntypsbc=1    ! Coupling type: Surface Conditions
    93   INTEGER, PUBLIC, PARAMETER ::   ntypinf=2    ! Coupling type: Inferences Models

In this example, two modules are registered:
    1. SBC module used by NEMO to couple with atmosphere
    2. INF (inference) module used in Morays framework to couple with Python script


Each time a module calls the OASIS routines within NEMO, the ID of the calling module must be passed as argument. Set an ID for your new module and create it. Then, import the OASIS module:

.. code-block :: fortran

    USE cpl_oasis3



Define coupled fields
~~~~~~~~~~~~~~~~~~~~~

As in many codes, your module must contain an initialization routine in which the coupling properties of the exchanged fields are defined. All initialization routines that use the OASIS interface must be called during the NEMO general initialization phase in ``nemogcm.F90``, after ``cpl_domain()`` and before ``cpl_define()``. For example:


.. code-block:: fortran

    vi nemogcm.F90
    ###  [...]
    392  IF ( lk_oasis )  CALL  cpl_domain                   ! Define coupling domain for oasis
    ###  [...]
    442                   CALL  sbc_init( Nbb, Nnn, Naa )    ! SBC : surface boundary conditions
    443                   CALL  bdy_init                     ! Open boundaries
    444                   CALL  inferences_init              ! INF : Inferences from ML
    445  IF ( lk_oasis )  CALL  cpl_define                   ! setup coupling environment


Main purpose of the initialization routine is to fill the ``ssnd`` and ``srcv`` arrays.

Both have two dimensions:
    - first one refers to current module ID
    - second one refers to IDs of coupled fields you wish the current module to manage

IDs of the coupled fields are locally defined by the considered module, proceed as you prefer. Default implementation allows a maximum number of 62 coupled fields per module. This value may be changed with the parameter ``nmaxfld`` in ``cpl_oasis3.F90``.


Each coupled field has a list of attributes to fill in accordance with the exchange you wish to realize (i.e. the exchanges defined in the Eophis script):

.. code-block:: fortran

    TYPE, PUBLIC ::   FLD_CPL               !: Type for coupled field informations
       LOGICAL               ::   laction   ! To be coupled or not
       CHARACTER(len = 8)    ::   clname    ! Field alias used by OASIS
       CHARACTER(len = 1)    ::   clgrid    ! Grid type
       REAL(wp)              ::   nsgn      ! Control of the sign change
       INTEGER, DIMENSION(nmaxcat,nmaxcpl) ::   nid   ! Id of the field (no more than 9 categories and 9 extrena models)
       INTEGER               ::   nct       ! Number of categories in field
       INTEGER               ::   ncplmodel ! Maximum number of models to/from which this variable may be sent/received
       INTEGER               ::   nlvl      ! Number of grid level to exchange, set 1 for 2D fields
    END TYPE FLD_CPL


Longitude and latitude sizes of the field do not need to be specified. It is automatically done during the initialization of the OASIS environment. Exchange frequencies do not need to be specified either since they are managed by Eophis.

For instance, here are the properties for two variables ``u`` and ``u_f`` whose IDs are ``jps_ssu`` and ``jpr_uf``. They are defined in the communication module whose ID is ``ntypinf``.

.. code-block:: fortran

    ! default definitions of ssnd snd srcv
    srcv(ntypinf,:)%laction = .FALSE.  ;  srcv(ntypinf,:)%clgrid = 'T'  ;  srcv(ntypinf,:)%nsgn = 1.
    srcv(ntypinf,:)%nct = 1  ;  srcv(ntypinf,:)%nlvl = 1
    !
    ssnd(ntypinf,:)%laction = .FALSE.  ;  ssnd(ntypinf,:)%clgrid = 'T'  ;  ssnd(ntypinf,:)%nsgn = 1.
    ssnd(ntypinf,:)%nct = 1  ;  ssnd(ntypinf,:)%nlvl = 1

    ! Properties for sending 2D surface U-velocity : "u" in Eophis
    ssnd( ntypinf , jps_ssu )%clname = 'E_OUT_0'
    ssnd( ntypinf , jps_ssu )%laction = .TRUE.
    ssnd( ntypinf , jps_ssu )%clgrid = 'U'
      
    ! Properties for receiving 2D forcing term on U-grid : "u_f" in Eophis
    srcv( ntypinf , jpr_uf )%clname = 'E_IN_0'
    srcv( ntypinf , jpr_uf )%laction = .TRUE.
    srcv( ntypinf , jpr_uf )%clgrid = 'U'


The attribute ``clname`` corresponds to the alias that OASIS uses to perform the communications. ``u_f`` is manipulated by OASIS under ``E_IN_0`` from the NEMO side and under another name from the Python side. Those aliases are used in the OASIS namelist. ``clname`` must absolutely be in accordance with the content of the OASIS namelist.

.. note:: Eophis provides useful `tools <https://eophis.readthedocs.io/en/latest/usage.html#oasis-namcouple>`_ to know and manipulate OASIS aliases.


Once ``ssnd`` and ``srcv`` have been correctly filled. Finalize fields registration with:

.. code-block:: fortran

    CALL cpl_var( krcv, ksnd, kcplmodel, ktyp)
    ! INTEGER :: krcv      ! number of fields to receive for current module
    ! INTEGER :: ksnd      ! number of fields to send for current module
    ! INTEGER :: kcplmodel ! Maximum number of models to/from which NEMO is potentialy sending/receiving data
    ! INTEGER :: ktyp      ! calling module ID


.. code-block:: fortran

    ! For above u and u_f examples
    CALL cpl_var( 1, 1, 1, ntypinf )



Send a field
~~~~~~~~~~~~

During a time iteration, a coupled field can be sent with this function:

.. code-block:: fortran

    CALL cpl_snd( kid, kstep, ktyp, pdata, kinfo)
    ! INTEGER                :: kid    ! field index in ssnd for calling module
    ! INTEGER                :: kstep  ! ocean time-step in second
    ! INTEGER                :: ktyp   ! calling module ID
    ! REAL, DIMENSION(:,:,:) :: pdata  ! field to send (shape is (:,:,1) for 2D)
    ! INTEGER                :: kinfo  ! OAIS3 info argument

The ``pdata`` shape must be equivalent to the NEMO longitude and latitude grid sizes **with halos** for the two first dimensions. The third dimension is compulsory even for 2D fields and must be at least equal to the ``nlvl`` value defined during initialization.

.. code-block:: fortran

    ! Example to send surface of "u"
    USE oce ! to get u velocity field uu
    ## [...]
    INTEGER :: isec, info
    
    isec = ( kt - nit000 ) * NINT( rn_Dt )
    info = OASIS_idle
    CALL cpl_snd( jps_ssu, isec, ntypinf, uu(:,:,1:1,Kbb), info )




Receive a field
~~~~~~~~~~~~~~~

During a time iteration, a coupled field can be received with this function:

.. code-block:: fortran

    CALL cpl_rcv( kid, kstep, ktyp, pdata, kinfo, pmask)
    ! INTEGER                :: kid    ! field index in srcv for calling module
    ! INTEGER                :: kstep  ! ocean time-step in second
    ! INTEGER                :: ktyp   ! calling module ID
    ! INTEGER                :: kinfo  ! returned OASIS3 info
    ! REAL, DIMENSION(:,:,:) :: pdata  ! returned received field (shape is (:,:,1) for 2D)
    ! REAL, DIMENSION(:,:,:) :: pmask  ! coupling mask, optional

As for sending, ``pdata`` shape must be equivalent to NEMO longitude and latitude grid sizes **with halos** for the two first dimensions and ``nlvl`` for the third dimension.

.. code-block:: fortran

    ! Example to receive 2D forcing field "u_f"
    INTEGER :: isec, info
    REAL(wp), DIMENSION(jpi,jpj,1) :: uforce
    
    isec = ( kt - nit000 ) * NINT( rn_Dt )
    info = OASIS_idle
    CALL cpl_rcv( jpr_uf, isec, ntypinf, uforce, info )
    
    ! output u_f with XIOS xml files
    CALL iom_put( 'ext_uf', uforce(:,:,1) )






User guide
----------

To facilitate deployment or modification of a Morays experiment, `Morays patches <https://github.com/morays-community/Patches-NEMO/>`_ directly provide pre-defined communication modules named ``pycpl.f90`` and ``pyfld.f90``. This section describes how to use them.


.. warning:: The pre-defined communication modules require Eophis version 1.1.0 or later to be used.


The presented examples rely on the DINO.GZ21 test case. Purposes and behaviors of DINO.GZ21 are described in the NEMO **Getting started** section of this documentation. Let's resume what both entities are exchanging.

NEMO intends to send:
    - two time-evolving velocity fields ``u`` and ``v`` at a ``2700s`` frequency, on grid ``(62,199)``
    - two fixed metric fields ``mask_u`` and ``mask_v`` on the same grid
and to receive from GZ21 model:
    - two forcing fields ``u_f`` and ``v_f`` with the same dimensions


**with corresponding Eophis exchange definition:**

.. code-block:: python

    {'freq' : step, 'grd' : 'DINO_Grid', 'lvl' : nlvl, 'in' : ['u','v'], 'out' : ['u_f','v_f']}



Python fields
~~~~~~~~~~~~~

Module ``pyfld.F90`` is dedicated to the definition of arrays containing the fields returned by the external Python script.


.. code-block:: fortran

    REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: ext_uf, ext_vf !: Outsourced subgrid momentum forcing fields


The advantage is that we just need to store the results exchanged and/or computed by the communication module in ``ext_uf`` and ``ext_vf``. Then, they can be imported to be used by any other NEMO4 module with:

.. code-block:: fortran

    USE pyfld


Two functions to (de)allocate the arrays with the right grid size are present in the module and must be correctly set. For instance:

.. code-block:: fortran

   SUBROUTINE init_python_fields()
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE init_python_fields  ***
      !!
      !! ** Purpose :   Initialisation of the Python module
      !!
      !! ** Method  :   * Allocate arrays for Python fields
      !!                * Configure Python coupling
      !!----------------------------------------------------------------------
      !
      ! Allocate fields
      ALLOCATE( ext_uf(jpi,jpj,jpk) , ext_vf(jpi,jpj,jpk) )
      !
      ! configure coupling
      CALL init_python_coupling()
      !
   END SUBROUTINE init_python_fields
   
   
.. code-block:: fortran

   SUBROUTINE finalize_python_fields()
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE finalize_python_fields  ***
      !!
      !! ** Purpose :   Free memory used by Python module
      !!
      !! ** Method  :   * deallocate arrays for Python fields
      !!                * deallocate Python coupling
      !!----------------------------------------------------------------------
      !
      ! Free memory
      DEALLOCATE( ext_uf, ext_vf )
      !
      ! terminate coupling environment
      CALL finalize_python_coupling()
      !
   END SUBROUTINE finalize_python_fields

They are automatically called during NEMO4 initialization and termination.


Exchanging fields
~~~~~~~~~~~~~~~~~

Configuration of OASIS following Eophis definitions in NEMO4 is automatically handled by the ``pycpl.F90`` module. The latter simply provides two functions to send and receive fields in accordance with the Eophis framework:


.. code-block:: fortran

    CALL send_to_python(varname, to_send, kt)
    ! CHAR                    :: varname  ! field name defined in Eophis
    ! REAL                    :: to_send  ! array to send, 2D or 3D
    ! INTEGER                 :: kt       ! time step
    
    # ---------------------------------------------------------------- #
    ! Example to send velocity field uu in pipeline "u"
    CALL send_to_python('u', uu(:,:,:,Kbb), kstp)


.. code-block:: fortran

    CALL receive_from_python(varname, to_rcv, kt)
    ! CHAR                    :: varname  ! field name defined in Eophis
    ! REAL                    :: to_rcv   ! array that will receive data, 2D or 3D
    ! INTEGER                 :: kt       ! time step
    
    # ---------------------------------------------------------------- #
    ! Example to receive forcing term from pipeline "u_f" in ext_uf
    CALL receive_from_python('u_f', ext_uf, kstp)


Those can be used anywhere in NEMO by importing the ``pycpl`` module. For more convenience, it can be useful to gather all sendings and/or receivings in a same routine. For instance, ``stpmlf.F90`` calls ``inputs_gz21()`` and ``update_from_gz21()`` in ``pyfld.F90`` at different moments of time loop to take advantage of parallel execution of NEMO4 and the Python scripts. The former gather all sendings, and the latter gathers all receptions.



Compile NEMO
------------

After configurating the communication module, NEMO must be compiled with active *key_oasis3* CPP key and OASIS_v5.0 (see this `guide <https://morays-doc.readthedocs.io/en/latest/nemo.getting_started.html#morays-environment>`_).
