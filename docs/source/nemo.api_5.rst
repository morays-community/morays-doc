NEMO5 for Morays
================

.. toctree::
   :maxdepth: 2

.. warning:: This section is likely to change since NEMO5 is still in beta testing phase.

Let's recall that a Morays experiment is an ocean simulation that exchanges fields with an external Python script through OASIS. The strategy is to deploy the OASIS interface in the Python side and configure the global coupling environment with help of the Eophis library.

From this point, we consider that all the Python material is already well configured. See `Eophis documentation <https://eophis.readthedocs.io/en/latest/>`_ or this `upcoming tutorial` for more details. What remains now is to configure the OASIS interface of NEMO in accordance with Eophis. This is described in this section.



Modified OASIS interface
------------------------

Section **NEMO4 for Morays** explains that NEMO needs modifications from `Morays patches <https://github.com/morays-community/Patches-NEMO/>`_ to couple with Python scripts. This is not required anymore in NEMO5 since modifications have been integrated with release. However, modifications are slightly different compared to NEMO4. We describe here how to use the modified OASIS interface to create a new NEMO5 module dedicated to Python communication. Note that Morays still provides patches for NEMO5 with a ready-to-use communication module, see corresponding subsection to learn how to use it.


Developper\'s guide
-------------------


Overview
~~~~~~~~

Coupling properties of the fields to send and receive are stored in ``ssnd`` and ``srcv`` arrays, respectively. Flexibility of OASIS in NEMO relies on an extra dimension in ``ssnd`` and ``srcv`` to sort coupling fields properties between the modules that are using the OASIS interface.

However, some modules will manipulate more fields than others and, in some configuration, not all modules might be used. To optimize memory, ``ssnd`` and ``srcv`` are now variables that allows to create batches of coupling properties arrays. This way, it is possible to allocate different array sizes according to the modules. For example:

.. code-block :: Fortran

    ssnd( mod_ID_1 )%fld( field_ID_1 )%field_property_1
    srcv( mod_ID_1 )%fld( field_ID_2 )%field_property_2
    ssnd( mod_ID_2 )%fld( field_ID_3 )%field_property_3


- First level dimension of ``ssnd / srcv`` corresponds to module IDs. It is automatically allocated if OASIS key is activated.

- Second level dimension corresponds to IDs of coupled fields you wish the module to manage. This dimension is not allocated by default to save memory.


Register a new module
~~~~~~~~~~~~~~~~~~~~~

The first thing to do is to register your module ID in the ``cpl_oasis3.F90`` global parameters:

.. code-block :: Fortran

    vi cpl_oasis3.F90
    ##   [...]
    92   INTEGER, PUBLIC, PARAMETER ::   nmodmax=2    ! Maximum number of identified modules
    93   INTEGER, PUBLIC, PARAMETER ::   nmodsbc=1    ! module ID #1 : surface boundary condition
    94   INTEGER, PUBLIC, PARAMETER ::   nmodext=2    ! module ID #2 : external communication module

In this example, two modules are registered:
    1. SBC module used by NEMO to couple with atmosphere
    2. EXT module used in Morays framework to couple Python script


Each time a module calls the OASIS routines within NEMO, the ID of the calling module must be passed as argument. Set an ID for your new module and create it. Then, import the OASIS module:

.. code-block :: fortran

    USE cpl_oasis3



Define coupled fields
~~~~~~~~~~~~~~~~~~~~~

As in many codes, your module must contain an initialization routine in which the coupling properties of the exchanged fields are defined. All initialization routines that use the OASIS interface must be called during the NEMO general initialization phase in ``nemogcm.F90``, after ``cpl_domain()`` and before ``cpl_define()``. For example:


.. code-block:: fortran

    vi nemogcm.F90
    ###  [...]
    391  IF ( lk_oasis )  CALL  cpl_domain                   ! Define coupling domain for oasis
    ###  [...]
    444                   CALL  sbc_init( Nbb, Nnn, Naa )    ! SBC : surface boundary conditions
    445                   CALL  bdy_init                     ! Open boundaries
    446                   CALL  extcom_init                  ! init external coupled model
    447  IF ( lk_oasis )  CALL  cpl_define                   ! setup coupling environment


Main purpose of the initialization routine is to fill ``ssnd`` and ``srcv`` second-level array. Second-level dimension must be first allocated in accordance with the number of coupling fields you wish your module to manage:

.. code-block :: Fortran

    ALLOCATE( srcv(nmodext)%fld( jpr_ext ) )
    ALLOCATE( ssnd(nmodext)%fld( jps_ext ) )

IDs of the coupled fields are locally defined by the considered module, proceed as you prefer.


Each identified field in the second-level array has a list of attributes to fill in accordance with the exchange you wish to realize (i.e. the exchanges defined in the Eophis script):

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

For instance, here are the properties for two variables ``u`` and ``u_f`` whose IDs are ``jps_ssu`` and ``jpr_uf``. They are defined in the communication module whose ID is ``nmodext``.

.. code-block:: fortran

    ! default values for ssnd and srcv batches
    srcv(nmodext)%fld(:)%laction = .FALSE.  ;  srcv(nmodext)%fld(:)%clgrid = 'T'  ;  srcv(nmodext)%fld(:)%nsgn = 1.
    srcv(nmodext)%fld(:)%nct = 1  ;  srcv(nmodext)%fld(:)%nlvl = 1
    !
    ssnd(nmodext)%fld(:)%laction = .FALSE.  ;  ssnd(nmodext)%fld(:)%clgrid = 'T'  ;  ssnd(nmodext)%fld(:)%nsgn = 1.
    ssnd(nmodext)%fld(:)%nct = 1  ;  ssnd(nmodext)%fld(:)%nlvl = 1

    ! Properties for sending 2D surface U-velocity : "u" in Eophis
    ssnd( nmodext )%fld( jps_ssu )%clname = 'E_OUT_0'
    ssnd( nmodext )%fld( jps_ssu )%laction = .TRUE.
    ssnd( nmodext )%fld( jps_ssu )%clgrid = 'U'
      
    ! Properties for receiving 2D forcing term on U-grid : "u_f" in Eophis
    srcv( nmodext )%fld( jpr_uf )%clname = 'E_IN_0'
    srcv( nmodext )%fld( jpr_uf )%laction = .TRUE.
    srcv( nmodext )%fld( jpr_uf )%clgrid = 'U'


The attribute ``clname`` corresponds to the alias that OASIS uses to perform the communications. ``u_f`` is manipulated by OASIS under ``E_IN_0`` from the NEMO side and under another name from the Python side. Those aliases are used in the OASIS namelist. ``clname`` must absolutely be in accordance with the content of the OASIS namelist.

.. note:: Eophis provides useful `tools <https://eophis.readthedocs.io/en/latest/usage.html#oasis-namcouple>`_ to know and manipulate OASIS aliases.


Once ``ssnd`` and ``srcv`` have been correctly filled. Finalize fields registration with:

.. code-block:: fortran

    CALL cpl_var( krcv, ksnd, kcplmodel, kmod)
    ! INTEGER :: krcv      ! number of fields to receive for current module
    ! INTEGER :: ksnd      ! number of fields to send for current module
    ! INTEGER :: kcplmodel ! Maximum number of models to/from which NEMO is potentialy sending/receiving data
    ! INTEGER :: kmod      ! calling module ID


.. code-block:: fortran

    ! For above u and u_f examples
    CALL cpl_var( 1, 1, 1, ntypinf )



Send a field
~~~~~~~~~~~~

During a time iteration, a coupled field can be sent with this function:

.. code-block:: fortran

    CALL cpl_snd( kmod, kid, kstep, pdata, kinfo)
    ! INTEGER                :: kmod   ! calling module ID
    ! INTEGER                :: kid    ! field index in ssnd for calling module
    ! INTEGER                :: kstep  ! ocean time-step in second
    ! REAL, DIMENSION(:,:,:) :: pdata  ! field to send (shape is (:,:,1) for 2D)
    ! INTEGER                :: kinfo  ! OAIS3 info argument

The ``pdata`` shape must be equivalent to the NEMO longitude and latitude grid sizes without halos for the two first dimensions. The third dimension is compulsory even for 2D fields and must be at least equal to the ``nlvl`` value defined during initialization.

.. code-block:: fortran

    ! Example to send surface of "u"
    USE oce ! to get u velocity field uu
    ## [...]
    INTEGER :: isec, info
    
    isec = ( kt - nit000 ) * NINT( rn_Dt )
    info = OASIS_idle
    CALL cpl_snd( nmodext, jps_ssu, isec, uu(A2D(0),1:1,Kbb), info )




Receive a field
~~~~~~~~~~~~~~~

During a time iteration, a coupled field can be received with this function:

.. code-block:: fortran

    CALL cpl_rcv( kmod, kid, kstep, pdata, kinfo, pmask)
    ! INTEGER                :: kmod   ! calling module ID
    ! INTEGER                :: kid    ! field index in srcv for calling module
    ! INTEGER                :: kstep  ! ocean time-step in second
    ! INTEGER                :: kinfo  ! returned OASIS3 info
    ! REAL, DIMENSION(:,:,:) :: pdata  ! returned received field (shape is (:,:,1) for 2D)
    ! REAL, DIMENSION(:,:,:) :: pmask  ! coupling mask, optional

As for sending, ``pdata`` shape must be equivalent to NEMO longitude and latitude grid sizes without halos for the two first dimensions and ``nlvl`` for the third dimension.

.. code-block:: fortran

    ! Example to receive 2D forcing field "u_f"
    INTEGER :: isec, info
    REAL(wp), DIMENSION(jpi,jpj,1) :: uforce
    
    isec = ( kt - nit000 ) * NINT( rn_Dt )
    info = OASIS_idle
    CALL cpl_rcv( nmodext, jpr_uf, isec, uforce(A2D(0),:), info )
    
    ! output u_f with XIOS xml files
    CALL iom_put( 'ext_uf', uforce(:,:,1) )




Configure coupling in NEMO
---------------------------

To facilitate the deployment, modification or development of a Morays experiment, Morays provides pre-defined communication modules for NEMO5 named ``exfld.f90`` and ``extcom.f90``. This section describes how to configure them.


.. note:: You are of course free to build your own external communication module with a more sophisticated use of the OASIS interface in NEMO.


The presented examples rely on the DINO.GZ21 test case. Purposes and behaviors of DINO.GZ21 are described in the NEMO **Getting started** section of this documentation. Let's resume what both entities are exchanging.

NEMO intends to send:
    - two time-evolving velocity fields ``u`` and ``v`` at a ``2700s`` frequency, on grid ``(62,199)``
    - two fixed metric fields ``mask_u`` and ``mask_v`` on the same grid
and to receive from GZ21 model:
    - two forcing fields ``u_f`` and ``v_f`` with the same dimensions


Coupled Fields
~~~~~~~~~~~~~~

``extfld.F90`` is dedicated to the definition of arrays containing the fields returned by the external Python script. It does not use the OASIS interface.


.. code-block:: fortran

    REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: ext_uf, ext_vf !: Outsourced subgrid momentum forcing fields


The advantage is that we just need to store the results exchanged and/or computed by the communication module in ``ext_uf`` and ``ext_vf``. Then, they can be imported to be used by any other NEMO5 module with:

.. code-block:: fortran

    USE extfld


Two functions to (de)allocate the arrays with the right grid size are present in the module and must be correctly set. For instance:

.. code-block:: fortran

   INTEGER FUNCTION extfld_alloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION extfld_alloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      ierr = 0
      !
      ALLOCATE( ext_uf(jpi,jpj,jpk) , ext_vf(jpi,jpj,jpk)  , STAT=ierr )
      extfld_alloc = ierr
      !
   END FUNCTION
   
   
.. code-block:: fortran

   INTEGER FUNCTION extfld_dealloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION extfld_dealloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      ierr = 0
      !
      DEALLOCATE( ext_uf , ext_vf  , STAT=ierr )
      extfld_dealloc = ierr
      !
   END FUNCTION

They are automatically called during the communication module initialization and termination.




Communication module
~~~~~~~~~~~~~~~~~~~~

``extcom.F90`` is the communication module that uses the OASIS interface to send ``u``, ``v``, ``mask_u``, ``mask_v`` and receive ``u_f``, ``v_f`` in DINO.GZ21. Its structure follows the one described in **Developper'\s guide** subsection.

The communication module must be activated in first place by setting ``ln_ext`` to ``.TRUE.`` in the NEMO namelist. Then, define the IDs of sending and receiving fields in the global module section :

.. code-block:: fortran

   INTEGER, PARAMETER ::   jps_ssu = 1    ! u surface velocity
   INTEGER, PARAMETER ::   jps_ssv = 2    ! v surface velocity
   INTEGER, PARAMETER ::   jps_mu = 3    ! u mask
   INTEGER, PARAMETER ::   jps_mv = 4    ! v mask
   INTEGER, PARAMETER ::   jps_ext = 4   ! total number of sendings for inferences

   INTEGER, PARAMETER ::   jpr_uf = 1    ! u subgrid forcing field
   INTEGER, PARAMETER ::   jpr_vf = 2    ! v subgrid forcing field
   INTEGER, PARAMETER ::   jpr_ext = 2   ! total number of inference receptions

   INTEGER, PARAMETER ::   jpext = MAX(jps_ext,jpr_ext) ! Maximum number of exchanges

   TYPE( DYNARR ), SAVE, DIMENSION(jpext) ::  extsnd, extrcv  ! sent/received inferences
   
   
Fields attributes must be then defined with ``ssnd`` and ``srcv`` in the ``extcom_init()`` subroutine, as shown in the previous subsection.


Sendings and receivings are performed in the ``ext_comm()`` subroutine. It takes the simulation time and time indexes as arguments. More variables may be passed through the subroutine, as more complex fields required by the external ML model.

``ext_comm()`` uses two lists of arrays ``extsnd`` and ``extrcv`` to store the fields to pass through ``cpl_snd()`` and ``cpl_rcv()``. Fill them with what will be exchanged with OASIS, for instance with the U-grid surface velocity and mask:

.. code-block:: fortran

      ! Sea Surface U velocity
      IF( ssnd(nmodext)%fld(jps_ssu)%laction ) THEN
         extsnd(jps_ssu)%z3(:,:,1:ssnd(nmodext)%fld(jps_ssu)%nlvl) = puu(:,:,1:ssnd(nmodext)%fld(jps_ssu)%nlvl,Kbb)
      ENDIF
      ! u-grid surface mask
      IF( ssnd(nmodext)%fld(jps_mu)%laction ) THEN
         extsnd(jps_mu)%z3(:,:,1:ssnd(nmodext)%fld(jps_mu)%nlvl) = umask(:,:,1:ssnd(nmodext)%fld(jps_mu)%nlvl)
      ENDIF


All sendings and receptions are then performed, in that order. Received fields are stored in ``extrcv`` and may be recovered as in this example:


.. code-block:: fortran

      IF( srcv(ntypinf,jpr_uf)%laction .AND. srcv(ntypinf,jpr_vf)%laction ) THEN
         ! Store received external forcing fields
         ext_uf(:,:,1:srcv(nmodext)%fld(jpr_uf)%nlvl) = extrcv(jpr_uf)%z3(:,:,1:srcv(nmodext)%fld(jpr_uf)%nlvl)
         ext_vf(:,:,1:srcv(nmodext)%fld(jpr_vf)%nlvl) = extrcv(jpr_vf)%z3(:,:,1:srcv(nmodext)%fld(jpr_vf)%nlvl)

         ! Output results
         CALL iom_put( 'ext_uf', ext_uf(:,:,1) )
         CALL iom_put( 'ext_vf', ext_vf(:,:,1) )

         ! Apply forcing fields to RHS
         puu(:,:,1:srcv(nmodext)%fld(jpr_uf)%nlvl,Krhs) = puu(:,:,1:srcv(nmodext)%fld(jpr_uf)%nlvl,Krhs) + ext_uf(:,:,1:srcv(nmodext)%fld(jpr_uf)%nlvl)
         pvv(:,:,1:srcv(nmodext)%fld(jpr_vf)%nlvl,Krhs) = pvv(:,:,1:srcv(nmodext)%fld(jpr_vf)%nlvl,Krhs) + ext_vf(:,:,1:srcv(nmodext)%fld(jpr_vf)%nlvl)
      ENDIF


Note that the received fields are directly added to the NEMO RHS that is passed as argument. This means that ``ext_comm()`` must be called at the right moment of the time integration. Here, it is done in ``stpmlf.F90``.

.. note:: This communication module works fine but is a bit clumsy to our taste. It is planned for future work to propose a finer one with Morays patches.


Compile NEMO
------------

After configurating the communication module, NEMO must be compiled with active *key_oasis3* CPP key and OASIS_v5.0 (see this `guide <https://morays-doc.readthedocs.io/en/latest/nemo.getting_started.html#morays-environment>`_).
