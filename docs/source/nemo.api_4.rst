NEMO4 for Morays
================

.. toctree::
   :maxdepth: 2

Let's recall that a Morays experiment is an ocean simulation that exchanges fields with an external Python script through OASIS. The strategy is to deploy the OASIS interface in the Python side and configure the global coupling environment with help of the Eophis library.

From this point, we consider that all the Python material is already well configured. See `Eophis documentation <https://eophis.readthedocs.io/en/latest/>`_ or this `tutorial <https://eophis.readthedocs.io/en/latest/tutorial.html>`_ for more details. What remains now is to configure the OASIS interface of NEMO in accordance with Eophis. This is described in this section.



Modified OASIS interface
------------------------

Current NEMO4 releases do include an OASIS module. However, it is restricted for coupling NEMO with atmospheric models only. Modifications of the NEMO4 sources are required to make it usable for other coupling purposes.

Morays provides `patches <https://github.com/morays-community/Patches-NEMO/>`_ with the abovementionned modifications of the NEMO code. More details about modifications are available `here <https://github.com/morays-community/Patches-NEMO/blob/main/README.md>`_. As a user, you just need to copy them in the ``MY_SRC`` directory of a NEMO4 config.



Developper\'s guide
~~~~~~~~~~~~~~~~~~~

We describe here how to use the OASIS interface in a new NEMO4 module dedicated to Python communication. Note that a ready-to-use communication module is provided with Morays patches, see next subsection to learn how to use it.


Register a new module
'''''''''''''''''''''

Flexibility of OASIS in NEMO relies on the fact that coupling properties of the fields to exchange are stored in meta-arrays ``ssnd`` and ``srcv`` defined in ``cpl_oasis3.F90``. A dimension has simply been added to those arrays to sort meta-data between all the other modules that are using the OASIS interface.

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
'''''''''''''''''''''

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
''''''''''''

During a time iteration, a coupled field can be sent with this function:

.. code-block:: fortran

    CALL cpl_snd( kid, kstep, ktyp, pdata, kinfo)
    ! INTEGER                :: kid    ! field index in ssnd for calling module
    ! INTEGER                :: kstep  ! ocean time-step in second
    ! INTEGER                :: ktyp   ! calling module ID
    ! REAL, DIMENSION(:,:,:) :: pdata  ! field to send (shape is (:,:,1) for 2D)
    ! INTEGER                :: kinfo  ! OAIS3 info argument

The ``pdata`` shape must be equivalent to the NEMO longitude and latitude grid sizes with halos for the two first dimensions. The third dimension is compulsory even for 2D fields and must be at least equal to the ``nlvl`` value defined during initialization.

.. code-block:: fortran

    ! Example to send surface of "u"
    USE oce ! to get u velocity field uu
    ## [...]
    INTEGER :: isec, info
    
    isec = ( kt - nit000 ) * NINT( rn_Dt )
    info = OASIS_idle
    CALL cpl_snd( jps_ssu, isec, ntypinf, uu(:,:,1:1,Kbb), info )




Receive a field
'''''''''''''''

During a time iteration, a coupled field can be received with this function:

.. code-block:: fortran

    CALL cpl_rcv( kid, kstep, ktyp, pdata, kinfo, pmask)
    ! INTEGER                :: kid    ! field index in srcv for calling module
    ! INTEGER                :: kstep  ! ocean time-step in second
    ! INTEGER                :: ktyp   ! calling module ID
    ! INTEGER                :: kinfo  ! returned OASIS3 info
    ! REAL, DIMENSION(:,:,:) :: pdata  ! returned received field (shape is (:,:,1) for 2D)
    ! REAL, DIMENSION(:,:,:) :: pmask  ! coupling mask, optional

As for sending, ``pdata`` shape must be equivalent to NEMO longitude and latitude grid sizes for the two first dimensions and ``nlvl`` for the third dimension.

.. code-block:: fortran

    ! Example to receive 2D forcing field "u_f"
    INTEGER :: isec, info
    REAL(wp), DIMENSION(jpi,jpj,1) :: uforce
    
    isec = ( kt - nit000 ) * NINT( rn_Dt )
    info = OASIS_idle
    CALL cpl_rcv( jpr_uf, isec, ntypinf, uforce, info )
    
    ! output u_f with XIOS xml files
    CALL iom_put( 'ext_uf', uforce(:,:,1) )




Configure coupling in NEMO
---------------------------

To facilitate the deployment, modification or development of a Morays experiment, Morays patches directly come with pre-defined communication modules named ``inffld.f90`` and ``infmod.f90``. This section describes how to configure them.


.. note:: You are of course free to build your own external communication module with a more sophisticated use of the OASIS interface in NEMO.


The presented examples rely on the DINO.GZ21 test case. Purposes and behaviors of DINO.GZ21 are described in the NEMO **Getting started** section of this documentation. Let's resume what both entities are exchanging.

NEMO intends to send:
    - two time-evolving velocity fields ``u`` and ``v`` at a ``2700s`` frequency, on grid ``(62,199)``
    - two fixed metric fields ``mask_u`` and ``mask_v`` on the same grid
and to receive from GZ21 model:
    - two forcing fields ``u_f`` and ``v_f`` with the same dimensions



Coupled Fields
~~~~~~~~~~~~~~

``inffld.F90`` is dedicated to the definition of arrays containing the fields returned by the external Python script. It does not use the OASIS interface.


.. code-block:: fortran

    REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: ext_uf, ext_vf !: Outsourced subgrid momentum forcing fields


The advantage is that we just need to store the results exchanged and/or computed by the communication module in ``ext_uf`` and ``ext_vf``. Then, they can be imported to be used by any other NEMO4 module with:

.. code-block:: fortran

    USE inffld


Two functions to (de)allocate the arrays with the right grid size are present in the module and must be correctly set. For instance:

.. code-block:: fortran

   INTEGER FUNCTION inffld_alloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION inffld_alloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      ierr = 0
      !
      ALLOCATE( ext_uf(jpi,jpj,jpk) , ext_vf(jpi,jpj,jpk)  , STAT=ierr )
      inffld_alloc = ierr
      !
   END FUNCTION
   
   
.. code-block:: fortran

   INTEGER FUNCTION inffld_dealloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION inffld_dealloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      ierr = 0
      !
      DEALLOCATE( ext_uf , ext_vf  , STAT=ierr )
      inffld_dealloc = ierr
      !
   END FUNCTION

They are automatically called during the communication module initialization and termination.




Communication module
~~~~~~~~~~~~~~~~~~~~

``infmod.F90`` is the communication module that uses the OASIS interface to send ``u``, ``v``, ``mask_u``, ``mask_v`` and receive ``u_f``, ``v_f`` in DINO.GZ21. Its structure follows the one described in **Developper'\s guide** subsection.

The communication module must be activated in first place by setting ``ln_inf`` to ``.TRUE.`` in the NEMO4 namelist. Then, define the IDs of sending and receiving fields in the global module section :

.. code-block:: fortran

   INTEGER, PARAMETER ::   jps_ssu = 1    ! u surface velocity
   INTEGER, PARAMETER ::   jps_ssv = 2    ! v surface velocity
   INTEGER, PARAMETER ::   jps_mu = 3    ! u mask
   INTEGER, PARAMETER ::   jps_mv = 4    ! v mask
   INTEGER, PARAMETER ::   jps_inf = 4   ! total number of sendings for inferences

   INTEGER, PARAMETER ::   jpr_uf = 1    ! u subgrid forcing field
   INTEGER, PARAMETER ::   jpr_vf = 2    ! v subgrid forcing field
   INTEGER, PARAMETER ::   jpr_inf = 2   ! total number of inference receptions

   INTEGER, PARAMETER ::   jpinf = MAX(jps_inf,jpr_inf) ! Maximum number of exchanges

   TYPE( DYNARR ), SAVE, DIMENSION(jpinf) ::  infsnd, infrcv  ! sent/received inferences
   
   
Fields attributes must be then defined with ``ssnd`` and ``srcv`` in the ``inferences_init()`` subroutine, as shown in the previous subsection.


Sendings and receivings are performed in the ``inferences()`` subroutine. It takes the simulation time and time indexes as arguments. More variables may be passed through the subroutine, as more complex fields required by the external ML model.

``inferences()`` uses two lists of arrays ``infsnd`` and ``infrcv`` to store the fields to pass through ``cpl_snd()`` and ``cpl_rcv()``. Fill them with what will be exchanged with OASIS, for instance with the U-grid surface velocity and mask:

.. code-block:: fortran

      ! Sea Surface U velocity
      IF( ssnd(ntypinf,jps_ssu)%laction ) THEN
         infsnd(jps_ssu)%z3(:,:,1:ssnd(ntypinf,jps_ssu)%nlvl) = puu(:,:,1:ssnd(ntypinf,jps_ssu)%nlvl,Kbb)
      ENDIF
      ! u-grid surface mask
      IF( ssnd(ntypinf,jps_mu)%laction ) THEN
          infsnd(jps_mu)%z3(:,:,1:ssnd(ntypinf,jps_mu)%nlvl) = umask(:,:,1:ssnd(ntypinf,jps_mu)%nlvl)
      ENDIF


All sendings and receptions are then performed, in that order. Received fields are stored in ``infrcv`` and may be recovered as in this example:


.. code-block:: fortran

      IF( srcv(ntypinf,jpr_uf)%laction .AND. srcv(ntypinf,jpr_vf)%laction ) THEN
         ! Store received external forcing fields
         ext_uf(:,:,1:srcv(ntypinf,jpr_uf)%nlvl) = infrcv(jpr_uf)%z3(:,:,1:srcv(ntypinf,jpr_uf)%nlvl)
         ext_vf(:,:,1:srcv(ntypinf,jpr_vf)%nlvl) = infrcv(jpr_vf)%z3(:,:,1:srcv(ntypinf,jpr_vf)%nlvl)

         ! Output results
         CALL iom_put( 'ext_uf', ext_uf(:,:,1) )
         CALL iom_put( 'ext_vf', ext_vf(:,:,1) )

         ! Apply forcing fields to RHS
         puu(:,:,1:srcv(ntypinf,jpr_uf)%nlvl,Krhs) = puu(:,:,1:srcv(ntypinf,jpr_uf)%nlvl,Krhs) + ext_uf(:,:,1:srcv(ntypinf,jpr_uf)%nlvl)
         pvv(:,:,1:srcv(ntypinf,jpr_vf)%nlvl,Krhs) = pvv(:,:,1:srcv(ntypinf,jpr_vf)%nlvl,Krhs) + ext_vf(:,:,1:srcv(ntypinf,jpr_vf)%nlvl)
      ENDIF


Note that the received fields are directly added to the NEMO RHS that is passed as argument. This means that ``inferences()`` must be called at the right moment of the time integration. Here, it is done in ``stpmlf.F90``.

.. note:: This communication module works fine but is a bit clumsy to our taste. It is planned for future work to propose a finer one with Morays patches.


Compile NEMO
------------

After configurating the communication module, NEMO must be compiled with active *key_oasis3* CPP key and OASIS_v5.0 (see this `guide <https://morays-doc.readthedocs.io/en/latest/nemo.getting_started.html#morays-environment>`_).
