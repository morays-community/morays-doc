MODULE infmod
   !!======================================================================
   !!                       ***  MODULE  infmod  ***
   !! Machine Learning Inferences : manage connexion with external ML codes 
   !!======================================================================
   !! History :  4.2.1  ! 2023-09  (A. Barge)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   naminf          : machine learning models formulation namelist
   !!   inferences_init : initialization of Machine Learning based models
   !!   inferences      : ML based models
   !!   inf_snd         : send data to external trained model
   !!   inf_rcv         : receive inferences from external trained model
   !!----------------------------------------------------------------------
   USE oce             ! ocean fields
   USE inffld         ! working fields for inferences models
   USE cpl_oasis3      ! OASIS3 coupling
   USE timing
   USE iom
   USE in_out_manager
   USE lib_mpp

   IMPLICIT NONE
   PRIVATE

   PUBLIC inf_alloc          ! function called in inferences_init 
   PUBLIC inf_dealloc        ! function called in inferences_final
   PUBLIC inferences_init    ! routine called in nemogcm.F90
   PUBLIC inferences         ! routine called in stpmlf.F90
   PUBLIC inf_snd            ! routine called in
   PUBLIC inf_rcv            ! routine called in
   PUBLIC inferences_final   ! routine called in nemogcm.F90

   INTEGER, PARAMETER ::   jpgtf2 = 1   ! coupling for 2D GeoTrainFlow
   INTEGER, PARAMETER ::   jpgtf3 = 2   ! coupling for 3D GeoTrainFlow 

   INTEGER, PARAMETER ::   jpinf = 2   ! total number of inferences models

   TYPE( DYNARR ), SAVE, DIMENSION(jpinf) ::  infrcv  ! received inferences
   !
   !!-------------------------------------------------------------------------
   !!                    Namelist for the Inference Models
   !!-------------------------------------------------------------------------
   !                           !!** naminf namelist **
   !TYPE ::   FLD_INF              !: Field informations ...  
   !   CHARACTER(len = 32) ::         ! 
   !END TYPE FLD_INF
   !
   LOGICAL , PUBLIC ::   ln_inf   !: activate module for inference models
   
   !!-------------------------------------------------------------------------

CONTAINS

   INTEGER FUNCTION inf_alloc()
      !!----------------------------------------------------------------------
      !!             ***  FUNCTION inf_alloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      INTEGER :: jn
      !!----------------------------------------------------------------------
      ierr = 0
      !
      DO jn = 1, jpinf
         IF( srcv(ntypinf,jn)%laction ) ALLOCATE( infrcv(jn)%z3(jpi,jpj,srcv(ntypinf,jn)%nlvl), STAT=ierr )
         inf_alloc = MAX(ierr,0)
      END DO
      !
   END FUNCTION inf_alloc

   
   INTEGER FUNCTION inf_dealloc()
      !!----------------------------------------------------------------------
      !!             ***  FUNCTION inf_dealloc  ***
      !!----------------------------------------------------------------------
      INTEGER :: ierr
      INTEGER :: jn
      !!----------------------------------------------------------------------
      ierr = 0
      !
      DO jn = 1, jpinf
         IF( srcv(ntypinf,jn)%laction ) DEALLOCATE( infrcv(jn)%z3, STAT=ierr )
         inf_dealloc = MAX(ierr,0)
      END DO
      !
   END FUNCTION inf_dealloc


   SUBROUTINE inferences_init 
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE inferences_init  ***
      !!
      !! ** Purpose :   Initialisation of the models that rely on external inferences
      !!
      !! ** Method  :   * Read naminf namelist
      !!                * create data for models
      !!----------------------------------------------------------------------
      !
      INTEGER ::   ios   ! Local Integer
      !!
      NAMELIST/naminf/  ln_inf
      !!----------------------------------------------------------------------
      !
      ! ================================ !
      !      Namelist informations       !
      ! ================================ !
      !
      READ  ( numnam_ref, naminf, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'naminf in reference namelist' )
      !
      READ  ( numnam_cfg, naminf, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'naminf in configuration namelist' )
      IF( lwm ) WRITE ( numond, naminf )
      !
      IF( lwp ) THEN                        ! control print
         WRITE(numout,*)
         WRITE(numout,*)'inferences_init : Setting inferences models'
         WRITE(numout,*)'~~~~~~~~~~~~~~~'
      END IF
      IF ( lwp .AND. ln_inf ) THEN
         WRITE(numout,*)'   Namelist naminf'
         WRITE(numout,*)'      Module used       ln_inf        = ', ln_inf
         WRITE(numout,*)'      Models available:'
         WRITE(numout,*)'         GeoTrainFlow           = ', 'T by default for now'
      ENDIF
      !
      IF( ln_inf .AND. .NOT. lk_oasis )   CALL ctl_stop( 'inferences_init : External inferences coupled via OASIS, but key_oasis3 disabled' )
      !
      !
      ! ======================================== !
      !     Define exchange needs for Models     !
      ! ======================================== !
      !
      ! default definitions of ssnd snd srcv
      srcv(ntypinf,:)%laction = .FALSE.  ;  srcv(ntypsbc,:)%clgrid = 'T'  ;  srcv(ntypinf,:)%nsgn = 1.
      srcv(ntypinf,:)%nct = 1  ;  srcv(ntypinf,:)%nlvl = 1
      !
      ssnd(ntypinf,:)%laction = .FALSE.  ;  ssnd(ntypsbc,:)%clgrid = 'T'  ;  ssnd(ntypinf,:)%nsgn = 1.
      ssnd(ntypinf,:)%nct = 1  ;  ssnd(ntypinf,:)%nlvl = 1
      
      IF( ln_inf ) THEN
      
         ! -------------------------------- !
         !      Kenigson et al. (2022)      !
         ! -------------------------------- !
      
         ! Sea Surface Temp Field <=> Sea Surface Temp Variance
         ssnd(ntypinf,jpgtf2)%clname = 'O_SST'
         ssnd(ntypinf,jpgtf2)%laction = .TRUE.

         srcv(ntypinf,jpgtf2)%clname = 'O_SSTVAR'
         srcv(ntypinf,jpgtf2)%laction = .TRUE.

         ! Sea Temp Field <=> Sea Temp Variance
         ssnd(ntypinf,jpgtf3)%clname = 'O_SVT'
         ssnd(ntypinf,jpgtf3)%laction = .TRUE.
         ssnd(ntypinf,jpgtf3)%nlvl = 3 !jpk

         srcv(ntypinf,jpgtf3)%clname = 'O_SVTVAR'
         srcv(ntypinf,jpgtf3)%laction = .TRUE.
         srcv(ntypinf,jpgtf3)%nlvl = 3 !jpk

         ! ------------------------------ !
         ! ------------------------------ !

      END IF

      ! ================================= !
      !   Define variables for coupling
      ! ================================= !
      CALL cpl_var(jpinf, jpinf, 1, ntypinf)
      !
      IF( inf_alloc() /= 0 )     CALL ctl_stop( 'STOP', 'inf_alloc : unable to allocate arrays' )
      IF( inffld_alloc() /= 0 )  CALL ctl_stop( 'STOP', 'inffld_alloc : unable to allocate arrays' ) 
      !
   END SUBROUTINE inferences_init


   SUBROUTINE inferences( kt, Kbb, Kmm, Kaa )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE inferences  ***
      !!
      !! ** Purpose :   update the ocean data with the ML based models
      !!
      !! ** Method  :   *  
      !!                * 
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt            ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Kaa ! ocean time level indices
      !
      INTEGER :: isec, info                           ! local integer
      REAL(wp), DIMENSION(jpi,jpj,jpk)   ::  zdata    ! sending buffer
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('inferences')
      !
      isec = ( kt - nit000 ) * NINT( rn_Dt )       ! Date of exchange 
      info = OASIS_idle
      !
      ! ------  Proceed exchanges via OASIS to get inferences ------
      !
      IF( ssnd(ntypinf,jpgtf2)%laction .AND. srcv(ntypinf,jpgtf2)%laction ) THEN 
         
         ! Send temperature field
         zdata(:,:,1:ssnd(ntypinf,jpgtf2)%nlvl) = ts(:,:,1:ssnd(ntypinf,jpgtf2)%nlvl,jp_tem,Kmm)
         CALL cpl_snd( jpgtf2, isec, ntypinf, zdata, info)

         ! Get inference: temperature variance
         CALL cpl_rcv( jpgtf2, isec, ntypinf, infrcv(jpgtf2)%z3, info)
         sigmaT_2D(:,:) = infrcv(jpgtf2)%z3(:,:,1)

         CALL iom_put( 'inf_sigmaT_2D', sigmaT_2D(:,:) ) ! output 2D temperature variance
      ENDIF
      !
      !
      IF( ssnd(ntypinf,jpgtf3)%laction .AND. srcv(ntypinf,jpgtf3)%laction ) THEN

         ! Send Temperature columns
         zdata(:,:,1:ssnd(ntypinf,jpgtf3)%nlvl) = ts(:,:,1:ssnd(ntypinf,jpgtf3)%nlvl,jp_tem,Kmm)
         CALL cpl_snd( jpgtf3, isec, ntypinf, zdata, info) 

         ! Get inference: temprature variance
         CALL cpl_rcv( jpgtf3, isec, ntypinf, infrcv(jpgtf3)%z3, info)
         sigmaT_3D(:,:,1:srcv(ntypinf,jpgtf3)%nlvl) = infrcv(jpgtf3)%z3(:,:,1:srcv(ntypinf,jpgtf3)%nlvl)

         CALL iom_put( 'inf_sigmaT_3D', sigmaT_3D(:,:,:) ) ! output 3D temperature variance
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('inferences')
      !
   END SUBROUTINE inferences


   SUBROUTINE inf_snd
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE inf_snd  ***
      !!
      !! ** Purpose :   send the ocean data needed for the trained models
      !!
      !! ** Method  :   *  
      !!                * 
      !!----------------------------------------------------------------------

      !

      !!----------------------------------------------------------------------


      !
   END SUBROUTINE inf_snd


   SUBROUTINE inf_rcv
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE inf_rcv  ***
      !!
      !! ** Purpose :   update the ocean data with the ML based models
      !!
      !! ** Method  :   * Receive all the fields for the models
      !!                * OASIS ...
      !!----------------------------------------------------------------------

      !

      !!----------------------------------------------------------------------


      !
   END SUBROUTINE inf_rcv


   SUBROUTINE inferences_final
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE inferences_final  ***
      !!
      !! ** Purpose :   Free memory used for inferences modules
      !!
      !! ** Method  :   * Deallocate arrays
      !!----------------------------------------------------------------------
      !
      IF( inf_dealloc() /= 0 )      CALL ctl_stop( 'STOP', 'inf_dealloc : unable to free memory' )
      IF( inffld_dealloc() /= 0 )  CALL ctl_stop( 'STOP', 'inffld_dealloc : unable to free memory' )      
      !
   END SUBROUTINE inferences_final 
   !!=======================================================================
END MODULE infmod
