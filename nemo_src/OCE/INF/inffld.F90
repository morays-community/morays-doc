MODULE inffld
   !!======================================================================
   !!                       ***  MODULE inffld  ***
   !! Inferences module :   variables defined in core memory
   !!======================================================================
   !! History :  4.2  ! 2023-09  (A. Barge)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   inffld_alloc : allocation of inferences fields arrays
   !!----------------------------------------------------------------------        
   !!=====================================================
   USE par_oce        ! ocean parameters
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC   inffld_alloc   ! routine called in infmod.F90
   PUBLIC   inffld_dealloc ! routine called in infmod.F90

   !!----------------------------------------------------------------------
   !!                    2D Inference Models fields
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)  :: sigmaT_2D    !: variance of a 2D temperature field

   !!----------------------------------------------------------------------
   !!                    3D Inference Models fields
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: sigmaT_3D  !: variance of a 3D temperature field

CONTAINS

   INTEGER FUNCTION inffld_alloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION inffld_alloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      ierr = 0
      !
      ALLOCATE( sigmaT_2D(jpi,jpj) , sigmaT_3D(jpi,jpj,jpk)  , STAT=ierr )
      inffld_alloc = ierr
      !
   END FUNCTION

   
   INTEGER FUNCTION inffld_dealloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION inffld_dealloc  ***
      !!---------------------------------------------------------------------
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      ierr = 0
      !
      DEALLOCATE( sigmaT_2D , sigmaT_3D  , STAT=ierr )
      inffld_dealloc = ierr
      !
   END FUNCTION

END MODULE inffld
