# oasis modules
import pyoasis
from pyoasis import OASIS
from mpi4py import MPI
# utils modules
import f90nml as nml
import numpy
import logging

logger = logging.getLogger(__name__)

def pycpl():
    # +++++++++++++++++++
    #   INITIALISATION
    # +++++++++++++++++++
    comm = MPI.COMM_WORLD
    component_name = 'moray'
    comp = pyoasis.Component(component_name,True,comm)

    comm_rank = comp.localcomm.rank
    comm_size = comp.localcomm.size

    if comm_rank == 0:
        logging.info('  -----------------------------------------------------------')
        logging.info('  Component name: %s with ID: %.1i' % (component_name,comp._id))
        logging.info('  Running with %.1i processes' % comm_size)
        logging.info('  -----------------------------------------------------------')

    # ++++++++++++++++++++
    #   GRID DEFINITION
    # ++++++++++++++++++++
    # Grid info directly given for now --> TO BE IMPROVED
    grid = 'torc'
    if grid == 'torc':
        grd_src = 'torc'
        nlon = 720
        nlat = 603
        nlvl = 3 #121
    else:
        grd_src = 'lmdz'
        nlon = 180
        nlat = 148

    if comm_rank == 0:
        logging.info('  Grid name : %s' % grd_src)
        logging.info('  Grid size : %.1i %.1i %.1i' % (nlon, nlat, nlvl))
        logging.info('  End Of Grid Def')

    # +++++++++++++++++++++++++
    #   PARTITION DEFINITION
    # +++++++++++++++++++++++++
    local_size = int(nlon * nlat / comm_size)
    offset = comm_rank * local_size

    if comm_rank == comm_size - 1:
        local_size = nlon * nlat - offset

    partition = pyoasis.ApplePartition(offset,local_size)
    part_id = partition._id

    if comm_rank == 0:
        logging.info('  End Of Part Def')

    # +++++++++++++++++++++++++
    #   VARIABLES DEFINITION
    # +++++++++++++++++++++++++
    var_in = ['DAT_SST','DAT_SVT']
    var_out = ['INF_SST','INF_SVT']

    # received variables (bundle_size is equivalent to second value of var_nodim in F90)
    dat_sst = pyoasis.Var(var_in[0],partition,OASIS.IN,bundle_size=1)
    dat_svt = pyoasis.Var(var_in[1],partition,OASIS.IN,bundle_size=nlvl)

    if comm_rank == 0:
        logging.info('Var id : %.1i %.1i' % (dat_sst._id, dat_svt._id))

    # sent variables
    inf_sst = pyoasis.Var(var_out[0],partition,OASIS.OUT,bundle_size=1)
    inf_svt = pyoasis.Var(var_out[1],partition,OASIS.OUT,bundle_size=nlvl)

    if comm_rank == 0:
        logging.info('  End Of Var Def')

    # +++++++++++++++++++++++++
    #   END OF DEFINITION
    # +++++++++++++++++++++++++

    comp.enddef()

    if comm_rank == 0:
        logging.info('  End Of Definition')


    # +++++++++++++++++++++++++
    #   RUN EXCHANGES
    # +++++++++++++++++++++++++

    # Get namelists
    namelist = nml.read('namelist_cfg')

    time_step = int( namelist['namdom']['rn_Dt']  )
    niter = int( namelist['namrun']['nn_itend'] - namelist['namrun']['nn_it000'] )
    total_time = niter*time_step

    if comm_rank == 0:
        logging.info('  -----------------------------------------------------------')
        logging.info('  Number of iterations : %.1i' % niter)
        logging.info('  Time step : %.1i' % time_step)
        logging.info('  Simulation length : %.1i' % total_time)
        logging.info('  -----------------------------------------------------------')

    # Init Field
    # NB: Fields to send/receive must be formatted with pyoasis.asarray
    sst = pyoasis.asarray( numpy.zeros((local_size,1)) )
    svt = pyoasis.asarray( numpy.zeros((local_size,nlvl)) )

    for it in range(niter):
        it_sec = int(time_step * it)

        # receive field from Ocean
        if it_sec%dat_sst.cpl_freqs[0] == 0.0:
            logging.info('  Waiting for receiving %s at time %.1i / %.1i ' % (dat_sst._name,it_sec,total_time))

        if dat_sst._id != -1:
            dat_sst.get(it_sec,sst)

        if it_sec%dat_sst.cpl_freqs[0] == 0.0 and comm_rank == 0:
            logging.info('  Receiving successful')

        # Modify field
        if it_sec%inf_sst.cpl_freqs[0] == 0.0:
            var_sst = pyoasis.asarray( sst + 10.0 )

        # send back field to Ocean
        if it_sec%inf_sst.cpl_freqs[0] == 0.0:
            logging.info('  Waiting for sending %s at time %.1i / %.1i ' % (inf_sst._name,it_sec,total_time))
 
        if inf_sst._id != -1:
            inf_sst.put(it_sec,var_sst)

        if it_sec%inf_sst.cpl_freqs[0] == 0.0 and comm_rank == 0:
            logging.info('  Sending successful')

        # == == == ==

        # receive field from Ocean
        if it_sec%dat_svt.cpl_freqs[0] == 0.0:
            logging.info('  Waiting for receiving %s at time %.1i / %.1i ' % (dat_svt._name,it_sec,total_time))

        if dat_svt._id != -1:
            dat_svt.get(it_sec,svt)

        if it_sec%dat_svt.cpl_freqs[0] == 0.0 and comm_rank == 0:
            logging.info('  Receiving successful')

        # Modify field
        if it_sec%inf_svt.cpl_freqs[0] == 0.0:
            var_svt = pyoasis.asarray( svt + 100.0 )

        # send back field to Ocean
        if it_sec%inf_svt.cpl_freqs[0] == 0.0:
            logging.info('  Waiting for sending %s at time %.1i / %.1i ' % (inf_svt._name,it_sec,total_time))

        if inf_svt._id != -1:
            inf_svt.put(it_sec,var_svt)

        
        if it_sec%inf_svt.cpl_freqs[0] == 0.0 and comm_rank == 0:
           logging.info('  Sending successful')

    if comm_rank == 0:
        logging.info('  End Of Loop')


    # +++++++++++++++++++++++++
    #   TERMINATION
    # +++++++++++++++++++++++++
    del comp

    if comm_rank == 0:
        logging.info('  End Of The Program')
