#!/usr/bin/env python3

# oasis modules
import pyoasis
from pyoasis import OASIS
from mpi4py import MPI
# utils modules
import f90nml as nml
import numpy
import logging


# +++++++++++++++++++
#   INITIALISATION
# +++++++++++++++++++

comm = MPI.COMM_WORLD
component_name = 'pycpl'
comp = pyoasis.Component(component_name,True,comm)

comm_rank = comp.localcomm.rank
comm_size = comp.localcomm.size

if comm_rank == 0:
    logging.basicConfig(filename='pycpl.root',encoding='utf-8',level=logging.INFO)

    logging.info('  -----------------------------------------------------------')
    logging.info('  Component name: %s with ID: %.1i' % (component_name,comp._id))
    logging.info('  Running with %.1i processes' % comm_size)
    logging.info('  -----------------------------------------------------------')

# Get namelists
namelist = nml.read('namelist_cfg')

# ++++++++++++++++++++
#   GRID DEFINITION
# ++++++++++++++++++++

# Grid info directly given for now --> TO BE IMPROVED
grid = 'torc'
if grid == 'torc':
    grd_src = 'torc'
    nlon = 720
    nlat = 603
else:
    grd_src = 'lmdz'
    nlon = 180
    nlat = 148

if comm_rank == 0:
    logging.info('  Grid name : %s' % grd_src)
    logging.info('  Grid size : %.1i %.1i' % (nlon, nlat))
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

var_name = ['I_ML','O_ML']

# received variables (bundle_size is equivalent to second value of var_nodim in F90)
var_i_atssh = pyoasis.Var(var_name[0],partition,OASIS.IN,bundle_size=1)

# sent variables
var_o_atssh = pyoasis.Var(var_name[1],partition,OASIS.OUT,bundle_size=1)

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
field = numpy.zeros((local_size,1))
field_recv = pyoasis.asarray(field)

for it in range(niter):
    it_sec = int(time_step * it)

    # receive field from Ocean
    if it_sec%var_i_atssh.cpl_freqs[0] == 0.0:
        logging.info('  Waiting for receiving %s at time %.1i / %.1i ' % (var_i_atssh._name,it_sec,total_time))

    var_i_atssh.get(it_sec,field_recv)

    if it_sec%var_i_atssh.cpl_freqs[0] == 0.0 and comm_rank == 0:
        logging.info('  Receiving successful')

    # Modify field
    if it_sec%var_i_atssh.cpl_freqs[0] == 0.0:
        field = field_recv + 10.0

    # send back field to Ocean
    if it_sec%var_o_atssh.cpl_freqs[0] == 0.0:
        logging.info('  Waiting for sending %s at time %.1i / %.1i ' % (var_o_atssh._name,it_sec,total_time))

    field_send = pyoasis.asarray(field)
    var_o_atssh.put(it_sec,field_send)

    if it_sec%var_o_atssh.cpl_freqs[0] == 0.0 and comm_rank == 0:
        logging.info('  Sending successful')

if comm_rank == 0:
    logging.info('  End Of Loop')

# +++++++++++++++++++++++++
#   TERMINATION
# +++++++++++++++++++++++++
del comp

if comm_rank == 0:
    logging.info('  End Of The Program')
