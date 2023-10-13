import f90nml as nml
import logging
from smart_morays.pycpl import Tunnel
from smart_morays.inferences import TrainedModel

logger = logging.getLogger(__name__)

def init_morays():

    # open logger
    logging.basicConfig(filename='morays.log',encoding='utf-8',level=logging.INFO)

    # print packages infos
    #logging.info(watermark(packages="torch,mpi4py,numpy",python=True))


def finish_morays(ocean,model):
    rank = ocean.comm_rank = 0
    del ocean
    del model
 
    if rank == 0:
        logging.info('  Morays successfully finished')


def coupled_run(ocean,model,time_step,niter):

    if ocean.comm_rank == 0:
        logging.info('  -------------------------- RUN LOOP ---------------------------------')
        logging.info('  Number of iterations : %.1i' % niter)
        logging.info('  Time step : %.1i' % time_step)
        logging.info('  Simulation length : %.1i \n' % total_time)

    for it in range(niter):
        it_sec = int(time_step * it)

    if ocean.comm_rank == 0:
        logging.info('  End Of Loop')
        logging.info('  -----------------------')

