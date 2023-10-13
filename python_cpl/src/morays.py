#import argparse
from smart_morays.pycpl import open_tunnel, Tunnel
import smart_morays.inferences as Models
from smart_morays.loop import init_morays, coupled_run, finish_morays, Freqs

def main():

    #  Init Morays
    # +++++++++++++
    init_morays()

    #  Couple Ocean
    # ++++++++++++++
    tunnel_config = dict({ 'label' : 'Morays', \
                           'grids' : dict({'torc' : [720,603] ,\
                                           'lmdz' : [180,151] }),
                           'vars' : [  dict({'in' : 'sst', 'out' : 'sst_var', 'grd' : 'torc', 'lvl' : 1, 'freq' : Freqs.DAILY }),  \
                                       dict({'in' : 'svt', 'out' : 'svt_var', 'grd' : 'torc', 'lvl' : 3, 'freq' : Freqs.DAILY }) ] \
                        })
                
    nemo = open_tunnel(**tunnel_config)

    #  Couple Model
    # ++++++++++++++
    model_config = dict({ 'label'    : '', \
                          'weights'  : '', \
                          'features' : [], \
                          'preds'    : [], \
                        })
    
    
    gtf = Models.LTPytorch_Model('GeoTrainFlow')

    #  Run Simulation
    # ++++++++++++++++
    
    # namelist
    namelist = nml.read('namelist_cfg')

    time_step = int( namelist['namdom']['rn_Dt']  )
    niter = int( namelist['namrun']['nn_itend'] - namelist['namrun']['nn_it000'] )

    coupled_run(nemo,gtf,time_step,niter)
    #uncoupled_run(gtf)

    #  Terminate
    # +++++++++++
    finish_morays(nemo,gtf)
 
 
if __name__=='__main__':
   main()
