# eophis API
import eophis
from eophis import Freqs, Grids
# other modules
import os

def main():
    #  Log examples
    # ++++++++++++++
    eophis.info('========= Running Case: NEMO-EOPHIS DEMO =========')


    #  Ocean
    # +++++++
    nemo_nml = eophis.FortranNamelist(os.path.join(os.getcwd(),"namelist_cfg"))
    step, it_end, it_0 = nemo_nml.get('rn_Dt','nn_itend','nn_it000')
    niter = it_end - it_0
    total_time = niter * step

    # coupling config
    tunnel_config = dict({ 'label' : 'TO_NEMO', \
                           'grids' : { 'torc' : Grids.eORCA05,   \
                                       'lmdz' : [180,151,0,0]    } \
                           'exchs' : [ {'freq' : Freqs.DAILY,  'grd' : 'torc', 'lvl' : 1, 'in' : ['sst'], 'out' : ['sst_var'] },   \
                                       {'freq' : Freqs.HOURLY, 'grd' : 'torc', 'lvl' : 3, 'in' : ['svt'], 'out' : ['svt_var'] }  ] \
                        })

    # tunnel creation (Lazy)
    nemo = eophis.register_tunnels( tunnel_config )
    
    # link all tunnels
    eophis.open_tunnels(opening_time = total_time)

    #  Models
    # ++++++++
    from models import add_100, Std_Stanley, GTF_LinReg_Stanley, GTF_FCNN_Stanley, GTF_CNN_Stanley


    #  Assemble
    # ++++++++++
    @eophis.all_in_all_out(earth_system=nemo, step=step, niter=niter)
    def loop_core(**inputs):
    
        outputs['sst_var'] = add_100(inputs['sst'])
        outputs['svt_var'] = add_100(inputs['svt'])
        #outputs['rho'] = GTF_LinReg_Grooms(inputs['sst'])
  
        return outputs


    #  Run
    # +++++
    start_eophis(loop_core)
    
    # Auto-Clean at closure
# ============================ #
if __name__=='__main__':
   main()
