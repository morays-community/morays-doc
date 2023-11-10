# eophis API
import eophis
from eophis import Freqs, Grids
# other modules
import argparse
import os
from mpi4py import MPI

def ocean_info():
    # coupling config
    tunnel_config = { 'label' : 'TO_NEMO', \
                      'grids' : { 'eORCA05' : Grids.eORCA05, \
                                  'lmdz' : (180,151,0,0)  }, \
                      'exchs' : [ {'freq' : Freqs.HOURLY, 'grd' : 'eORCA05', 'lvl' : 1, 'in' : ['sst'], 'out' : ['sst_var'] },  \
                                  {'freq' : Freqs.DAILY,  'grd' : 'eORCA05', 'lvl' : 3, 'in' : ['svt'], 'out' : ['svt_var'] } ] }
      # optional      'es_aliases' : { 'sst' : 'OCE_SST', 'svt' : 'OCE_TEMP', 'sst_var' : 'OCE_SSTV', 'svt_var' : 'OCETEMPV'},  \
      # optional      'im_aliases' : { 'sst' : 'EOP_SST', 'svt' : 'EOP_TEMP', 'sst_var' : 'EOP_SSTV', 'svt_var' : 'EOPTEMPV'}   }

    # ocean namelist
    nemo_nml = eophis.FortranNamelist(os.path.join(os.getcwd(),'namelist_cfg'))
    return tunnel_config, nemo_nml


def preproduction():
    eophis.info('========= MORAYS : Pre-Production =========')
    eophis.info('  Aim: write coupling namelist\n')

    # ocean info
    tunnel_config, nemo_nml = ocean_info()
    step, it_end, it_0 = nemo_nml.get('rn_Dt','nn_itend','nn_it000')
    total_time = (it_end - it_0 + 1) * step

    # tunnel registration (lazy) compulsory to update namelist
    eophis.register_tunnels( tunnel_config )
    
    # write updated namelist
    eophis.write_coupling_namelist( simulation_time=total_time )


def production():
    eophis.info('========= MORAYS : Production =========')
    eophis.info('  Aim: execute coupled simulation\n')

    #  Ocean Coupling
    # ++++++++++++++++
    tunnel_config, nemo_nml = ocean_info()
    step, it_end, it_0 = nemo_nml.get('rn_Dt','nn_itend','nn_it000')
    niter = it_end - it_0 + 1
    total_time = niter * step

    # tunnel registration (lazy)
    nemo, = eophis.register_tunnels( tunnel_config )

    # link all tunnels (beware, dormant errors will likely appear here)
    eophis.open_tunnels()

    #  Models
    # ++++++++
    from models import add_100, Std_Stanley, GTF_LinReg_Stanley, GTF_FCNN_Stanley, GTF_CNN_Stanley

    #  Assemble
    # ++++++++++
    @eophis.all_in_all_out(earth_system=nemo, step=step, niter=niter)
    def loop_core(**inputs):
        outputs = {}

        outputs['sst_var'] = add_100(inputs['sst'])
        outputs['svt_var'] = add_100(inputs['svt'])
        #outputs['rho'] = GTF_LinReg_Grooms(inputs['sst'])
        
        return outputs

    #  Run
    # +++++
    eophis.starter(loop_core)
    
# ============================ #
if __name__=='__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--exec', dest='exec', type=str, default='prod', help='Execution type: preprod or prod')
    args = parser.parse_args()

    if args.exec == 'preprod':
        preproduction()
    elif args.exec == 'prod':
        production()
    else:
        eophis.abort(f'Unknown execution mode {args.exec}, use "preprod" or "prod"')
