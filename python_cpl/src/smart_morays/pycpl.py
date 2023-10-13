# oasis modules
import pyoasis
from pyoasis import OASIS
from mpi4py import MPI
# utils
import numpy as np
import logging
import os
import shutil

logger = logging.getLogger(__name__)

class Tunnel:

    def __init__(self,label="morays"):
        self.comp = pyoasis.Component(label,True,MPI.COMM_WORLD)
        self.comm_rank = self.comp.localcomm.rank
        self.comm_size = self.comp.localcomm.size
        
        self.partitions = {}
        self.variables = { "rcv" : {} , "snd" : {} }

    def __del__(self):
        del self.comp
  
    def define_partitions(self,grids):
        self.grids = grids
        
        """ /!\ TODO: découpage optimal des grilles /!\ """
        # Create Partitions
        for grd_lbl,(nlon,nlat) in grids.items():
            local_size = int( nlon * nlat / self.comm_size )
            offset = self.comm_rank * self.local_size
        
            if self.comm_rank == self.comm_size - 1:
                local_size = nlon * nlat - offset
                
            self.partitions.update({ grd_lbl : pyoasis.ApplePartition(offset,local_size,name=grd_lbl) })

    def add_variables_for_exchanges(self,vars):
        self.vars = vars
        for i, var in enumerate(vars):
            self.exchanges["rcv"].update({ var['in'] : pyoasis.Var( "M_IN_"+str(i+1+len(exchanges["rcv"])), self.partitions[var['grd']], OASIS.IN, bundle_size=var['lvl']) })
            self.exchanges["snd"].update({ var['out'] : pyoasis.Var( "M_OUT_"+str(i+1+len(exchanges["snd"])), self.partitions[var['grd']], OASIS.OUT, bundle_size=var['lvl']) })
    
    def send(self,var_label,date,values):
        var = self.exchanges["snd"][var_label]
        snd_fld = pyoasis.asarray(values)
        
        if snd_fld.shape != 3 :
            logging.error('  Shape of sending array for var %s must be 3' % (var_label))
            quit()
        
        if (snd_fld.shape[0]*snd_fld.shape[1], snd_fld.shape[2]) != (var._partition_local_size, var.bundle_size):
            logging.error('  Size of sending array for var %s does not match associated partition' % (var_label))
            quit()
            
        var.put(date,snd_fld)

    def receive(self,var_label,date):
        var = self.exchanges["rcv"][var_label]
        rcv_fld = pyoasis.asarray( np.zeros((var._partition_local_size,var.bundle_size)) )
        var.get(date,rcv_fld)
        return rcv_fld


def open_tunnel(label,grids,vars):

    tunnel = Tunnel(label)

    if tunnel.comm_rank == 0:
        logging.info('  ----------------------- TUNNEL CREATED --------------------------')
        logging.info('  Component %s initialized with ID: %.1i' % (tunnel.comp._name,tunnel.comp._id))
        logging.info('  Running with %.1i processes' % tunnel.comm_size)

    tunnel.define_partitions(grids)
    if tunnel.comm_rank == 0:
        logging.info('  Grids registered')

    tunnel.add_variables_for_exchanges(vars)
    if tunnel.comm_rank == 0:
        logging.info('  Variables registered')

    if tunnel.comm_rank == 0:
        write_oasis_namelist(tunnel)
        logging.info('  -----------------------------------------------------------')

    
    tunnel.comp.enddef()

    return tunnel
    
    
def write_oasis_namelist(tunnel):
            
    namcouple = os.path.join(os.getcwd(),"namcouple")
    shutil.copy(os.path.join(os.path.abspath(smart_morays.__file__)[:-24],"namcouple_base"),namcouple)

    logging.info('  Writing %s' % namcouple)

    # Write exchanges in namelist
    file = open(namcouple,'a')
    file.write(" $STRINGS\n")
    
    for inout,ex in tunnel.exchanges.items():
        for lbl,var in ex.items():
            if inout == "rcv":
                name_snd = var.name()
                name_rcv = "O_OUT_"+name_snd[-1:]
            if inout == "snd":
                name_rcv = var.name()
                name_snd = "O_IN_"+name_rcv[-1:]
                
            idx = [i for i, dic in enumerate(tunnel.vars) if lbl in dic.values()][0]
            grd_lbl = tunnel.vars[idx]['grd']
            freq = tunnel.vars[idx]['freq']
            nlon, nlat = tunnel.grids[grd_lbl]
            
        bloc = name_snd+" "+name_rcv+" 1 "+str(freq)+" 2 rst.nc EXPOUT\n" \
               +str(nlon)+" "+str(nlat)+" "+str(nlon)+" "+str(nlat)+" "+grd_lbl+" "+grd_lbl+" LAG=0\n" \
               +"P 2 P 2\n#\n"
        file.write(bloc)
        
    file.write(" $END")
    file.close()

    # Replace Header
    nbex = len(exchanges['rcv']) + len( exchanges['snd'])
    replacements = {'NBFLD' : str(nbex) , 'NTIME': str()}
    with open(namcouple) as file:
        file.seek(0,0)
        file.write("############# AUTOMATICALLY WRITTEN BY SMART-MORAYS ###############\n")
        for line in file:
            for src, target in replacements.items():
                line = line.replace(src,target).read()
            file.write(line)
