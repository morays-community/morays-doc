"""
Contains User Inference/Analytic Models.

An model must fit the following requisites and structure :
--------------------------------------------------------
    1. must be a callable function that takes N numpy arrays as inputs
    2. /!\ returns N None for the N awaited outputs if at least one of the input is None /!\
    3. inputs may be freely formatted and transformed into what you want BUT...
    4. ...outputs must be formatted as numpy array for sending back
"""
import numpy as np

#             Utils            #
# ++++++++++++++++++++++++++++ #
def Is_None(*inputs):
    """ Test presence of at least one None in inputs """
    return any(item is None for item in inputs)

def format_out(*outputs):
    """ Transform the values in outputs into numpy arrays """
    return [np.array(out) for out in outputs]


# ============================ #
#          Add Hundred         #
# ============================ #
def add_100(sst):
    """ Trivially add 100 to sst (numpy.ndarray) """
    if Is_None(sst):
        return None
    else:
        return np.add(sst,100)

# ============================ #
#     Stanley et al. 2020      #
# ============================ #
# Equation Of State
def EOS(T_var):
    rho = T_var * 2.0
    return format_out(rho)


# analytic computation
def Std_Stanley(sst):
    if Is_None(sst):
        return None
    else:
        rho = EOS(sst)
        return rho


# LinReg GeoTrainFlow computation
def GTF_LinReg_Stanley(sst):
    if Is_None(sst):
        return None
    else:
        rho = EOS(sst)
        return rho


# FCNN GeoTrainFlow computation
def GTF_FCNN_Stanley(sst):
    if Is_None(sst):
        return None
    else:
        rho = EOS(sst)
        return rho


# CNN GeoTrainFlow computation
def GTF_CNN_Stanley(sst):
    if Is_None(sst):
        return None
    else:
        rho = EOS(sst)
        return rho
