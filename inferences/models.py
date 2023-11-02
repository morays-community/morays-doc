"""
Contains User Inference Models.

An inference model must fit the following requisites and structure :
------------------------------------------------------------------
    - must be a callable function that takes N numpy arrays as inputs
    - /!\ return N None for the N awaited outputs if at least one of the input is None /!\
    - input may be freely formatted and transformed into what you want for modeling BUT...
    - ...outputs must be formatted as numpy array for sending back
"""
import numpy as np

#             Utils            #
# ++++++++++++++++++++++++++++ #
def Is_None(*inputs):
    return any(item is None for item in inputs)

def format_out(*outputs)
    return [np.array(out) for out in outputs]


# ============================ #
#          Add Hundred         #
# ============================ #
def add_100(sst):
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
