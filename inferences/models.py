"""
Contains User Inference/Analytic Models.

A model must fit the following requisites and structure :
--------------------------------------------------------
    1. must be a callable function that takes N numpy arrays as inputs
    2. /!\ returns N None for the N awaited outputs if at least one of the input is None /!\
    3. inputs may be freely formatted and transformed into what you want BUT...
    4. ...outputs must be formatted as numpy array for sending back
"""
import numpy as np
import gsw
import xarray as xr
import pytorch_lightning as pl
import torch

# --------- utils ---------- #
def Is_None(*inputs):
    """ Test presence of at least one None in inputs """
    return any(item is None for item in inputs)


# ============================ #
#          Add Hundred         #
# ============================ #
def add_100(field):
    """ Trivially add 100 to field (numpy.ndarray) """
    if Is_None(field):
        return None
    else:
        return np.add(field,100)

# ============================ #
#     Stanley et al. 2020      #
# ============================ #
def stanley_terms(T,S):
    """ Compute rho(T,S) + 0.5 * d2 rho(T,S) / dTdT """
    rho = gsw.density.sigma0(S,T)
    tmp = gsw.rho_second_derivatives(S,T,np.zeros_like(T))
    return rho + tmp[2] / 2.0


# sigmaT = c * |lx_i · dT/dx_i|**2
def Std_Stanley(T, S, c=1.0):
    """ Computation of Stanley et al. (2020) model with analytical methods """
    if Is_None(T,S):
        return None
    else:
        dTdx, dTdy = np.gradient(T, edge_order=2)
        sigmaT = dTdx**2 + dTdy**2
        return stanley_terms(T,S) * c * sigmaT


# sigmaT from GeoTrainFlow LinReg
def GTF_LinReg_Stanley(T,S):
    """ Computation of Stanley et al. (2020) model with LinReg inferences """
    if Is_None(T,S):
        return None
    else:
        sigmaT = 0.0
        return stanley_terms(T,S) * sigmaT


# sigmaT from GeoTrainFlow FCNN
def GTF_FCNN_Stanley(T,S):
    """ Computation of Stanley et al. (2020) model with FCNN inferences """
    if Is_None(T,S):
        return None
    else:
        sigmaT = 0.0
        return stanley_terms(T,S) * sigmaT


# sigmaT from GeoTrainFlow CNN
def GTF_CNN_Stanley(T,S):
    """ Computation of Stanley et al. (2020) model with CNN inferences """
    if Is_None(T,S):
        return None
    else:
        sigmaT = 0.0
        return stanley_terms(T,S) * sigmaT



# ++++++++++++++++++++++++++++ #
#           NN Models          #
# ++++++++++++++++++++++++++++ #

# ...
        # format input
        #Tds = xr.Dataset('diff_temp_sqr',(['x','y'],T)})
        #Tds['masks'] = (['x','y'],np.ones_like(T))

        # create a LinReg model
        #model = lin_regr_model('2D', 1, 1)
        
        # set up lightning module with linReg, I/O and torch optiparamsnso
        #pylight_module = TrainingModule( model, ['diff_temp_sqr'], ['temp_var'], None, torch.nn.functional.huber_loss, \
                                         torch.optim.SGD, learning_rate=1e-3, input_normalization_features=None, loss_normalization=False)
        #pylight_module.load_from_checkpoint(checkpoint='linreg_weights.ckpt', model=lrmodel)

        # data manager
        #data_formatter = DataModule( None, [Txr], '2D', ['diff_temp_sqr'], [], height=T.shape[1], width=T.shape[0], batch_size=16, load_data=True)
        #data_formatter.prepare_data()
        #data_formatter.setup(stage='test')

        # gather everything in trainer
        #trainer = pl.Trainer(accelerator='cpu',devices=None)
        
        # predict
        #preds = trainer.predict(pylight_module,dataloaders=data_formatter.test_dataloader())

    
        #sigmaT = ? unformatter (preds['temp_var']) ?
# ...
