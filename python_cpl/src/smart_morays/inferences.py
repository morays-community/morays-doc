import logging
#import pytorch_lightning as lt

logger = logging.getLogger(__name__)

class TrainedModel:

    def __init__(self):
        pass
    
    def __del__(self):
        pass

    def load_data(self):
        pass

    def load_weight(self):
        pass


class Pytorch_Model(TrainedModel):

    def __init__(self):
        pass
        
    def __del__(self):
        pass
        
        
class LTPytorch_Model(TrainedModel):

    def __init__(self, label, DataManager, LightningModule):
        self.DM = DataManager
        self.LTM = LTModule
        
    def __del__(self):
        pass


class TensorFlow_Model(TrainedModel):

    def __init__(self):
        logging.error('  Tensor Flow models not supported yet')
        quit()
        
    def __del__(self):
        pass


class UDF_Model(TrainedModel):

    def __init__():
        pass
        
    def __del__():
        pass
