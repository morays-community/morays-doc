Advanced NEMO-Eophis coupling
=============================


.. toctree::
   :maxdepth: 2

Previous sections gave minimal guidelines to reproduce or create a Morays experiment with NEMO. In this section, we present additional tools to make Morays modeler's life easier.



pyDCM
-----

NEMO experiments (from Morays or not) often need to perform long simulation times, which requires to run the simulation on several segments. Each segment starts on the results of the previous one. Hopefully, they are many of useful tools to automate the segments succession and save the intermediate results and restarts. One of this tool is named Drakkar Config Manager (DCM).

However, DCM has not been designed to manage hybrid modeling environment with Python and Eophis material. `pyDCM <https://github.com/alexis-barge/pyDCM/tree/4.2.1_pyoasis?tab=readme-ov-file>`_ is an updated version of the DCM in which those missing features have been added. We invite you to read the pyDCM documentation for more details on how to use it.

Some of the Morays use cases have been conducted with pyDCM (``NEMO-eORCA025_Subgrid_Density`` for instance).

Additional material required by pyDCM are stored in experiments repositories in accordance with Morays template.




Configure NEMO with Eophis
--------------------------

.. warning :: Work in progress




Hybrid CPU/GPU execution
------------------------

Running a Python neural network on pure CPUs may take time. Despite it is not impossible to obtain acceptable results with enough ressources, it could be worth to execute the coupled configuration on hybrid hardwares. For example, running NEMO on CPUs and the coupled ML model on GPUs.

**A quick layman explanation for those who are not familiar with GPUs:**

    *A GPU is always the subordinate of a master CPU that transfers instructions and variables. A GPU program is a portion of code executed by a CPU that is designated to be transferred to the subordinate GPU (it may be the entire code). Note that the language of the code identified as GPU instructions depends on the GPU brand. For instance, CUDA allows the transfer of instructions to NVIDIA GPUs only. Parts of the code that are written in "classic" languages (Fortran, C, Python...) will be executed normally by the master CPU.*



This means that NEMO and the Python scripts will be executed on CPUs, which is desirable for the OASIS communications. In the Eophis script, exchanged fields are numpy arrays and are not GPU-compatible.

Thus, a typical pipeline inside the coupled model for hybrid execution would be:
    - convert received numpy arrays fields into a GPU format
    - load and configure the neural network to do the predictions on GPU
    - convert back the results into a CPU format for sending back as numpy arrays


Some libraries, as PyTorch, may be executed on both CPU and GPU with a simple high-level switch command. Here are some examples from **DINO.GZ21** test case that represents the abovementionned pipeline:

 .. code-block :: python
    
    import torch
    
    # use GPUs if available
    if torch.cuda.is_available():
        print("CUDA Available")
        device = torch.device('cuda')
    else:
        print('CUDA Not Available')
        device = torch.device('cpu')

    # Load model on CPU or GPU
    net = FullyCNN()
    model_weights = torch.load( wights_path , map_location=device )
    net.load_state_dict( model_weights )
    net.to(device)

    # convert numpy arrays into torch tensors on CPU or GPU
    tensor_fld = torch.tensor( rcv_fld ).to(device)
    
    # prediction, bring back results on CPU
    preds = net( tensor_fld )
    if device.type == 'cuda':
        preds = preds.cpu()
        
    # convert back into numpy array
    preds = preds.numpy()


You are of course free to use other libraries to transfer your model to a GPU.


.. important :: Let's say that you need a large number of CPUs to run NEMO, a few for the Python scripts, and one GPU for the ML model. Depending on your HPC center, you might allocate the GPUs associated with the CPUs used for NEMO and potentially reserve more GPUs than you really need. This situation is likely to occur often with realistic configurations.

    We strongly recommend you to contact the technical assistance of your HPC center to find the solution that would best fit your needs or to pull back to a pure CPU/CPU execution.
