Advanced CROCO-Eophis coupling
==============================


.. toctree::
   :maxdepth: 2

Previous sections gave complete minimal guidelines to reproduce or create an hybrid ocean modeling experiment with CROCO. In this section, we present additional tools to make hybrid modeler's life easier.

.. warning:: WORK IN PROGRESS



Configure CROCO with Eophis
---------------------------

.. warning :: Work in progress




Hybrid CPU/GPU execution
------------------------

Running a neural network on pure CPUs may take time. Despite it is not impossible to obtain acceptable results with enough ressources, it could be worth to execute the coupled configuration on hybrid hardwares. For example, running NEMO on CPUs and the coupled ML model on GPUs.

**A quick layman explanation for those who are not familiar with GPUs:**

    *A GPU is always the subordinate of a master CPU that transfers instructions and variables. A GPU program is a portion of code executed by a CPU that is designated to be transferred to the subordinate GPU (it may be the entire code). Note that the language of the code identified as GPU instructions depends on the GPU brand. For instance, CUDA allows the transfer of instructions to NVIDIA GPUs only. Parts of the code that are written in "classic" languages (Fortran, C, Python...) will be executed normally by the master CPU.*



This means that CROCO and the Python scripts will be executed on CPUs, which is desirable for the OASIS communications. In the Eophis script, exchanged fields are numpy arrays and are not GPU-compatible.

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


.. important :: Let's say that you need a large number of CPUs to run CROCO, a few for the Python scripts, and one GPU for the ML model. Depending on your HPC center, you might allocate the GPUs associated with the CPUs used for CROCO and potentially reserve more GPUs than you really need. This situation is likely to occur often with realistic configurations.

    We strongly recommend you to contact the technical assistance of your HPC center to find the solution that would best fit your needs or to pull back to a pure CPU/CPU execution.
