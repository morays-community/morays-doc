# PyOASIS with NEMO

Quick guide to compile NEMO with OASIS_V5.0 and use the Python API.

**WARNING**: This tutorial is done considering NEMO_V4.2.1, which officially does not support OASIS_V5.0.

## Install OASIS_V5.0

OASIS needs to be compiled first because both XIOS and NEMO depend on it.

### Download OASIS

* Get OASIS sources by cloning git repository : ```git clone https://gitlab.com/cerfacs/oasis3-mct.git```

    This command may also be found on [CERFACS website](https://oasis.cerfacs.fr/en/).

* Be sure to be on the right commit (``` git checkout OASIS3-MCT_5.0 ```) and go in compilation directory: ``` cd oasis3-mct/util/make_dir ```.

### Compile Libraries

* OASIS libraries must be dynamically compiled to use C and Python APIs. Create and adapt your own "make.\<YOUR_ARCH\>" file.

* Define the following flags to ensure dynamic compilation :

    ``` DYNOPT = -fPIC ```

    ``` LDDYNOPT = -shared ${NETCDF_LIBRARY} ```

    Do not forget to add an include path to the new `cbindings` module directory.


* Run Makefile:

    ``` make -f TopMakefileOasis3 pyoasis ```


* If everything goes right, you should find the following libraries in `oasis3-mct/lib/`:

      libmct.so         libmpeu.so         liboasis.cbind.so         libpsmile.MPI1.so         libscrip.so

## Required Python Environnement

* Source the following files to initialize the PyOASIS modules. The best is to put those commands in your `.bash_profile`:

    ``` source /PATH/TO/oasis3-mct/BLD/python/init.sh ```

    ``` source /PATH/TO/oasis3-mct/BLD/python/init.csh ```

* Some python packages are required to use PyOASIS. Use your favorite python environnment manager to install and load them:

        mpi4py         numpy         netcdf4

* Those are not compulsory but useful to run the tests:

        matplotlib         scipy         shapely         cartopy         pytest

    **NB:** See PyOASIS documentation for more details.

## Testing PyOASIS

* Go in directory `../oasis3-mct/pyoasis/tests` and execute `run_pytest.sh` or directly type `pytest` command to run PyOASIS unit tests.

* If successfull, go in `../oasis3-mct/pyoasis/` and type `make test` to execute the OASIS Fortran, C and Python operating tests.

## Compile XIOS and NEMO

* Compile XIOS and NEMO with OASIS linking as usual. Do not forget to add the path to the `cbindings` include directory and link the `-loasis.cbind` library during compilation (not compulsory but cleaner).
