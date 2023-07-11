# SIBERIA

SIBERIA simulates the evolution of landscapes under the action of runoff and erosion over long time scales.

## Build/Install

This example can be built on Linux, macOS, and Windows.

**Prerequisites:**

* A Fortran compiler
* CMake

Optionally,
at CSDMS we recommend setting up a [conda environment](https://conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html)
into which these prerequisites, and SIBERIA, can be installed.
After [installing conda](https://conda.io/projects/conda/en/latest/index.html) on your system,
create a conda environment from the file [environment.yml](./environment.yml) with:
```
conda env create --file environment.yml
```
Then activate the environment with:
```
conda activate siberia
```
(Note on Linux and macOS, you may have to use `source` instead of `conda` to activate the environment.)

### Linux and macOS

To build this example from source with CMake, run:
```
mkdir _build && cd _build
cmake .. -DCMAKE_INSTALL_PREFIX=<install-prefix>
make
```
where `<install-prefix>` is the base directory
where SIBERIA is installed (`/usr/local` is the default).
When using a conda environment,
use the `$CONDA_PREFIX` environment variable for `<install-prefix>`.

Then, to install:
```
make install
```
This places the executable `siberia` in `<install-prefix>/bin`.

### Windows

An additional prerequisite is needed for Windows:

* Microsoft Visual Studio 2017 or Microsoft Build Tools for Visual Studio 2017

To configure this example from source with CMake
run the following in a [Developer Command Prompt](https://docs.microsoft.com/en-us/dotnet/framework/tools/developer-command-prompt-for-vs)
```
mkdir _build && cd _build
cmake .. -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=<install-prefix>
```
where `<install-prefix>` is the base directory
where SIBERIA is installed (`"C:\Program Files (x86)"` is the default).
When using a conda environment,
use the `$CONDA_PREFIX` environment variable for `<install-prefix>`.


Then, to build and install:
```
cmake --build . --target install --config Release
```
This places the executable `siberia` in `<install-prefix>/bin`.

## Run

Run SIBERIA with the `siberia` executable.
Optionally, configure SIBERIA with the files

* `siberia.setup`
* `siberia.ctrl.txt`
* `layer.model.txt`
* `default-directory.txt`

as well as with the input data files in the [data/](./data/) directory.

Output from SIBERIA is written to the file `siberia-NNNN.output`,
where `NNNN` is an incremented four-digit index starting at 0001.
