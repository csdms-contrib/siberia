# SIBERIA

SIBERIA simulates the evolution of landscapes under the action of runoff and erosion over long time scales.

## Build/Install

This example can be built on Linux, macOS, and Windows.

**Prerequisites:**

* A Fortran compiler
* CMake

At CSDMS we recommend setting up a [conda environment](https://conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html)
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
This places the executable `siberia` in `<install-prefix>/bin` in your path.

### Windows

An additional prerequisite is needed for Windows:

* Microsoft Visual Studio 2019 or Microsoft Build Tools for Visual Studio 2019

To configure this example from source with CMake
run the following in a [Developer Command Prompt](https://docs.microsoft.com/en-us/dotnet/framework/tools/developer-command-prompt-for-vs)
```
mkdir _build && cd _build
cmake .. -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=<install-prefix>
```
where `<install-prefix>` is the base directory
where SIBERIA is installed (`"C:\Program Files (x86)"` is the default).
When using a conda environment,
use the `%CONDA_PREFIX%` environment variable for `<install-prefix>`.


Then, to build and install:
```
cmake --build . --target install --config Release
```
This places the executable `siberia` in `<install-prefix>/bin` in your path.

## Test

After installing SIBERIA,
tests can be run with:
```
ctest
```

## Run

Run SIBERIA with the `siberia` command.
Optionally, configure SIBERIA with the files

* `siberia.setup`
* `siberia.ctrl.txt`
* `layer.model.txt`
* `default-directory.txt`

SIBERIA is an interactive program.
It will prompt you for a series of parameter inputs.

To run SIBERIA noninteractively,
set up a parameter file and use shell redirection.
For example,
to run SIBERIA with its default parameters,
use the file `default.cfg` in the `tests` directory:
```
siberia < tests/default.cfg
```
To use SIBERIA with example data files
provided in the [data/](./data/) directory, try:
```
siberia < tests/example.cfg
```

Output from SIBERIA is written to the file `siberia-NNNN.output`,
where `NNNN` is an incremented four-digit index starting at 0001.
