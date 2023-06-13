# SIBERIA

SIBERIA simulates the evolution of landscapes under the action of runoff and erosion over long time scales.

## Build/Install

This example can be built on Linux, macOS, and Windows.

**Prerequisites:**

* A Fortran compiler
* CMake

### Linux and macOS

To build this example from source with CMake, run:
```
mkdir _build && cd _build
cmake .. -DCMAKE_BUILD_TYPE=Release
make
```
Then, to install:
```
make install
```
This places the executable `siberia` in the root directory of the project.

### Windows

An additional prerequisite is needed for Windows:

* Microsoft Visual Studio 2017 or Microsoft Build Tools for Visual Studio 2017

To configure this example from source with CMake
run the following in a [Developer Command Prompt](https://docs.microsoft.com/en-us/dotnet/framework/tools/developer-command-prompt-for-vs)
```
mkdir _build && cd _build
cmake .. -G "NMake Makefiles" -DCMAKE_BUILD_TYPE=Release
```
Then, to build and install:
```
cmake --build . --target install --config Release
```
This places the executable `siberia` in the root directory of the project.

## Run

Run SIBERIA with the `siberia` executable.
Optionally, configure SIBERIA with the files

* `siberia.setup`
* `siberia.ctrl.txt`
* `layer.model.txt`
* `default-directory.txt`

as well as with the input data files in the [data/](./data/) directory.

Output from SIBERIA is written to the file `siberia-NNNN.output`,
where `NNNN` is an incremented four-digit index starting at 1.
