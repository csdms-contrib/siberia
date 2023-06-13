       KEY POINTS IN THE REVISION HISTORY OF SIBERIA V8
     ====================================================

V8.01
-----

- Introduction of soils model. Simple exponential insitu soil production 
(ala Heimsmath) with a spatial soil wetness feedback on soil production 
rate soils model. Preliminary testing.

V8.10
-----

- Last version fully compatible with Fortran77. Future versions to incorporate
Fortran 95 features.

V8.23
-----

- Release version of model with soils module allowing output of various aspects 
of the soil model for Patricia Saco project. 
- Some introduction of dynamic memory

- FINALISED (9/03)

V8.24
-----

- Bug fixes on soils module. 
- Further introduction of dynamic memory.
- Existing LAYERS module removed and work commenced on a new more general LAYERS
and sediment tracking module.

- FINALISED (23/11/03)

V8.25
-----

- First reliable release with full dynamic memory. In Windows 2000/XP  
the limitation on the maximum grid size of 1200x1200 is removed. Performance 
is limited by installed memory with speed dropping dramatically once installed 
memory is exceeded (2 Gigabytes on Windows approx equivalent to 1200x1200).
- No longer runs in parallel under openMP V1.0 (due to dynamic memory features
which require use of MODULES for global data rather than COMMON).
Parallel version under openMP V2.0 planned for future version.
- Work continuing on the new LAYERS module.
- bug fix for Dinfinity

- RELEASE VERSION: dynamic memory (8/4/04)

V8.26
-----

- work continuing on the LAYERS module and sediment tracking
- improved error checking on RST2 file input
- modeMC=1 completely removed ... request for ModeMC=1 sets ModeMC=0
- adding binary file output as an alternative to .RSU file ... for
  use by IDL and other visualisation suites.
- preliminary testing of Dinfinity fix ... still doesn't work reliably

- FINALISED (25/10/04): Sent to LANL

V8.27
-----

- work continuing on the LAYERS module and sediment tracking.
- files for user defined models (e.g. test.erode) are now called model files
(e.g. test.model) and are now completely portable between the various user 
defined models (e.g. erosion, runoff, tectonics).
- improved modelling of detachment-limitation in ModeErode=3. 
ModeSolver=8 triggers the new capabilities. For ModeErode=0 (or where regions
all have the same erodibility) the results will be the same (+/- round off error)
as for ModeSolver=4. Performance penalty of ModeSolver=8 relative to ModeSolver=4
about 20%.

- FINALISED (25/11/04): Sent to LANL

V8.28
-----

- First complete version of the LAYERS and sediment tracking module. Some 
functionality still to be implemented but the computational engine is largely complete. 
- Relative to a nonlayers run the extra cost of tracking layers appears to be 
an increase of about 25% in computation time if transpiorted limited. If
detachment limitation is turned on as well then the computation times are 
increased by a further 25%. Extra memory is required as
well but for normal problems (i.e. 10 layers) this is nominal and doesn't impact 
on CPU time.
- You can now overwrite existing RST2 file if desired (see siberia.setup)
- Corrected a bug in non-dimensional mode where the RSU output for sediment flux
  was incorrect (OK in dimensional mode). 

V8.29
-----

- Changed mixing model in layers from linear to b1-d50.
- First release version of the LAYERS and sediment tracking module. Some 
functionality still to be implemented but the computational engine is largely complete. 

V8.30
----

- Fixed a bug in the layering model that had meant that parts of the layering model were 
calculated even when the layering model was not being used.
- Better bug checking for modes in the model that interact with layering
- Adaptive time stepping not implemented when layering used (a check for this now stops the code)

V8.31
-----

- A bug fix in the layering model that stopped the code running when the domain was set too close to the boundary
- A bug fix so that automated output of file when a negative output time given now works correctly.
- NEW: Basic code for doing tracers to allow U Glasgow to do tracing of cosmogenic nuclides. 

V8.32
-----

- code forked for V8.31, 8.30. V8.32 merged these forks
- Conflict between dynamic timestepping and LAYERS. Error now flagged
  that LAYERS and dynamic timestepping are incompatible.

V8.33
-----

- in LAYERS code incorrectly converted units if only one of ModeErode and ModeRunoff
  were greater than 20. Fixed so that if one is > 20 then both are set > 20.
- error in initialisation of sediment grading in flow tracking fixed
- expanded the length of allowable file names to 1000 characters
