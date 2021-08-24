# Changelog highlights

## New tools
- New help browser using Microdown

## Cleaning
- Removed Glamour
- Removed GTools



The following are Pharo 90 logs. 
We keep them as example and they should be removed. 

## Summary of changes and improvements
- mature 64-bit VM for Windows
- alternative headless VM (preview)
  - a virtual machine without integrated support of GUI
  - the user interface is managed by Pharo code directly which brings more flexibility
  - full backwards compatibility with the old windows management
  - threaded FFI with callbacks support
- better system performance
- many more tests
- the new generation of Spec UI building framework (preview)
- new tests management tool called DrTests (preview)
- new code completion engine

## Infrastructure changes

- system performance testing on the CI Infrastructure changes
- all VM sources managed by Git Infrastructure changes
- intensive VM testing Infrastructure changes
- bootstrap process documentation improved

## Git support (Iceberg)

- enhanced projects and repositories management.
- improved merging
- more exposed settings
- faster loading and comparison for projects with big packages.
- closer integration with Calypso
- fixes in Tonel format support

## Code management

- new code completion engine
- Calypso (system browser)
  - suggestions for class definitions
  - more refactorings
- safer deprecation
- better pool dictionaries support in Dependency analyzer
- cursors, shared pools and variables inspectors
- Enlumineur (preview) - new code formatter
- better syntax highlighter performance
- branch coloring support in Hiedra

## Reflectivity

- more slots examples (HistorySlot…)
- better slots integration
- Reflectivity-Tools refactoring

## UI building framework (Spec)

- introduction of Spec 2
- better support of multiple backends (Morphic, Gtk…)
- simplified implementation
- better layouts
- easier testing
- applications management

## Look & Feel

- the dark theme as default
- fonts corruption fixes
- more settings for production images
- copy windows title command

## FFI and VM Interface

- UFFI
  - better support of literals
  - more types implemented
  - supports of ThreadedFFI backend
- OSWindow
  - improved events and windows management
  - world rendering in different backends
  - more capable backends setting
- File Attributes Plugin
  - new file systems properties interface

## New projects

- DrTests (preview) - tests management tool with plugins support
- Clap - Command line argument parser
- Beacon - logging engine
- SUnit-Visitor - standardized way to visit test suites
- Spec 2 (preview) - UI building framework with multiple backends
- Commander 2 (preview) - advanced command pattern implementation

## Performance

- faster Collections implementation
- speed-up of system queries and sources access
- faster Spotter searches
- better class building performance
- SSD friendly read-write streams
- faster Athens, examples improvements		
		
You can see the Pharo 9.0 changelog at: 

[https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo90ChangeLogs.md](https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo90ChangeLogs.md)