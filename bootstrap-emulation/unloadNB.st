| remove |

"save and restart image here"

SessionManager default unregisterClassNamed: #GTSpotterEventRecorderSettings.
SessionManager default unregisterClassNamed: #FFICallbackThunk.
SessionManager default unregisterClassNamed: #Alien.

(Smalltalk classNamed: #AlienWeakTable) allInstancesDo: [:table |
  WeakArray removeWeakDependent: table ].
WeakArray removeWeakDependent: (Smalltalk classNamed: #Callback).

remove := #('Athens-Cairo' 'NativeBoost-Tests'  'NativeBoost-Examples' 'NativeBoost-Unix' 'NativeBoost-Win32' 
'NativeBoost-Mac'   'NativeBoost-Core' 'NativeBoost-Pools' 'AsmJit-Tests' 'AsmJit-StackManagement' 
'AsmJit-Operands' 'AsmJit-x86'  'AsmJit-Extension' 'AsmJit-Instructions' 'AsmJit-Core' 'Alien' 'FFI-Kernel' 'FFI-MacOS'
'FFI-Pools' 'FFI-Tests' 'FFI-Unix' 'FFI-Win32').

remove do: [ :packageName |
    (MCWorkingCopy forPackage: (MCPackage named: packageName)) unload. ].

Smalltalk recreateSpecialObjectsArray.

Context initializePrimitiveSimulators.

SmalltalkImage current fixObsoleteReferences.
Behavior flushObsoleteSubclasses.
Smalltalk garbageCollect.

Smalltalk snapshot: true andQuit: true.


