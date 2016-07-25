testing
appliesToDebugger: aDebugger

	^ (self showWithDebugger: aDebugger) and: [ super appliesToDebugger: aDebugger ]