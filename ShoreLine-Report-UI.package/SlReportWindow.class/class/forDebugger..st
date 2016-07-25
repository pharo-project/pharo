instance creation
forDebugger: aDebugger

	^ self basicNew initialize
		debugger: aDebugger;
		updateReport;
		yourself