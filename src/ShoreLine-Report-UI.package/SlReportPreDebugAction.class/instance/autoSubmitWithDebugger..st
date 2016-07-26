testing
autoSubmitWithDebugger: aDebugger

	SlReporter autoSubmit ifTrue: [ self openWithDebugger: aDebugger ].
	^ SlReporter autoSubmit