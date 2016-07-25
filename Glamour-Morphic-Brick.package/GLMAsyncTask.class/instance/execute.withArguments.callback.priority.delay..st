async
execute: aBlock withArguments: aCollection callback: callbackBlock priority: aPriority delay: anIntegerMilliseconds
	
	self terminate.
	
	callback := callbackBlock.
	process := [ |result|
		anIntegerMilliseconds > 0 ifTrue: [ anIntegerMilliseconds milliSeconds wait ].
		result := aBlock valueWithEnoughArguments: aCollection.
		"we use it to pass result from background thread and execute callback block in UI thread"
		UIManager default defer: [ self complete: result callback: callbackBlock ]
	]	newProcess.
	
	process priority: aPriority.
	process resume.
		
	^ self