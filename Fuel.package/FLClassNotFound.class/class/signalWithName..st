signaling
signalWithName: className
	^ self signal: 'Class named ', className printString, ' not found.'