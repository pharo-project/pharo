signaling
signalWithName: aName
	^ self signal: 'Global named ', aName printString, ' not found.'