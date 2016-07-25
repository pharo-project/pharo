signaling
signalWithName: classOrTraitName
	^ self signal: classOrTraitName printString, ' can not be serialized as global because it is obsolete.'