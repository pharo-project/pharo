printing
printOn: aStream

	super printOn: aStream.
	aStream << '[ '.
	self content trackedClass printOn: aStream.
	aStream << ' ]'.