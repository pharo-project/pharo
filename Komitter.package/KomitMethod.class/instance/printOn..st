*Komitter-UI
printOn: aStream

	super printOn: aStream.
	aStream << '[ '.
	self method selector printOn: aStream.
	aStream << ' ]'.