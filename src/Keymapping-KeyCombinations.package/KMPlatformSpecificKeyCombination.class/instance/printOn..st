printing
printOn: aStream
	super printOn: aStream.
	aStream << ' ('.
	shortcut printOn: aStream.
	aStream << ' on '.
	aStream << platform asString.
	aStream << ')'