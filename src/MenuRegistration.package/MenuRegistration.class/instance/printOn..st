printing
printOn: aStream

	super printOn: aStream.
	aStream << ' ( ' << self name asString << ' ) '