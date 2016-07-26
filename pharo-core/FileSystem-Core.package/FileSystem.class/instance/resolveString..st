navigating
resolveString: aString
	"Returns the root of the receiver filesystem, i.e. / on unix"
	
	^ workingDirectory resolvePath: (store pathFromString: aString)