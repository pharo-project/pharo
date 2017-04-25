navigating
resolveString: aString
	"Returns the root of the receiver filesystem, i.e. / on unix"
	
	^ self workingDirectoryPath resolvePath: (store pathFromString: aString)