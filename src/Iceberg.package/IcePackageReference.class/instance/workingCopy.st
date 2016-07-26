querying
workingCopy
	^ self isLoaded ifTrue: [
		MCWorkingCopy forPackage: (MCPackage named: self packageName)]