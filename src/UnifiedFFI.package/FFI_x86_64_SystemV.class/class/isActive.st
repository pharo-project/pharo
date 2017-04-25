testing
isActive
	^ Smalltalk vm wordSize = 8 and: [ OSPlatform current isWindows not ]