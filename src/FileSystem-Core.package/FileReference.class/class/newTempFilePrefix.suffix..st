instance creation
newTempFilePrefix: prefix suffix: suffix
	| tmpDir random fileName |
	
	tmpDir := FileLocator temp asFileReference.
	[ random := UUIDGenerator default generateRandomBitsOfLength: 100.
	fileName := prefix , random asString, suffix ] doWhileTrue: [ (tmpDir / fileName) exists ].
	^ tmpDir / fileName