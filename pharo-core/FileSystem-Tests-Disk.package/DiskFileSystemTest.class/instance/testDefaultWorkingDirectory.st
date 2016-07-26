tests
testDefaultWorkingDirectory
	| ref |
	ref := filesystem workingDirectory.
	self assert: (('File @ ', Smalltalk vm imagePath) beginsWith: ref asString)