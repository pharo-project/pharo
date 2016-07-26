tests
testContainsLocator
	| ref |
	ref := FileLocator imageDirectory resolve parent.
	self assert: (ref contains: FileLocator image)