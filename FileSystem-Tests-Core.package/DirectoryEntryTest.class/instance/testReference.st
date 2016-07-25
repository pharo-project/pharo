tests
testReference
	| ref entry |
	ref := FileLocator image resolve.
	entry := ref entry.
	self assert: entry reference = ref