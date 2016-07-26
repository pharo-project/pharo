tests
testIsDirectory
	| ref entry |
	ref := FileLocator imageDirectory resolve.
	entry := ref entry.
	self assert: entry isDirectory