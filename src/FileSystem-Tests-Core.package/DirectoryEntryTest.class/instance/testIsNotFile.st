tests
testIsNotFile
	| ref |
	ref := FileLocator imageDirectory resolve.
	self deny: ref entry isFile