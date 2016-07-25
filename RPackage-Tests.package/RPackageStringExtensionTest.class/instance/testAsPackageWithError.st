tests
testAsPackageWithError
	self 
		should: ['___inexistent___package___' asPackage]
		raise: NotFound
	