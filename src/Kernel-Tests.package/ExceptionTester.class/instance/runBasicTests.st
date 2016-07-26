suites
runBasicTests

	self basicTestSelectors
		do:
			[:eachTestSelector |
			self runTest: eachTestSelector]