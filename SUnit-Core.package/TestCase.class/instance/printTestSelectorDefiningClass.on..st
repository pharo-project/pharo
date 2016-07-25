printing
printTestSelectorDefiningClass: aClass on: aStream
	aStream << '('.
	aClass printOn: aStream.
	aStream << ')'