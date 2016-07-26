error handling
failUnless: aBoolean 
	aBoolean
		ifFalse: [self error]