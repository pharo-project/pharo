building suites
addToSuite: suite fromMethods: testMethods 
	testMethods do:  [ :selector | 
			suite addTest: (self selector: selector) ].
	^suite