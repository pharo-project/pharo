history
dispatchResultsIntoHistory 

	self classesTested do: 
		[ :testClass | 
		self class
			historyAt: testClass
			put: (self selectResultsForTestCase: testClass) ].
