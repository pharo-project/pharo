history
selectResultsForTestCase: aTestCaseClass
	| passedSelectors errorsSelectors failuresSelectors |
	passedSelectors := self passed
						select: [:testCase | testCase class == aTestCaseClass ] thenCollect: [:testCase | testCase selector].
	errorsSelectors := self errors 
						select: [:testCase | testCase class == aTestCaseClass ] thenCollect:  [:testCase | testCase selector].
	failuresSelectors := self failures 
						select: [:testCase | testCase class == aTestCaseClass ] thenCollect:  [:testCase | testCase selector].
	
	^ self class newTestDictionary 
		at: #passed put: passedSelectors asSet;
		at: #failures put: failuresSelectors asSet;
		at: #errors put: errorsSelectors asSet;
		yourself
		