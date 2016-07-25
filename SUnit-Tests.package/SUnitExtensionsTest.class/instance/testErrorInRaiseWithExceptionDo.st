test
testErrorInRaiseWithExceptionDo

	| testCase testResult  |
	
	testCase := self class selector: #errorInRaiseWithExceptionDoTest.
	testResult := testCase run.
		
	self assert: (testResult errors includes: testCase).
	self assert: testResult errors size=1.
	self assert: testResult failures isEmpty.
	self assert: testResult passed isEmpty.
	
	