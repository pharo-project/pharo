test
testNoExceptionInShouldRaiseWithExceptionDo

	| testCase testResult  |
	
	testCase := self class selector: #noExceptionInShouldRaiseWithExceptionDoTest.
	testResult := testCase run.
	
	self assert: (testResult failures includes: testCase).
	self assert: testResult failures size=1.
	self assert: testResult passed isEmpty.
	self assert: testResult errors isEmpty.
	
	