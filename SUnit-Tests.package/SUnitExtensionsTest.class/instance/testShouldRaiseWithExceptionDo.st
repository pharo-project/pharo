test
testShouldRaiseWithExceptionDo

	| testCase testResult  |
	
	testCase := self class selector: #shouldRaiseWithExceptionDoTest.
	testResult := testCase run.
	
	self assert: (testResult passed includes: testCase).
	self assert: testResult passed size=1.
	self assert: testResult failures isEmpty.
	self assert: testResult errors isEmpty.
	
	