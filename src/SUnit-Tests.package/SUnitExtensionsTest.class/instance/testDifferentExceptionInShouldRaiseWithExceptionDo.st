test
testDifferentExceptionInShouldRaiseWithExceptionDo

	| testCase testResult  |
	
	testCase := self class selector: #differentExceptionInShouldRaiseWithExceptionDoTest.
	testResult := testCase run.
	
	self assert: (testResult passed includes: testCase).
	self assert: testResult errors isEmpty.
	self assert: testResult failures isEmpty.
	self assert: testResult passed size=1