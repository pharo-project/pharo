test
testInvalidShouldNotTakeMoreThan

	| testCase testResult |

	testCase := self class selector: #invalidShouldNotTakeMoreThan.
	testResult := testCase run.

	self assert: testResult passed isEmpty.
	self assert: testResult failures size = 1.
	self assert: (testResult failures includes: testCase).
	self assert: testResult errors isEmpty

