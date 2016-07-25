menus behavior
runTestForAMethod: aMethod notifying: aBoolean
	| testMethod testClass testResult color |
	testMethod := aMethod correspondingTestMethod.
	testMethod ifNil: [ ^ self ].
	testClass := testMethod methodClass.
	testResult := testClass debug: testMethod selector.
	aBoolean
		ifFalse: [ ^ self ].
	color := Color gray.
	testMethod hasPassedTest
		ifTrue: [ color := Color green ].
	testMethod hasFailedTest
		ifTrue: [ color := Color yellow ].
	testMethod hasErrorTest
		ifTrue: [ color := Color red ].
	self notifyTitle: 'Test Finished' contents: 'Method: ' , testMethod methodClass asString , '>>#' , testMethod selector asString color: color