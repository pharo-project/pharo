menus behavior
runTestForAMethod: aMethod notifying: aBoolean
	| testMethod testClass testResult color |
	(testMethod := aMethod asTestMethod) ifNil: [ ^ self ].
	testClass := testMethod methodClass.
	testResult := testClass debug: testMethod selector.
	aBoolean
		ifFalse: [ ^ self ].
	color := Color gray.
	testMethod isPassedTest
		ifTrue: [ color := Color green ].
	testMethod isFailedTest
		ifTrue: [ color := Color yellow ].
	testMethod isErrorTest
		ifTrue: [ color := Color red ].
	self
		notifyTitle: 'Test Finished'
		contents: 'Method: ' , testMethod methodClass asString , '>>#' , testMethod selector asString
		color: color