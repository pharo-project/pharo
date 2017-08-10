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
		ifTrue: [ color := self theme successBackgroundColor ].
	testMethod isFailedTest
		ifTrue: [ color := self theme warningBackgroundColor ].
	testMethod isErrorTest
		ifTrue: [ color := self theme dangerBackgroundColor ].
	self
		notifyTitle: 'Test Finished'
		contents: 'Method: ' , testMethod methodClass asString , '>>#' , testMethod selector asString
		color: color