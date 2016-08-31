accessing
iconForTestResultOf: aMethod
	aMethod isPassedTest
		ifTrue: [ ^ self iconNamed: #testGreenIcon ].
	aMethod isFailedTest
		ifTrue: [ ^ self iconNamed: #testYellowIcon ].
	aMethod isErrorTest
		ifTrue: [ ^ self iconNamed: #testRedIcon ].
	^ self iconNamed: #testNotRunIcon.