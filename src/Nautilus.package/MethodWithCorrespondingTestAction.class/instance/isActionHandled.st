order
isActionHandled

	^ method methodClass isTestCase not and: [ 
		(testMethod := method correspondingTestMethod) notNil 
	]