initialization
initializeAssestionContextFrom: aContext
	| frameworkTestClasses |
	
	frameworkTestClasses := {TestCase. TestCase superclass}.
	
	assertionContext := aContext findContextSuchThat: [ :anotherContext |
		anotherContext = testContext 
			ifTrue: [ 
				"the assestion context should be between the top context and before the test context"
				^ self  ].
		anotherContext sender notNil and: [ 
			 (frameworkTestClasses anySatisfy: [ :aTestClass |
				 aTestClass includesSelector: anotherContext sender method selector]) not ] ].
	assertionContext 
		ifNotNil: [ assertionCallerContext := assertionContext sender]
	
		