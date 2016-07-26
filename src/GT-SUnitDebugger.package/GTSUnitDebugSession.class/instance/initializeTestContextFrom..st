initialization
initializeTestContextFrom: aContext

	"we first try to find a test method in the stack"
	testContext := aContext findContextSuchThat: [ :anotherContext |
		anotherContext method isTestMethod ].
	
	testContext ifNil: [
		"if we do not find a test method, perhaps we are in the context of a different method, such as setUp, so we are looking for the last occurence of the test case instance in the stack"
		testContext := aContext findContextSuchThat: [ :anotherContext |
			anotherContext receiver class isTestCase ] ].
	
	testContext ifNil: [ 
		"if we did not find anything, we fallback to the default context"
		testContext := aContext ]