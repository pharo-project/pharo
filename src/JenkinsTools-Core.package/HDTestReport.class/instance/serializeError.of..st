running
serializeError: error of: aTestCase
	"We got an error from a test, let's serialize it so we can properly debug it later on..."
	
	self class environment at: #FuelOutStackDebugAction ifPresent: [ :fuelOutStackDebugAction |
		| context testCaseMethodContext |
		context := error signalerContext.
		testCaseMethodContext := context findContextSuchThat: [ :ctx| 
			ctx receiver == aTestCase and: [ ctx methodSelector == #performTest ]].
		context := context copyTo: testCaseMethodContext.
		
		[ fuelOutStackDebugAction
			serializeTestFailureContext: context sender 		
			toFileNamed: aTestCase class name asString,'-', aTestCase selector, '.fuel'	] 
		on: Error 
		do: [:err| "simply continue..." ]
	].

