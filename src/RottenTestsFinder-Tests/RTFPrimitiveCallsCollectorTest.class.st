Class {
	#name : 'RTFPrimitiveCallsCollectorTest',
	#superclass : 'TestCase',
	#category : 'RottenTestsFinder-Tests',
	#package : 'RottenTestsFinder-Tests'
}

{ #category : 'tests' }
RTFPrimitiveCallsCollectorTest >> testFindsAssert [
	
	| collector tree compiledMethod methods |
	collector := RTFPrimitiveCallsCollector new.
	compiledMethod := RTFFakeTestClass >> #assertHelper.
	tree := RTFSelfCallNode astNode: compiledMethod parseTree compiledMethod: compiledMethod.
	tree acceptVisitor: collector.
	methods := collector primitiveCalls 
]
