Class {
	#name : 'RTFPrimitiveCallsCollectorTest',
	#superclass : 'TestCase',
	#category : 'RottenTestsFinder-Tests-SelfInterpreter',
	#package : 'RottenTestsFinder-Tests',
	#tag : 'SelfInterpreter'
}

{ #category : 'tests' }
RTFPrimitiveCallsCollectorTest >> testFindsAssert [
	
	| collector tree compiledMethod methods |
	collector := RTFPrimitiveCallsCollector new.
	compiledMethod := RTFFakeTestClassTest >> #assertHelper.
	tree := RTFSelfCallNode astNode: compiledMethod parseTree compiledMethod: compiledMethod.
	tree acceptVisitor: collector.
	methods := collector primitiveCalls 
]
