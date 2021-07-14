Class {
	#name : #RBPushDownVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
RBPushDownVariableTransformationTest >> testTransform [

	| transformation |
	transformation := (RBPushDownVariableTransformation 
							instanceVariable: 'foo1'
							class: #RBDummyLintRuleTest)
							transform.
		
	(transformation model classNamed: #RBDummyLintRuleTest) 
	subclasses do: [ :each | self assert: (each directlyDefinesInstanceVariable: 'foo1') ]
]
