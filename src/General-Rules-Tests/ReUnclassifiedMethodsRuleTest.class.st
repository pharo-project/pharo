Class {
	#name : 'ReUnclassifiedMethodsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'tests' }
ReUnclassifiedMethodsRuleTest >> testRule [

	self generateMethod: 'method' classified: Protocol unclassified.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReUnclassifiedMethodsRuleTest >> testRuleNotViolated [

	self generateMethod: 'method ^ 1' classified: 'accessing'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
