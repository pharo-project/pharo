Class {
	#name : 'ReUnclassifiedMethodsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'tests' }
ReUnclassifiedMethodsRuleTest >> testRule [

	| critiques | 
	self class compile: 'method'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1  ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReUnclassifiedMethodsRuleTest >> testRuleNotViolated [

	| critiques | 
	self class compile: 'method ^ 1' classified: 'test-helper' .
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
