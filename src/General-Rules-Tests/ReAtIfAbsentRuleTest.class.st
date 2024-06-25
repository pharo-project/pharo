Class {
	#name : 'ReAtIfAbsentRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'test-helper' }
ReAtIfAbsentRuleTest >> method [ | array | array := Dictionary new. array at: 1 ifAbsent: [ 1 ]
]

{ #category : 'tests' }
ReAtIfAbsentRuleTest >> testRule [ 

	| critiques | 
	self class compile: 'method | array | array := Array new. array at: 1 ifAbsent: [ 1 ]' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method)  ]
]
