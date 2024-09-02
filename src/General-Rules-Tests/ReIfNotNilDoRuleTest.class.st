Class {
	#name : 'ReIfNotNilDoRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReIfNotNilDoRuleTest >> testRule [ 

	| critiques | 
	self class compile: 'method | arg | arg ifNotNilDo: [ arg := 1 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIfNotNilDoRuleTest >> testRuleNotViolated [ 

	| critiques | 
	self class compile: 'method | arg | arg ifNotNil: [ arg := 1 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
