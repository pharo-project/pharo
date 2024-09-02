Class {
	#name : 'ReLiteralArrayContainsCommaRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReLiteralArrayContainsCommaRuleTest >> testRule [ 

	| critiques | 
	self class compile: 'method #(#ok #o,k #t,t,t,t)' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReLiteralArrayContainsCommaRuleTest >> testRuleNotViolated [

	| critiques | 
	self class compile: 'method #(#ok #R2D2 #t_t_t)' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]
