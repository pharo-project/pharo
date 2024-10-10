Class {
	#name : 'ReUnoptimizedAndOrRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReUnoptimizedAndOrRuleTest >> testRule [ 

	| critiques | 
	
	self class compile: 'method (true and: false) and: true' classified: 'test-helper'. 
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReUnoptimizedAndOrRuleTest >> testRuleNotViolated [

	| critiques | 
	
	self class compile: 'method true and: [ false and: [ true ] ]' classified: 'test-helper'. 
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
