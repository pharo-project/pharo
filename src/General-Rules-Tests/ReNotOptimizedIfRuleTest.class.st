Class {
	#name : 'ReNotOptimizedIfRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReNotOptimizedIfRuleTest >> testRule [ 

	| critiques | 
	
	self class compile: 'method: anObject anObject ifTrue: 1 ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:. 
	self assert: critiques size equals: 1] ensure: [ (self class >> #method:) removeFromSystem ]
]

{ #category : 'tests' }
ReNotOptimizedIfRuleTest >> testRuleNotViolated [

	| critiques | 
	
	self class compile: 'method: anObject anObject ifTrue: [ ^ 1 ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:. 
	self assertEmpty: critiques] ensure: [ (self class >> #method:) removeFromSystem ]
]
