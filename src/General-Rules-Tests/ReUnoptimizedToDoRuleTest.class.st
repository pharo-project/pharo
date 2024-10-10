Class {
	#name : 'ReUnoptimizedToDoRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReUnoptimizedToDoRuleTest >> testRule [ 

	| critiques | 
	
	self class compile: 'method (1 to: 10) do: [ :each | each ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReUnoptimizedToDoRuleTest >> testRuleNotViolated [

	| critiques | 
	
	self class compile: 'method 1 to: 10 do: [ :each | each ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
