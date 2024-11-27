Class {
	#name : 'ReUnwindBlocksRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReUnwindBlocksRuleTest >> testRuleNotViolatedWithAssignment [

	| critiques | 
	
	self class compile: 'method | var | var := [ 1 + 1. 42  ] ensure: [ 2 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReUnwindBlocksRuleTest >> testRuleNotViolatedWithReturn [

	| critiques | 
	
	self class compile: 'method ^ [ 1 + 1. 42 ] ensure: [ 2 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReUnwindBlocksRuleTest >> testRuleWithAssignment [

	| critiques | 
	
	self class compile: 'method | var | [ 1 + 1. var := 42  ] ensure: [ 2 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReUnwindBlocksRuleTest >> testRuleWithReturn [

	| critiques | 
	
	self class compile: 'method [ 1 + 1. ^ 42 ] ensure: [ 2 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]
