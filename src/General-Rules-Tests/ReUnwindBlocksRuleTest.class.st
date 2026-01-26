Class {
	#name : 'ReUnwindBlocksRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReUnwindBlocksRuleTest >> testRuleNotViolatedWithAssignment [

	self generateMethod: 'method | var | var := [ 1 + 1. 42  ] ensure: [ 2 ]'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReUnwindBlocksRuleTest >> testRuleNotViolatedWithReturn [

	self generateMethod: 'method ^ [ 1 + 1. 42 ] ensure: [ 2 ]'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReUnwindBlocksRuleTest >> testRuleWithAssignment [

	self generateMethod: 'method | var | [ 1 + 1. var := 42  ] ensure: [ 2 ]'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReUnwindBlocksRuleTest >> testRuleWithReturn [

	self generateMethod: 'method [ 1 + 1. ^ 42 ] ensure: [ 2 ]'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]
