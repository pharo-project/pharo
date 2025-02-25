Class {
	#name : 'ReNilBranchRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReNilBranchRuleTest >> testRuleNotViolated [

	self generateMethod: 'method 1 < 0 ifTrue: [ 1 ]'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReNilBranchRuleTest >> testRuleWithIfFalse [

	self generateMethod: 'method 1 < 0 ifFalse: [ 1 ] ifTrue: [ nil ]'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReNilBranchRuleTest >> testRuleWithIfTrue [

	self generateMethod: 'method 1 < 0 ifTrue: [ 1 ] ifFalse: [ nil ]'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]
