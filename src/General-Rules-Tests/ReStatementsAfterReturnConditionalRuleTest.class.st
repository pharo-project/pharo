Class {
	#name : 'ReStatementsAfterReturnConditionalRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'tests' }
ReStatementsAfterReturnConditionalRuleTest >> testRule [

	self generateMethod: 'method 1 = 1 ifTrue: [ ^ 1 ] ifFalse: [ ^ 2 ]. 2'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReStatementsAfterReturnConditionalRuleTest >> testRuleNotViolated [

	self generateMethod: 'method 1 = 1 ifTrue: [ ^ 1 ] ifFalse: [ ^ 2 ].'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReStatementsAfterReturnConditionalRuleTest >> testRuleNotViolatedWithNotReturnInBranch [

	self generateMethod: 'method 1 = 1 ifTrue: [  ] ifFalse: [ ^ 2 ]. 2'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
