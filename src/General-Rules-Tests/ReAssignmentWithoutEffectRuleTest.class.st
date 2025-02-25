Class {
	#name : 'ReAssignmentWithoutEffectRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReAssignmentWithoutEffectRuleTest >> testRule [

	self generateMethod: 'method |x| x:=x'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReAssignmentWithoutEffectRuleTest >> testRuleNotViolated [

	self generateMethod: 'method |x| x:=1'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
