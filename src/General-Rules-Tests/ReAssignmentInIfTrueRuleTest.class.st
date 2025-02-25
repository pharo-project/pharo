Class {
	#name : 'ReAssignmentInIfTrueRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReAssignmentInIfTrueRuleTest >> testRule [

	self generateMethod: 'testMethod |var| 1 == 1 ifTrue: [var := 1] ifFalse: [var := 2]'.
	self assert: (self myCritiquesOnMethod: self class >> #testMethod) size equals: 1
]

{ #category : 'tests' }
ReAssignmentInIfTrueRuleTest >> testRuleNotViolated [

	self generateMethod: 'testMethod |var| var := 1 == 1 ifTrue: [1] ifFalse: [2]'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #testMethod)
]
