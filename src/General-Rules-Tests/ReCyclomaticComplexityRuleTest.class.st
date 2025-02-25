Class {
	#name : 'ReCyclomaticComplexityRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReCyclomaticComplexityRuleTest >> testRule [

	self generateMethod: 'method 
		1 < 2 ifTrue: [ 2 < 3 ifTrue: [ 3 < 4 ifTrue: [ 4 < 5 ifTrue: [ 3 ] ] ifFalse: [ 1 ] ] ifFalse: [ 2 < 4 ifTrue: [ 1 ] ifFalse: [ 2 ] ] ] ifFalse: [ 1 < 3 ifTrue: [ 1 ] 		ifFalse: [ 2 ] ]'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReCyclomaticComplexityRuleTest >> testRuleNotViolated [

	self generateMethod: 'method 
		1 < 2 ifTrue: [ 2 < 3 ifTrue: [ 3 < 4 ifTrue: [ 2 ] ifFalse: [ 1 ] ] ifFalse: [ 2 < 4 ifTrue: [ 1 ] ifFalse: [ 2 ] ] ] ifFalse: [ 1 < 3 ifTrue: [ 1 ] ]'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
