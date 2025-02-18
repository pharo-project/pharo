Class {
	#name : 'ReUnoptimizedAndOrRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReUnoptimizedAndOrRuleTest >> testRule [

	self generateMethod: 'method (true and: false) and: true'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReUnoptimizedAndOrRuleTest >> testRuleNotViolated [

	self generateMethod: 'method true and: [ false and: [ true ] ]'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
