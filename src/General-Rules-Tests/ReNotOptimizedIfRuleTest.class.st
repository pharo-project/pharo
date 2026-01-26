Class {
	#name : 'ReNotOptimizedIfRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReNotOptimizedIfRuleTest >> testRule [

	self generateMethod: 'method: anObject anObject ifTrue: 1 '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]

{ #category : 'tests' }
ReNotOptimizedIfRuleTest >> testRuleNotViolated [

	self generateMethod: 'method: anObject anObject ifTrue: [ ^ 1 ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method:)
]
