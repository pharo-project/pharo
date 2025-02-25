Class {
	#name : 'ReNotOptimizedIfNilRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReNotOptimizedIfNilRuleTest >> testRule [

	self generateMethod: 'method: anObject anObject ifNil: 1 '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]

{ #category : 'tests' }
ReNotOptimizedIfNilRuleTest >> testRuleNotViolated [

	self generateMethod: 'method: anObject anObject ifNil: [ ^ 1 ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method:)
]
