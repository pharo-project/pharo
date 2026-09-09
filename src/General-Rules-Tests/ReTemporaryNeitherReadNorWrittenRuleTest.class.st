Class {
	#name : 'ReTemporaryNeitherReadNorWrittenRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testRule [

	self generateMethod: 'method |unused|'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReTemporaryNeitherReadNorWrittenRuleTest >> testRuleNotViolated [

	self generateMethod: 'method |used| used := 1. ^used'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
