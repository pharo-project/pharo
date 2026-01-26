Class {
	#name : 'ReEqualsTrueRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReEqualsTrueRuleTest >> testRule [

	self generateMethod: 'method: isFoo ^ isFoo == false '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]

{ #category : 'tests' }
ReEqualsTrueRuleTest >> testRuleNotViolated [

	self generateMethod: 'method: isFoo ^ isFoo '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method:)
]

{ #category : 'tests' }
ReEqualsTrueRuleTest >> testRuleWithBoolAsArgument [

	self generateMethod: 'method: isFoo ^ true == isFoo '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]
