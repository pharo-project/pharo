Class {
	#name : 'ReLiteralArrayCharactersRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReLiteralArrayCharactersRuleTest >> testRule [

	self generateMethod: 'method ^ #($a $b $c)'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReLiteralArrayCharactersRuleTest >> testRuleNotViolated [

	self generateMethod: 'method ^ #($a $b 1)'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
