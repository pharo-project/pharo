Class {
	#name : 'ReTrueFalseDuplicationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReTrueFalseDuplicationRuleTest >> testRule [

	self generateMethod: 'method self conditional ifTrue: [ self foo. self bar] ifFalse: [ self foo. self ornicar ]'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReTrueFalseDuplicationRuleTest >> testRuleNotViolated [

	self generateMethod: 'method self foo. self conditional ifTrue: [ self bar ] ifFalse: [ self ornicar ]'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
