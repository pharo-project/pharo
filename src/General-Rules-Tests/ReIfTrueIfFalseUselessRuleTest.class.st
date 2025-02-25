Class {
	#name : 'ReIfTrueIfFalseUselessRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReIfTrueIfFalseUselessRuleTest >> testRule [

	self generateMethod: 'method 1 = 1 ifTrue: [ ^ true ] ifFalse: [ ^ false ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReIfTrueIfFalseUselessRuleTest >> testRuleNotViolated [

	self generateMethod: 'method ^ ((1 = 1) & (2 = 2))'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
