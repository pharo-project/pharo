Class {
	#name : 'RePointRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
RePointRuleTest >> testRule [

	self generateMethod: 'methodTest ^Point x: 2 y: 3'.
	self assert: (self myCritiquesOnMethod: self class >> #methodTest) size equals: 1
]

{ #category : 'tests' }
RePointRuleTest >> testRuleNotViolated [

	self generateMethod: 'x:x y:y ^ x + y'.
	self generateMethod: 'methodTest ^self new x: 2 y: 3'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #methodTest)
]
