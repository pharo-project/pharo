Class {
	#name : 'ReEquivalentSuperclassMethodsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReEquivalentSuperclassMethodsRuleTest >> testRule [

	self generateMethod: 'myCritiques ^ self myCritiquesOnMethod: self class >> #sampleMethod'.
	self assert: (self myCritiquesOnMethod: self class >> #myCritiques) size equals: 1
]

{ #category : 'tests' }
ReEquivalentSuperclassMethodsRuleTest >> testRuleNotViolated [

	self generateMethod: 'myCritiques ^true'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #myCritiques)
]
