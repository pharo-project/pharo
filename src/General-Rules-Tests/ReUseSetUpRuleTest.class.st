Class {
	#name : 'ReUseSetUpRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReUseSetUpRuleTest >> testRule [

	self generateMethod: 'initialize' classified: 'initialization'.
	self assert: (self myCritiquesOnMethod: self class >> #initialize) size equals: 1
]

{ #category : 'tests' }
ReUseSetUpRuleTest >> testRuleNotViolated [

	self generateMethod: 'setUp'.
	self assertEmpty: (self myCritiquesOnClass: self class)
]
