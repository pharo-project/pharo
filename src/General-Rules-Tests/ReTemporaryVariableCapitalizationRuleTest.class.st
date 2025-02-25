Class {
	#name : 'ReTemporaryVariableCapitalizationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'tests' }
ReTemporaryVariableCapitalizationRuleTest >> testRuleForArgument [

	self generateMethod: 'methodWith: Arg ^ Arg'.
	self assert: (self myCritiquesOnMethod: self class >> #methodWith:) size equals: 1
]

{ #category : 'tests' }
ReTemporaryVariableCapitalizationRuleTest >> testRuleForTemporary [

	self generateMethod: 'methodWithTemp | Tp | Tp := 1. ^ Tp'.
	self assert: (self myCritiquesOnMethod: self class >> #methodWithTemp) size equals: 1
]

{ #category : 'tests' }
ReTemporaryVariableCapitalizationRuleTest >> testRuleNotViolatedForArgument [

	self generateMethod: 'methodWith: arg ^ arg'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #methodWith:)
]

{ #category : 'tests' }
ReTemporaryVariableCapitalizationRuleTest >> testRuleNotViolatedForTemporary [

	self generateMethod: 'methodWithTemp | tp | tp := 1. ^ tp'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #methodWithTemp)
]
