Class {
	#name : 'ReTemporaryVariableCapitalizationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'tests' }
ReTemporaryVariableCapitalizationRuleTest >> testRuleForArgument [

	| critiques | 
	self class compile: 'methodWith: Arg ^ Arg' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #methodWith:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #methodWith: )removeFromSystem ]
]

{ #category : 'tests' }
ReTemporaryVariableCapitalizationRuleTest >> testRuleForTemporary [

	| critiques | 
	self class compile: 'methodWithTemp | Tp | Tp := 1. ^ Tp' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #methodWithTemp.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #methodWithTemp )removeFromSystem ]
]

{ #category : 'tests' }
ReTemporaryVariableCapitalizationRuleTest >> testRuleNotViolatedForArgument [

	| critiques | 
	self class compile: 'methodWith: arg ^ arg' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #methodWith:.
	self assertEmpty: critiques ] ensure: [ (self class >> #methodWith: )removeFromSystem ]
]

{ #category : 'tests' }
ReTemporaryVariableCapitalizationRuleTest >> testRuleNotViolatedForTemporary [

	| critiques | 
	self class compile: 'methodWithTemp | tp | tp := 1. ^ tp' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #methodWithTemp.
	self assertEmpty: critiques ] ensure: [ (self class >> #methodWithTemp )removeFromSystem ]
]
