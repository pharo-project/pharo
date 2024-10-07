Class {
	#name : 'ReEqualsTrueRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReEqualsTrueRuleTest >> testRule [

	| critiques | 
	
	self class compile: 'method: isFoo ^ isFoo == false ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method:) removeFromSystem ]
]

{ #category : 'tests' }
ReEqualsTrueRuleTest >> testRuleNotViolated [

	| critiques | 
	
	self class compile: 'method: isFoo ^ isFoo ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assertEmpty: critiques ] ensure: [ (self class >> #method:) removeFromSystem ]
]

{ #category : 'tests' }
ReEqualsTrueRuleTest >> testRuleWithBoolAsArgument [

	| critiques | 
	
	self class compile: 'method: isFoo ^ true == isFoo ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method:) removeFromSystem ]
]
