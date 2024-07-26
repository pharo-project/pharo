Class {
	#name : 'ReClassNameInSelectorRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'tests' }
ReClassNameInSelectorRuleTest >> testRule [ 

	| critiques |
	self class class compile: 'useReClassNameInSelectorRuleTestToTest: something' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class class >> #useReClassNameInSelectorRuleTestToTest:.
	self assert: critiques size equals: 1 ] ensure: [ (self class class >> #useReClassNameInSelectorRuleTestToTest:) removeFromSystem]
]

{ #category : 'tests' }
ReClassNameInSelectorRuleTest >> testRuleNotViolated [

	| critiques |
	self class class compile: 'useToTest: something' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class class >> #useToTest:.
	self assertEmpty: critiques ] ensure: [ (self class class >> #useToTest:) removeFromSystem]
]
