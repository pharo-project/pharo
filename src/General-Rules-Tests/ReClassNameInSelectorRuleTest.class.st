Class {
	#name : 'ReClassNameInSelectorRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'tests' }
ReClassNameInSelectorRuleTest >> testRule [

	self class class compile: 'useReClassNameInSelectorRuleTestToTest: something' classified: 'test-helper'.
	[ self assert: (self myCritiquesOnMethod: self class class >> #useReClassNameInSelectorRuleTestToTest:) size equals: 1 ] ensure: [
		(self class class >> #useReClassNameInSelectorRuleTestToTest:) removeFromSystem ]
]

{ #category : 'tests' }
ReClassNameInSelectorRuleTest >> testRuleNotViolated [

	self class class compile: 'useToTest: something' classified: 'test-helper'.
	[ self assertEmpty: (self myCritiquesOnMethod: self class class >> #useToTest:) ] ensure: [ (self class class >> #useToTest:) removeFromSystem ]
]
