Class {
	#name : 'ReThemeAPIUpdateRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReThemeAPIUpdateRuleTest >> testRule [ 

	| critiques | 
	self class compile: 'method UITheme current' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReThemeAPIUpdateRuleTest >> testRuleNotViolated [

	| critiques | 
	self class compile: 'method Smalltalk ui theme' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]
