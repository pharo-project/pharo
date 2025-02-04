Class {
	#name : 'ReSearchingLiteralRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReSearchingLiteralRuleTest >> testRule [

	| critiques | 
	
	self class compile: 'method: aValue aValue = 1 or: [ aValue = 2 or: [ aValue = 3 ] ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 2 ] ensure: [ (self class >> #method:) removeFromSystem ]
]

{ #category : 'tests' }
ReSearchingLiteralRuleTest >> testRuleNotViolated [

	| critiques | 
	
	self class compile: 'method: aValue #(1 2 3) includes: aValue ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assertEmpty: critiques ] ensure: [ (self class >> #method:) removeFromSystem ]
]
