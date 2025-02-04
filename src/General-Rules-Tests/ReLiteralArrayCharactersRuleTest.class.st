Class {
	#name : 'ReLiteralArrayCharactersRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReLiteralArrayCharactersRuleTest >> testRule [ 

	| critiques | 
	
	self class compile: 'method ^ #($a $b $c)' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	 self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ] 
]

{ #category : 'tests' }
ReLiteralArrayCharactersRuleTest >> testRuleNotViolated [

	| critiques | 
	
	self class compile: 'method ^ #($a $b 1)' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	 self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ] 
]
