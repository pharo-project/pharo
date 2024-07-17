Class {
	#name : 'ReLiteralArrayCharactersRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'test-helper' }
ReLiteralArrayCharactersRuleTest >> method [ ^ #('a' 'b' 'c')
]

{ #category : 'tests' }
ReLiteralArrayCharactersRuleTest >> testRule [ 

	| critiques | 
	
	self class compile: 'method ^ #(''a'')' classified: 'test-helper'
]
