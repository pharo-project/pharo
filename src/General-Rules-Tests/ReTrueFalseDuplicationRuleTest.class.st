Class {
	#name : 'ReTrueFalseDuplicationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReTrueFalseDuplicationRuleTest >> testRule [ 

	| critiques | 
	
	self class compile: 'method self conditional ifTrue: [ self foo. self bar] ifFalse: [ self foo. self ornicar ].  ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assert: critiques size equals: 1] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReTrueFalseDuplicationRuleTest >> testRuleNotViolated [ 

	| critiques | 
	
	self class compile: 'method self foo. self conditional ifTrue: [ self bar ] ifFalse: [ self ornicar ].  ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
