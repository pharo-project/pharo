Class {
	#name : 'ReNotEliminationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReNotEliminationRuleTest >> testRuleWithEvaluator [

	| critiques | 
	
	self class compile: 'method (1 < 3) not' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReNotEliminationRuleTest >> testRuleWithIfTrue [

	| critiques | 
	
	self class compile: 'method true not ifTrue: [ true ] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReNotEliminationRuleTest >> testRuleWithSelect [

	| critiques | 
	
	self class compile: 'method: aCol aCol select: [ :elem | elem not] ' classified: 'test-helper'.
	
	[ critiques := self myCritiquesOnMethod: self class >> #method:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method:) removeFromSystem ]
]
