Class {
	#name : 'ReCollectSelectNotUsedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReCollectSelectNotUsedRuleTest >> testRuleForCollect [ 

	| critiques | 
	
	self class compile: 'method #(1 2 3 4) collect: [ :number | number > 0 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReCollectSelectNotUsedRuleTest >> testRuleForselect [

	| critiques | 
	
	self class compile: 'method #(1 2 3 4) select: [ :number | number > 0 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReCollectSelectNotUsedRuleTest >> testRuleNotViolated [

	| critiques | 
	
	self class compile: 'method ^ #(1 2 3 4) select: [ :number | number > 0 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
