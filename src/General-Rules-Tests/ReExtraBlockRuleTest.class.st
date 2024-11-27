Class {
	#name : 'ReExtraBlockRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReExtraBlockRuleTest >> testRule [

	| critiques | 
	self class compile: 'method ^ [ :x | x * 23 ] value: 0 ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReExtraBlockRuleTest >> testRuleNotViolated [

	| critiques | 
	self class compile: 'method ^ 0 * 23' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
