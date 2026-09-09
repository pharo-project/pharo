Class {
	#name : 'ReExtraBlockRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReExtraBlockRuleTest >> testRule [

	self generateMethod: 'method ^ [ :x | x * 23 ] value: 0 '.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReExtraBlockRuleTest >> testRuleNotViolated [

	self generateMethod: 'method ^ 0 * 23'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
