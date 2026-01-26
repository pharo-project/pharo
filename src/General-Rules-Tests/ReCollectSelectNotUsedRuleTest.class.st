Class {
	#name : 'ReCollectSelectNotUsedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReCollectSelectNotUsedRuleTest >> testRuleForCollect [

	self generateMethod: 'method #(1 2 3 4) collect: [ :number | number > 0 ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReCollectSelectNotUsedRuleTest >> testRuleForselect [

	self generateMethod: 'method #(1 2 3 4) select: [ :number | number > 0 ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReCollectSelectNotUsedRuleTest >> testRuleNotViolated [

	self generateMethod: 'method ^ #(1 2 3 4) select: [ :number | number > 0 ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
