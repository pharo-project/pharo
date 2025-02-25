Class {
	#name : 'ReNotEliminationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReNotEliminationRuleTest >> testRuleWithEvaluator [

	self generateMethod: 'method (1 < 3) not'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReNotEliminationRuleTest >> testRuleWithIfTrue [

	self generateMethod: 'method true not ifTrue: [ true ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReNotEliminationRuleTest >> testRuleWithSelect [

	self generateMethod: 'method: aCol aCol select: [ :elem | elem not] '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 1
]
