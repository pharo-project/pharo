Class {
	#name : 'ReYourselfNotUsedRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReYourselfNotUsedRuleTest >> testRule [

	self generateMethod: 'methodTest self class yourself'.
	self assert: (self myCritiquesOnMethod: self class >> #methodTest) size equals: 1
]

{ #category : 'tests' }
ReYourselfNotUsedRuleTest >> testRuleNotViolated [

	self generateMethod: 'methodTest ^ self yourself'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #methodTest)
]
