Class {
	#name : 'ReSearchingLiteralRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReSearchingLiteralRuleTest >> testRule [

	self generateMethod: 'method: aValue aValue = 1 or: [ aValue = 2 or: [ aValue = 3 ] ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method:) size equals: 2
]

{ #category : 'tests' }
ReSearchingLiteralRuleTest >> testRuleNotViolated [

	self generateMethod: 'method: aValue #(1 2 3) includes: aValue '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method:)
]
