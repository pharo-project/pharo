Class {
	#name : 'ReAsOrderedCollectionNotNeededRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReAsOrderedCollectionNotNeededRuleTest >> testRuleAsArray [

	self generateMethod: 'method | col | col := OrderedCollection new. col addAll: (1 to: 10) asArray'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReAsOrderedCollectionNotNeededRuleTest >> testRuleAsOrderedCollection [

	self generateMethod: 'method | col | col := OrderedCollection new. col addAll: (1 to: 10) asOrderedCollection'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReAsOrderedCollectionNotNeededRuleTest >> testRuleNotViolated [

	self generateMethod: 'method | col | col := OrderedCollection new. ^ col addAll: (1 to: 10)'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
