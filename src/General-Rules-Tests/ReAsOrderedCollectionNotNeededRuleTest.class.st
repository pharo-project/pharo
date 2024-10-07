Class {
	#name : 'ReAsOrderedCollectionNotNeededRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReAsOrderedCollectionNotNeededRuleTest >> testRuleAsArray [

	| critiques | 
	self class compile: 'method | col | col := OrderedCollection new. col addAll: (1 to: 10) asArray' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReAsOrderedCollectionNotNeededRuleTest >> testRuleAsOrderedCollection [

	| critiques | 
	self class compile: 'method | col | col := OrderedCollection new. col addAll: (1 to: 10) asOrderedCollection' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReAsOrderedCollectionNotNeededRuleTest >> testRuleNotViolated [

	| critiques | 
	self class compile: 'method | col | col := OrderedCollection new. ^ col addAll: (1 to: 10)' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
