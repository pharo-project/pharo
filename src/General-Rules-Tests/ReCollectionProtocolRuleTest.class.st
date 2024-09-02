Class {
	#name : 'ReCollectionProtocolRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReCollectionProtocolRuleTest >> testRule [ 

	| critiques | 
	self class compile: 'method | set col | set := #(1 2 3). col := OrderedCollection new. set do: [ :number | col add: number ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReCollectionProtocolRuleTest >> testRuleNotViolatedWithCollect [

	| critiques | 
	self class compile: 'method | set col | set := #(1 2 3). col := set collect: [ :number | number ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReCollectionProtocolRuleTest >> testRuleNotViolatedWithSelect [

	| critiques | 
	self class compile: 'method | set col | set := #(1 2 3). col := set select: [ :number | number%2 ~= 0] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
