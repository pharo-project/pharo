"
A ReCollectionProtocolRuleTest is a test class for testing the behavior of ReCollectionProtocolRule
"
Class {
	#name : 'ReCollectionProtocolRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReCollectionProtocolRuleTest >> testRule [

	self generateMethod: 'method | set col | set := #(1 2 3). col := OrderedCollection new. set do: [ :number | col add: number ] '.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReCollectionProtocolRuleTest >> testRuleNotViolatedWithCollect [

	self generateMethod: 'method | set col | set := #(1 2 3). col := set collect: [ :number | number ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReCollectionProtocolRuleTest >> testRuleNotViolatedWithSelect [

	self generateMethod: 'method | set col | set := #(1 2 3). col := set select: [ :number | number%2 ~= 0] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
