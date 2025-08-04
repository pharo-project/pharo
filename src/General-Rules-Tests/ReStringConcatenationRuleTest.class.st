Class {
	#name : 'ReStringConcatenationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReStringConcatenationRuleTest >> testRule [

	self generateMethod: 'method #(1 2 3) do: [ :each | string := string, each asString].'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReStringConcatenationRuleTest >> testRuleNotViolated [

	self generateMethod: 'method String streamContents: [:s | #(1 2 3)  do: [:each | s nextPutAll: each asString]]'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
