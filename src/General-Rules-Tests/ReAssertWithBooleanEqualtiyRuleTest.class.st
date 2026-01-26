Class {
	#name : 'ReAssertWithBooleanEqualtiyRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testAssertEqualsRule [

	self generateMethod: 'method 
			self assert: true equals: false. 
			self assert: true equals: true.'.

	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 2
]

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testAssertIdenticalRule [

	self generateMethod: 'method 
			self assert: true identicalTo: false. 
			self assert: true identicalTo: true.'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 2
]

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testDenyEqualsRule [

	self generateMethod: 'method 
			self deny: true equals: false. 
			self deny: true equals: true.'.

	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 2
]

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testDenyIdenticalRule [

	self generateMethod: 'method 
			self deny: true identicalTo: false. 
			self deny: true identicalTo: true.'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 2
]

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testRuleNotViolated [

	self generateMethod: 'method self assert: {1. 2. 3.} size equals: 3'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
