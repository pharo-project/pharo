Class {
	#name : 'ReAssertWithBooleanEqualtiyRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testAssertEqualsRule [

	| critiques |
	self class
		compile:
		'method 
			self assert: true equals: false. 
			self assert: true equals: true.'
		classified: 'test-help'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 2 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testAssertIdenticalRule [

	| critiques |
	self class
		compile:
		'method 
			self assert: true identicalTo: false. 
			self assert: true identicalTo: true.'
		classified: 'test-help'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 2 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testDenyEqualsRule [

	| critiques |
	self class
		compile:
		'method 
			self deny: true equals: false. 
			self deny: true equals: true.'
		classified: 'test-help'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 2 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testDenyIdenticalRule [

	| critiques |
	self class
		compile:
		'method 
			self deny: true identicalTo: false. 
			self deny: true identicalTo: true.'
		classified: 'test-help'.
	[
	critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 2 ] ensure: [
		(self class >> #method) removeFromSystem ]
]

{ #category : 'actions' }
ReAssertWithBooleanEqualtiyRuleTest >> testRuleNotViolated [

	|critiques| 
	self class compile: 'method self assert: {1. 2. 3.} size equals: 3' classified: 'test-help'.
	[ critiques := self myCritiquesOnMethod: self class >>#method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]
