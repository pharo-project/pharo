Class {
	#name : 'ReSmalltalkGlobalsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReSmalltalkGlobalsRuleTest >> testRule1 [

	| critiques | 
	self class compile: 'method Smalltalk allClasses' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReSmalltalkGlobalsRuleTest >> testRule2 [

	| critiques | 
	self class compile: 'method Smalltalk allClassesDo: [ 1. ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReSmalltalkGlobalsRuleTest >> testRuleNotViolated [ 

	| critiques | 
	self class compile: 'method Smalltalk globals allClassesDo: [ 1. ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
