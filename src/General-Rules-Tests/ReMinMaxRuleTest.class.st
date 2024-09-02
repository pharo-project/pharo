Class {
	#name : 'ReMinMaxRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReMinMaxRuleTest >> testRuleCase1 [

	| critiques | 
	self class compile: 'method 1 < 2 ifTrue: [ 1 ] ifFalse: [ 2 ] ' classified: 'test-helper' .
	[ critiques  := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReMinMaxRuleTest >> testRuleCase2 [

	| critiques | 
	self class compile: 'method 1 > 2 ifTrue: [ 1 ] ifFalse: [ 2 ] ' classified: 'test-helper' .
	[ critiques  := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReMinMaxRuleTest >> testRuleNotViolatedWithMax [

	| critiques | 
	self class compile: 'method 1 max: b ' classified: 'test-helper' .
	[ critiques  := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReMinMaxRuleTest >> testRuleNotViolatedWithMin [

	| critiques | 
	self class compile: 'method 1 min: b ' classified: 'test-helper' .
	[ critiques  := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]
