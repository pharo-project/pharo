Class {
	#name : 'ReDetectContainsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReDetectContainsRuleTest >> testRule1 [

	| critiques | 
	self class compile: 'method | col | col := OrderedCollection new. col do: [ :elem | elem > 1 ifTrue: [ ^ elem ] ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReDetectContainsRuleTest >> testRule2 [

	| critiques | 
	self class compile: 'method | col | col := OrderedCollection new. col do: [ :elem | elem > 1 ifFalse: [ ^ true ] ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReDetectContainsRuleTest >> testRuleNotViolatedWithContains [

	| critiques | 
	self class compile: 'method | col | col := OrderedCollection new. col contains: 1 ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReDetectContainsRuleTest >> testRuleNotViolatedWithDetect [

	| critiques | 
	self class compile: 'method | col | col := OrderedCollection new. col detect: [ :elem | elem > 1 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
