Class {
	#name : 'ReIsNotNilAndConditionalRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReIsNotNilAndConditionalRuleTest >> testRuleNotViolatedIfNil [

	| critiques | 
	self class compile: 'method | arg | ^ arg ifNil: [ arg := 1 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNotNilAndConditionalRuleTest >> testRuleNotViolatedIfNotNil [

	| critiques | 
	self class compile: 'method | arg | ^ arg ifNotNil: [ arg := 1 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNotNilAndConditionalRuleTest >> testRuleNotViolatedIfNotNilIfNil [

	| critiques | 
	self class compile: 'method | arg | ^ arg ifNotNil: [ arg := 2 ] ifNil: [ arg := 1 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNotNilAndConditionalRuleTest >> testRuleWithIsNotNilIfFalse [

	| critiques | 
	self class compile: 'method | arg | ^ arg isNotNil ifFalse: [ arg := 1 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNotNilAndConditionalRuleTest >> testRuleWithIsNotNilIfTrueIfFalse [

	| critiques | 
	self class compile: 'method | arg | ^ arg isNotNil ifTrue: [ arg := 1 ] ifFalse: [ arg := 2 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNotNilAndConditionalRuleTest >> testRuleWithNotNilIfTrue [

	| critiques | 
	self class compile: 'method | arg | ^ arg notNil ifTrue: [ arg := 1 ]' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReIsNotNilAndConditionalRuleTest >> testRuleWithNotNilIfTrueIfFalse [

	| critiques | 
	self class compile: 'method | arg | ^ arg notNil ifTrue: [ arg := 1 ] ifFalse: [ arg := 2 ] ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]
