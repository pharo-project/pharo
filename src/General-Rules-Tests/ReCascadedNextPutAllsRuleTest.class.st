Class {
	#name : 'ReCascadedNextPutAllsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReCascadedNextPutAllsRuleTest >> testRule [ 

	| critiques | 
	self class compile: 'printOn: aStream aStream nextPutAll: ''test'', ''1'' ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self  class >> #printOn:.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #printOn:) removeFromSystem ]
]

{ #category : 'tests' }
ReCascadedNextPutAllsRuleTest >> testRuleNotViolated [ 

	| critiques | 
	self class compile: 'printOn: aStream aStream nextPutAll: ''test''; nextPutAll: ''1'' ' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self  class >> #printOn:.
	self assertEmpty: critiques ] ensure: [ (self class >> #printOn:) removeFromSystem ]
]
