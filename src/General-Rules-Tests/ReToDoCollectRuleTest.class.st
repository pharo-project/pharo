Class {
	#name : 'ReToDoCollectRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'tests' }
ReToDoCollectRuleTest >> testRule [ 

	| critiques | 
	
	self class compile: 'method | aCol | aCol := Array new: 10. 1 to: aCol size do: [ :i | aCol at: i put: 1  ]' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReToDoCollectRuleTest >> testRuleNotViolated [ 

	| critiques | 
	
	self class compile: 'method | aCol | aCol := Array new: 10. aCol collect: [ :i | ]' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
