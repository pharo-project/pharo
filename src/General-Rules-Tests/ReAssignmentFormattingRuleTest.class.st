Class {
	#name : 'ReAssignmentFormattingRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Formatting',
	#package : 'General-Rules-Tests',
	#tag : 'Formatting'
}

{ #category : 'tests' }
ReAssignmentFormattingRuleTest >> testRuleCase1 [ 

	| critiques | 
	
	self class compile: 'method | arg | arg:=1' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	  self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReAssignmentFormattingRuleTest >> testRuleCase2 [

	| critiques | 
	
	self class compile: 'method | arg | arg:= 1' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	  self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReAssignmentFormattingRuleTest >> testRuleCase3 [

	| critiques | 
	
	self class compile: 'method | arg | arg :=1' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	  self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ]
]

{ #category : 'tests' }
ReAssignmentFormattingRuleTest >> testRuleNotViolated [ 

	| critiques | 
	
	self class compile: 'method | arg | arg := 1' classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #method.
	  self assertEmpty: critiques ] ensure: [ (self class >> #method) removeFromSystem ]
]
