Class {
	#name : 'ReSeparateMethodBodyAndHeaderRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Formatting',
	#package : 'General-Rules-Tests',
	#tag : 'Formatting'
}

{ #category : 'tests' }
ReSeparateMethodBodyAndHeaderRuleTest >> testRuleNotViolated [

	| critiques |
	self class
		compile: 'withSeparation
	"I have a comment"

	| some |
	some := 1.
	^ some'
		classified: 'test-helper'.
	[ critiques := self myCritiquesOnMethod: self class >> #withSeparation.
	  self assertEmpty: critiques ] ensure: [ (self class >> #withSeparation) removeFromSystem ] 
]

{ #category : 'tests' }
ReSeparateMethodBodyAndHeaderRuleTest >> testRuleWithComment [

	| critiques |
	self class
		compile: 'noSeparation1
	"I have a comment"
	| some |
	some := 1.
	^ some'
		classified: 'test-helper'.
	self class
		compile: 'noSeparation2

	"I have a comment"
	^ #foobar'
		classified: 'test-helper'.

	self class
		compile: 'noSeparation3

	"I have a comment"
	
	^ #foobar'
		classified: 'test-helper'.
	critiques := [
	             critiques := self myCritiquesOnMethod:
		                          self class >> #noSeparation1.
	             critiques add: (self myCritiquesOnMethod:
		                          self class >> #noSeparation2).
	             critiques add: (self myCritiquesOnMethod:
		             self class >> #noSeparation3).
	             self assert: critiques size equals: 3 ] ensure: [
		             (self class >> #noSeparation1) removeFromSystem.
		             (self class >> #noSeparation2) removeFromSystem.
		             (self class >> #noSeparation3) removeFromSystem ]
]

{ #category : 'tests' }
ReSeparateMethodBodyAndHeaderRuleTest >> testRuleWithoutComment [

	| critiques |
	self class
		compile: 'noSeparation1
	^ 1'
		classified: 'test-helper'.

	self class
		compile: 'noSeparation2
	| some |
	some := 1.
	^ some'
		classified: 'test-helper'.

	self class
		compile: 'noSeparation3
	[ | some |
	some := 1 ].
	^ #foobar'
		classified: 'test-helper'.

	self class
		compile: 'noSeparation4
	< pragma >
	^ #foobar'
		classified: 'test-helper'.
	critiques := [
	             critiques := self myCritiquesOnMethod:
		                          self class >> #noSeparation1.
	             critiques add:
		             (self myCritiquesOnMethod: self class >> #noSeparation2).
	             critiques add:
		             (self myCritiquesOnMethod: self class >> #noSeparation3).
	             critiques add:
		             (self myCritiquesOnMethod: self class >> #noSeparation4).
	             self assert: critiques size equals: 4 ] ensure: [
		             (self class >> #noSeparation1) removeFromSystem.
		             (self class >> #noSeparation2) removeFromSystem.
		             (self class >> #noSeparation3) removeFromSystem.
						 (self class >> #noSeparation4) removeFromSystem ]
]
