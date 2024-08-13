"
A RBMultiplePeriodsTerminatingStatementRuleTest is a test class for testing the behavior of RBMultiplePeriodsTerminatingStatementRule
"
Class {
	#name : 'ReMultiplePeriodsTerminatingStatementRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'utilities' }
ReMultiplePeriodsTerminatingStatementRuleTest >> myCritiques [

	| critiques |
	critiques := super myCritiques.
	self subjectUnderTest  new
		check: (self class >> #sampleMethod2) forCritiquesDo:[:critique | critiques add: critique].
	^ critiques
]

{ #category : 'sample' }
ReMultiplePeriodsTerminatingStatementRuleTest >> sampleMethod [
	"I have two periods between statements"
	2+3.  .
	{1.2..3}.
	99."I put a dot after my comment".
	100.
.
]

{ #category : 'sample' }
ReMultiplePeriodsTerminatingStatementRuleTest >> sampleMethod2 [
	"I have two statements and two dots"

	self asString."foobar". self asString
]

{ #category : 'tests' }
ReMultiplePeriodsTerminatingStatementRuleTest >> testRule [
	| critiques |
	critiques := self myCritiques .

	self assert: critiques size equals: 4.
	self assert: (critiques allButLast allSatisfy: [ :cr | cr sourceAnchor sourceEntity selector = #sampleMethod ]).
	self assert: (self sourceAtChritique: critiques first) equals: '.  .'.
	self assert: (self sourceAtChritique: critiques second) equals: '."I put a dot after my comment".'.
	self assert: (self sourceAtChritique: critiques third) equals: '.',(Character cr asString),'.'.
	
	self assert: critiques fourth sourceAnchor sourceEntity selector equals: #sampleMethod2.
	self assert: (self sourceAtChritique: critiques fourth) equals: '."foobar".'
]
