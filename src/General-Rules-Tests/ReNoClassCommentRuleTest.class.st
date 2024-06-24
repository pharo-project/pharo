Class {
	#name : 'ReNoClassCommentRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Coding Idiom Violation',
	#package : 'General-Rules-Tests',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'running' }
ReNoClassCommentRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase new
]

{ #category : 'running' }
ReNoClassCommentRuleTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReNoClassCommentRuleTest >> testRule [ 

	| critiques classWithoutComment| 
	classWithoutComment := classFactory make: [ :builder |
		                  builder
			                  tag: 'aTag';
			                  package: 'aPackage' ].
	critiques := self myCritiquesOnClass: classWithoutComment.
	self assert: critiques size equals: 1.
]

{ #category : 'tests' }
ReNoClassCommentRuleTest >> testRuleNotViolated [

	| critiques classWithoutComment| 
	classWithoutComment := classFactory make: [ :builder |
		                  builder
			                  tag: 'aTag';
			                  package: 'aPackage' ].
	classWithoutComment comment: 'Hello I am a comment'.
	critiques := self myCritiquesOnClass: classWithoutComment.
	self assertEmpty: critiques.
]
