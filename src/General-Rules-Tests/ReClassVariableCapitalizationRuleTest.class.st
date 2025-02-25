Class {
	#name : 'ReClassVariableCapitalizationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Style',
	#package : 'General-Rules-Tests',
	#tag : 'Style'
}

{ #category : 'running' }
ReClassVariableCapitalizationRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase new
	"Put here a common initialization logic for tests"
]

{ #category : 'running' }
ReClassVariableCapitalizationRuleTest >> tearDown [ 
	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReClassVariableCapitalizationRuleTest >> testPoolVariableNameStartWithLowerCase [

	| myClass |
	myClass := classFactory make: [ :builder | builder sharedVariables: { #pool } ].
	self assert: (self myCritiquesOnClass: myClass) size equals: 1
]

{ #category : 'tests' }
ReClassVariableCapitalizationRuleTest >> testRuleNotViolated [

	| myClass |
	myClass := classFactory make: [ :builder | builder sharedVariables: { #Pool } ].
	self assertEmpty: (self myCritiquesOnClass: myClass)
]
