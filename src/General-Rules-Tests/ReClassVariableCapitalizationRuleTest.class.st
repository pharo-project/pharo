Class {
	#name : 'ReClassVariableCapitalizationRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
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

	| critiques myClass |
	myClass := classFactory make: [ :builder |
		           builder sharedVariables: { #pool }].
	critiques := self myCritiquesOnClass: myClass.
	self assert: critiques size equals: 1
]
