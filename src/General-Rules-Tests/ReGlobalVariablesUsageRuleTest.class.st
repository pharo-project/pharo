"
A RBGlobalVariablesUsageTest is a test class for testing the behavior of RBGlobalVariablesUsage
"
Class {
	#name : 'ReGlobalVariablesUsageRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'sample' }
ReGlobalVariablesUsageRuleTest >> sampleMethod [
	"Global variable are stored in Smalltalk globals, and one should avoid them when possible.
	Examples are Display and Sensor"

	"Note: we compile this method in #setUp and #tearDown to add
	a global variable that would otherwise be undeclared"
	Smalltalk value
]

{ #category : 'running' }
ReGlobalVariablesUsageRuleTest >> setUp [
	super setUp.

	"Register a new unknown global"
	Smalltalk globals
		at: #MyGlobal
		put: (self class >> #sampleMethod) sourceCode.

	self class compiler permitFaulty: true; install: 'sampleMethod
	sampleMethod
		Smalltalk value.
		MyGlobal value'
]

{ #category : 'running' }
ReGlobalVariablesUsageRuleTest >> tearDown [
	self class compile: (Smalltalk globals at: #MyGlobal).
	Smalltalk globals removeKey: #MyGlobal.

	super tearDown
]

{ #category : 'tests' }
ReGlobalVariablesUsageRuleTest >> testRule [
	| critiques |
 	critiques := self myCritiques.

	"Sensor and Display are both known globals"
 	self assert: critiques size equals: 1.
 	self assert: (self sourceAtChritique: critiques first) equals: 'MyGlobal'
]
