Class {
	#name : 'ReTestCaseShouldNotUseInitializeRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'aDummyVar'
	],
	#category : 'General-Rules-Tests-Clean Code',
	#package : 'General-Rules-Tests',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReTestCaseShouldNotUseInitializeRuleTest >> aDummyVar [
	"Only necessary that ivar does not appear unused"

	^ aDummyVar
]

{ #category : 'running' }
ReTestCaseShouldNotUseInitializeRuleTest >> tearDown [

	self class removeSelector: #initialize.
	super tearDown
]

{ #category : 'tests' }
ReTestCaseShouldNotUseInitializeRuleTest >> testRule [
	| critiques |
	self class compile: 'initialize
		super initialize.
		aDummyVar := 2 ' classified: 'test-help'.

	critiques := self myCritiquesOnClass: self class.

	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReTestCaseShouldNotUseInitializeRuleTest >> testRuleDoesNotAppear [
	| critiques |
	self class compile: 'initialize
		super initialize. ' classified: 'test-help'.

	critiques := self myCritiquesOnClass: self class.

	self assert: critiques size equals: 0
]
