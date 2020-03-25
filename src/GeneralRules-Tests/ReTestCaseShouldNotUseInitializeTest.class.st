Class {
	#name : #ReTestCaseShouldNotUseInitializeTest,
	#superclass : #ReAbstractRuleTestCase,
	#instVars : [
		'aDummyVar'
	],
	#category : #'GeneralRules-Tests-Migrated'
}

{ #category : #running }
ReTestCaseShouldNotUseInitializeTest >> tearDown [ 

	self class removeSelector: #initialize.
	super tearDown.
]

{ #category : #tests }
ReTestCaseShouldNotUseInitializeTest >> testRule [
	| critiques |
	self class compile: 'initialize
		super initialize.
		aDummyVar := 2 ' classified: 'test-help'.

	critiques := self myCritiquesOnClass: self class.
	
	self assert: critiques size equals: 1.

]

{ #category : #tests }
ReTestCaseShouldNotUseInitializeTest >> testRuleDoesNotAppear [
	| critiques |
	self class compile: 'initialize
		super initialize. ' classified: 'test-help'.

	critiques := self myCritiquesOnClass: self class.
	
	self assert: critiques size equals: 0.

]
