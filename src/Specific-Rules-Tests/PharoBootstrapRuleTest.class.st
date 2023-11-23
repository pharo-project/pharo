Class {
	#name : 'PharoBootstrapRuleTest',
	#superclass : 'TestCase',
	#instVars : [
		'checkerBackup'
	],
	#category : 'Specific-Rules-Tests',
	#package : 'Specific-Rules-Tests'
}

{ #category : 'running' }
PharoBootstrapRuleTest >> setUp [
	super setUp.
	checkerBackup := PharoBootstrapRule
		classVarNamed: 'DependencyChecker'
]

{ #category : 'running' }
PharoBootstrapRuleTest >> tearDown [
	PharoBootstrapRule
		classVarNamed: 'DependencyChecker'
		put: checkerBackup.
	super tearDown
]

{ #category : 'tests' }
PharoBootstrapRuleTest >> testRuleDoesNotFailIfAppliedOnNonBootstrapPackage [
	| rule critiques |
	rule := PharoBootstrapRule new.
	rule class classVarNamed: 'DependencyChecker' put: PharoBootstrapAnalyzerStubWithNoNewDependency.

	critiques := rule check: (Package named: #P1).

	self assertEmpty: critiques
]

{ #category : 'tests' }
PharoBootstrapRuleTest >> testRuleDoesNotFailIfNoNewExternalDependency [
	| rule critiques |
	rule := PharoBootstrapRule new.
	rule class classVarNamed: 'DependencyChecker' put: PharoBootstrapAnalyzerStubWithNoNewDependency.

	critiques := rule check: (Package named: #Kernel).

	self assertEmpty: critiques
]

{ #category : 'tests' }
PharoBootstrapRuleTest >> testRuleFailsIfAnyNewExternalDependencyFound [
	| rule packageUnderTest critiques |
	rule := PharoBootstrapRule new.
	rule class
		classVarNamed: 'DependencyChecker'
		put: PharoBootstrapAnalyzerStubWithNewDependency.
	packageUnderTest := Package named: #Kernel.

	critiques := rule check: packageUnderTest.

	self assert: critiques size equals: 1.

	self
		assert: critiques anyOne sourceAnchor entity
		equals: packageUnderTest
]
